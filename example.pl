# Run as:
#   perl -I<perldir> [-w] example.pl
# Note difference in behavior depending on whether -w flag is used.

use Class::Generate qw(&class &subclass);

class Person => {
    last_name  => { type => '$', required => 1, readonly => 1 },
    first_name => { type => '$', required => 1, readonly => 1 },
    mi	       => { type => '$', assert => '! defined $mi || $mi =~ /^[A-Z]$/' },
    kids       => '@Person',
    age	       => { type => '$', assert => '! defined $age || $age >= 0 && $age <= 200' },
    ssn	       => { type => '$', assert => '! defined $ssn || $ssn =~ /^\d{3}-\d{2}-\d{4}$/' },

    '&name'    => 'return defined $mi ? "$first_name $mi. $last_name" : "$first_name $last_name";',
    '&clone' => 'my $p = Person->new(last_name => $last_name,
				     first_name => $first_name,
				     $ssn => $_[0]);
		 $p->mi($mi)   if defined $mi;
		 $p->age($age) if defined $age;
		 for ( @kids ) { $p->add_kids($_->copy) }
		 return $p;'

};

subclass Employee => {
    company => { type => '$', required => 1 },
    salary  => { type => '$', required => 1, assert => '$salary =~ /^\d+(\.\d+)?$/' },
    boss    => 'Manager'
}, parent => 'Person';

subclass Manager => {
    slaves => '@Employee'
}, parent => 'Employee';

$p = new Person last_name => 'Blow', first_name => 'Joe',
		age => 34, ssn => '555-66-7788';
$q = new Person last_name => 'Schmoe', first_name => 'Joe',
		age => 18, ssn => '555-66-7789';
print $p->name, "\n";

$m = new Manager last_name => 'Boss', first_name => 'Big',
		 company => 'NetCorp Inc.',  salary  => 25000,
		 age => 55, ssn => '555-77-2222';
$e = new Employee company => 'NetCorp Inc.',
		  salary => 100000.00,
		  last_name => 'Smith', first_name => 'Sally', mi => 'S',
	  	  age => 23, ssn => '555-88-9999',
		  boss => $m;
print $e->name, " is managed by ",  $e->boss->name, "\n";
$m->add_slaves($e);
print $m->name, " manages ", join(', ', map($_->name, $m->slaves)), "\n";

eval { $m->add_slaves($q) };	# Will fail if $WARNING is true:
print $@ if $@;			# $q is not an employee.

$r = $p->clone('555-99-8765');
eval { $r->ssn(42) };		# Will fail if $WARNING is true:
print $@ if $@;			# invalid SSN.
