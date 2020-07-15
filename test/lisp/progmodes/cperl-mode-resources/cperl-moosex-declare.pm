# This test case shows the SYNOPSIS of MooseX::Declare.  That module
# is deprecated now (2020), but it is still an interesting test case.
use MooseX::Declare;

class BankAccount {
    has 'balance' => ( isa => 'Num', is => 'rw', default => 0 );

    method deposit (Num $amount) {
        $self->balance( $self->balance + $amount );
    }

    method withdraw (Num $amount) {
        my $current_balance = $self->balance();
        ( $current_balance >= $amount )
            || confess "Account overdrawn";
        $self->balance( $current_balance - $amount );
    }
}

class CheckingAccount extends BankAccount {
    has 'overdraft_account' => ( isa => 'BankAccount', is => 'rw' );

    before withdraw (Num $amount) {
        my $overdraft_amount = $amount - $self->balance();
        if ( $self->overdraft_account && $overdraft_amount > 0 ) {
            $self->overdraft_account->withdraw($overdraft_amount);
            $self->deposit($overdraft_amount);
        }
    }
}
