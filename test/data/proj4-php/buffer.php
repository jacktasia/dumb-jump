<?php
/**
 * Buffer used for testing
 */
//static method
User::create();

//normal method
$user->getSelf();

//normal method, nested
$user->getSelf()->saveInternal();

?>
