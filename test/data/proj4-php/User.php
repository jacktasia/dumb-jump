<?php
/**
 * dumb-jump unit test class
 *
 * @property       string $firstname
 * @property-read  string $lastname
 * @property       int    $age
 * @property-write User   $sibling
 *
 * @method void calculateAge($date)
 */
class User
{
    /**
     * @var string
     */
    public $username;

    public static function create($foo)
    {
        $create = true;//variable with same name as method
    }

    protected function saveInternal()
    {
        self::create('foo')->getSelf()->calculateAge();
    }

    public function getSelf()
    {
        $getSelf = false;//dummy variable
        return $this;
    }
}
?>
