<?php
/**
 * PHP 8.x features test file for dumb-jump
 * Tests PHP 7.4+ and PHP 8.x syntax patterns
 *
 * Each definition is followed by usage examples for testing dumb-jump navigation.
 * Place cursor on a symbol usage and run M-x dumb-jump-go to jump to its definition.
 */

namespace App\Models;

use DateTime;
use App\Interfaces\Serializable;
use App\Traits\Loggable as LoggableTrait;

// --- Global constant ---
const APP_VERSION = '1.0.0';
// Usage: Jump from APP_VERSION below to definition above
$version = APP_VERSION;

// --- define() ---
define('MAX_ITEMS', 100);
// Usage: Jump from MAX_ITEMS below to definition above
$limit = MAX_ITEMS;

// =============================================================================
// ENUMS
// =============================================================================

/**
 * PHP 8.1+ Enum - Basic
 */
enum Status {
    case Pending;
    case Active;
    case Archived;
}
// Usage: Jump from Status/Pending/Active below to definitions above
$currentStatus = Status::Pending;
$isActive = ($currentStatus === Status::Active);
$isArchived = Status::Archived;

/**
 * PHP 8.1+ Enum - Backed with int
 */
enum HttpStatus: int {
    case Ok = 200;
    case NotFound = 404;
    case ServerError = 500;
}
// Usage: Jump from HttpStatus/Ok/NotFound below to definitions above
$successCode = HttpStatus::Ok;
$errorCode = HttpStatus::NotFound;
$code = HttpStatus::ServerError->value;

/**
 * PHP 8.1+ Enum - Backed with string, implements interface
 */
enum Environment: string implements Serializable {
    case Development = 'dev';
    case Production = 'prod';
    case Testing = 'test';

    public function label(): string {
        return match($this) {
            self::Development => 'Development',
            self::Production => 'Production',
            self::Testing => 'Testing',
        };
    }
}
// Usage: Jump from Environment/Development/Production below to definitions above
$env = Environment::Production;
$isDev = Environment::Development;
$label = Environment::Testing->label();

// =============================================================================
// CLASSES
// =============================================================================

/**
 * Abstract base class
 */
abstract class BaseModel {
    abstract public function getId(): int;
    abstract protected function validate(): bool;
}
// Usage: Jump from BaseModel below to definition above
class ConcreteModel extends BaseModel {
    public function getId(): int { return 1; }
    protected function validate(): bool { return true; }
}
$model = new ConcreteModel();

/**
 * Final class - cannot be extended
 */
final class Singleton {
    private static ?self $instance = null;

    private function __construct() {}

    public static function getInstance(): self {
        if (self::$instance === null) {
            self::$instance = new self();
        }
        return self::$instance;
    }
}
// Usage: Jump from Singleton below to definition above
$singleton = Singleton::getInstance();

/**
 * Readonly class (PHP 8.2+)
 */
readonly class UserDTO {
    public function __construct(
        public int $id,
        public string $name,
        public string $email,
    ) {}
}
// Usage: Jump from UserDTO below to definition above
$userDto = new UserDTO(1, 'John', 'john@example.com');

/**
 * Final readonly class (PHP 8.2+)
 */
final readonly class ImmutableConfig {
    public function __construct(
        public string $appName,
        public string $version,
    ) {}
}
// Usage: Jump from ImmutableConfig below to definition above
$immutableConfig = new ImmutableConfig('MyApp', '1.0.0');

/**
 * Readonly final class (PHP 8.2+) - alternative modifier order
 */
readonly final class AnotherDTO {
    public function __construct(
        public string $value,
    ) {}
}
// Usage: Jump from AnotherDTO below to definition above
$anotherDto = new AnotherDTO('test');

// =============================================================================
// INTERFACE & TRAIT
// =============================================================================

/**
 * Interface with constants
 */
interface Configurable {
    public const CONFIG_VERSION = 1;
    public const DEFAULT_TIMEOUT = 30;
}
// Usage: Jump from Configurable/CONFIG_VERSION below to definitions above
class ConfigurableImpl implements Configurable {}
$ver = Configurable::CONFIG_VERSION;
$timeout = Configurable::DEFAULT_TIMEOUT;

/**
 * Trait definition
 */
trait Timestampable {
    public ?DateTime $createdAt = null;
    public ?DateTime $updatedAt = null;

    public function touch(): void {
        $this->updatedAt = new DateTime();
    }
}
// Usage: Jump from Timestampable below to definition above
class TimestampedEntity {
    use Timestampable;
}
$entity = new TimestampedEntity();
$entity->touch();

// =============================================================================
// CLASS WITH PROPERTIES AND CONSTANTS
// =============================================================================

/**
 * Regular class with modern PHP features
 */
class User extends BaseModel implements Configurable {
    use Timestampable;

    // Class constants with visibility
    public const TABLE_NAME = 'users';
    protected const HIDDEN_FIELDS = ['password'];
    private const INTERNAL_VERSION = 2;
    final public const MAX_LOGIN_ATTEMPTS = 5;

    // Typed properties (PHP 7.4+)
    public int $id;
    public string $name;
    private ?string $email = null;
    protected array $roles = [];

    // Readonly property (PHP 8.1+)
    public readonly string $uuid;

    // Static property (typed)
    private static int $instanceCount = 0;
    public static ?self $lastCreated = null;

    // Static property (untyped)
    static $untypedCounter;

    // Union type property (PHP 8.0+)
    public string|int $identifier;

    // Nullable typed property
    public ?DateTime $lastLogin = null;

    public function __construct(int $id, string $name) {
        $this->id = $id;
        $this->name = $name;
        $this->uuid = uniqid('user_', true);
        self::$instanceCount++;
        self::$lastCreated = $this;
    }

    public function getId(): int {
        return $this->id;
    }

    protected function validate(): bool {
        return !empty($this->name);
    }

    public static function getInstanceCount(): int {
        return self::$instanceCount;
    }
}
// Usage: Jump from User/TABLE_NAME/MAX_LOGIN_ATTEMPTS below to definitions above
$user = new User(1, 'Alice');
echo User::TABLE_NAME;
echo User::MAX_LOGIN_ATTEMPTS;
echo $user->id;
echo $user->name;
echo $user->uuid;
echo User::$lastCreated;

// =============================================================================
// FUNCTIONS
// =============================================================================

// Function using arrow function (PHP 7.4+)
function processItems(array $items): array {
    $doubled = array_map(fn($x) => $x * 2, $items);
    return $doubled;
}
// Usage: Jump from processItems below to definition above
$result = processItems([1, 2, 3]);

// Switch statement (should NOT match enum case pattern)
function handleStatus(int $code): string {
    switch ($code) {
        case 200:
            return 'OK';
        case 404:
            return 'Not Found';
        default:
            return 'Unknown';
    }
}
// Usage: Jump from handleStatus below to definition above
$statusText = handleStatus(200);

// =============================================================================
// NEGATIVE TEST CASES
// These should NOT be matched by patterns (comments, variable assignments)
// =============================================================================

// enum FakeEnum { case Fake; }
// abstract class FakeAbstract {}
// final class FakeFinal {}
// readonly class FakeReadonly {}
// const FAKE_CONST = 123;
// define('FAKE_DEFINE', 456);

// Variable assignments (should NOT match constant patterns)
$enum = 'not an enum';
$const = 'not a constant';
$define = 'not define()';

// Function use and const use (should NOT match class use pattern)
use function App\Helpers\helperFunction;
use const App\Constants\SOME_CONSTANT;
