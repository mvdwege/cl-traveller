(in-package :traveller)
;;;; Set up weapon data for the weapons module

(defvar *weapon-types-table*
  '((:type guns
     :subtypes (gun gun gatling cannon cannon autocannon)
     :descriptors (anti-flyer anti-tank assault fusion gauss plasma))
    (:type rifles
     :subtypes (carbine carbine carbine rifle rifle rifle rifle rifle carbine carbine carbine)
     :descriptors (nil accelerator assault battle combat (poison-dart . dart) gauss hunting laser splat survival))
    (:type pistols
     :subtypes (revolver pistol pistol pistol pistol revolver)
     :descriptors (nil accelerator laser machine nil nil))
    (:type shotguns
     ; one-item tables return the same value every time when using
     ; roll-on.
     :subtypes (shotgun)
     :descriptors (nil assault hunting hunting assault nil))
    (:type machineguns
     :subtypes (machinegun)
     :descriptors (nil anti-flyer assault sub sub nil))
    (:type launchers
     :subtypes (launcher launcher launcher multi-launcher multi-launcher multi-launcher)
     :descriptors (at-missile af-missile grenade ram-grenade missile rocket))
    (:type projectors
     :subtypes (projector)
     :descriptors (poison-gas (emp . rad) fire flash freeze (grav . laser) mag psi-amp (acid . shock) sonic stench))
    (:type designators
     :subtypes (designator)
     :descriptors (poison-gas (emp . rad) fire flash freeze (grav . laser) mag psi-amp (acid . shock) sonic stench))))

(defvar *weapon-subtypes-table*
  '(gun (:category artillery :code "G" :tl 6 :range 4 :mass 9 :burden 1 :h1 any :d1 2 :cr 5000)
    gatling (:category artillery :code "Ga" :tl 7 :range 4 :mass 40 :burden 2 :h1 any :d1 3 :cr 8000)
    cannon (:category artillery :code "C" :tl 6 :range 6 :mass 200 :burden 4 :h1 any :d1 4 :cr 10000)
    autocannon (:category artillery :code "aC" :tl 8 :range 6 :mass 300 :burden 4 :h1 any :d1 5 :cr 30000)
    rifle (:category long-guns :code "R" :tl 5 :range 5 :mass 4 :burden 0 :h1 bullet :d1 2 :cr 500)
    carbine (:category long-guns :code "C" :tl 5 :range 4 :mass 3 :burden -1 :h1 bullet :d1 1 :cr 400)
    pistol (:category handguns :code "P" :tl 5 :range 2 :mass 1.1 :burden 0 :h1 bullet :d1 1 :cr 150)
    revolver (:category handguns :code "R" :tl 4 :range 2 :mass 1.25 :burden 0 :h1 bullet :d1 1 :cr 100)
    shotgun (:category shotguns :code "S" :tl 4 :range 2 :mass 4 :burden 0 :h1 frag :d1 2 :cr 300)
    machinegun (:category machineguns :code "Mg" :tl 6 :range 5 :mass 8 :burden 1 :h1 bullet :d1 4 :cr 3000)
    projector (:category projectors :code "Pj" :tl 9 :range 0 :mass 1 :burden 0 :h1 any :d1 1 :cr 300)
    designator (:category designators :code "D" :tl 7 :range 5 :mass 10 :burden 1 :h1 any :d1 1 :cr 2000)
    launcher (:category launchers :code "L" :tl 6 :range 3 :mass 10 :burden 1 :h1 any :d1 1 :cr 1000)
    multi-launcher (:name "Multi-Launcher" :category launchers :code "mL" :range 5 :mass 8 :burden 1 :h1 any :d1 1 :cr 3000)))

(defvar *weapon-descriptors-table*
  '(anti-flyer (:code "aF" :name "Anti-Flyer" :category artillery :tl 4 :range 6 :mass 6 :h2 frag :d2 1 :h3 blast :d3 3 :cr 3)
    anti-tank (:code "aT" :name "Anti-Tank" :category artillery :range 5 :mass 8 :h2 pen :d2 3 :h3 blast :d3 3 :cr 2)
    assault (:code "A" :category artillery :tl 2 :range 4 :mass 0.8 :h2 bang :d2 1 :h3 blast :d3 2 :cr 1.5)
    fusion (:code "F" :category artillery :tl 7 :range 4 :mass 2.3 :h2 pen :d2 4 :h3 burn :d3 4 :cr 6)
    gauss (:code "G" :tl 7 :range 4 :mass 0.9 :h2 bullet :d2 3 :cr 2)
    plasma (:code "P" :tl 5 :range 4 :mass 2.5 :h2 pen :d2 3 :h3 burn :d3 3 :cr 2)
    accelerator (:code "Ac" :tl 4 :mass 0.6 :h2 bullet :d2 2 :cr 3)
    battle (:code "B" :tl 1 :range 5 :mass 1  :burden 1 :h2 bullet :d2 1 :cr 0.8)
    combat (:code "C" :tl 2 :range 3 :mass 0.9 :h2 bullet :d2 1 :cr 1.5)
    dart (:code "D" :tl 1 :range 4 :mass 0.6 :h2 tranq :d2 (1 2 3) :cr 0.9)
    poison-dart (:code "P" :tl 1 :range 4 :mass 1 :h2 poison :d2 (1 2 3) :cr 0.9)
    hunting (:code "H" :range 3 :mass 0.9 :burden -1 :h2 bullet :d2 1 :cr 1.2)
    laser (:code "L" :tl 4 :mass 1.2 :h2 burn :d2 (1 2 3) :h3 pen :d3 (1 2 3) :cr 6)
    splat (:code "Sp" :tl 2 :range 4 :mass 1.3 :burden 1 :h2 bullet :d2 1 :cr 2.4)
    survival (:code "S" :range 2 :mass 0.5 :h2 bullet :d2 1 :cr 1.2)
    machine (:code "M")
    sub (:code "S")
    acid (:code "A")
    fire (:code "H")
    poison-gas (:code "P")
    stench (:code "S")
    emp (:code "Emp")
    flash (:code "F")
    freeze (:code "C")
    grav (:code ""G)
    mag (:code "M")
    psi-amp (:code "Psi")
    rad (:code "R")
    shock (:code "Sh")
    sonic (:code "S")
    af-missile (:code "aF")
    at-missile (:code "aT")
    grenade (:code "Gr")
    missile (:code "M")
    ram-grenade (:code "RAM")
    rocket (:code "R")))
