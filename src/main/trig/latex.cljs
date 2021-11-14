(ns trig.latex
  (:require ["katex" :as katex]))

(defn tex [text]
  [:span
   {:ref
    (fn [el]
      (when el
        (try
          (katex/render text el (clj->js {:throwOnError false}))
          (catch :default e
            (js/console.warn "Unexpected KaTeX error" e)
            (aset el "innerHTML" text)))))}])

(def letters
  {:A "M208 74Q208 50 254 46Q272 46 272 35Q272 34 270 22Q267 8 264 4T251 0Q249 0 239 0T205 1T141 2Q70 2 50 0H42Q35 7 35 11Q37 38 48 46H62Q132 49 164 96Q170 102 345 401T523 704Q530 716 547 716H555H572Q578 707 578 706L606 383Q634 60 636 57Q641 46 701 46Q726 46 726 36Q726 34 723 22Q720 7 718 4T704 0Q701 0 690 0T651 1T578 2Q484 2 455 0H443Q437 6 437 9T439 27Q443 40 445 43L449 46H469Q523 49 533 63L521 213H283L249 155Q208 86 208 74ZM516 260Q516 271 504 416T490 562L463 519Q447 492 400 412L310 260L413 259Q516 259 516 260Z"
   :a "M115 473T93 473 56 458 41 419Q41 384 75 357T196 330Q269 330 314 362T374 438Q379 452 379 469T380 584V654Q380 702 385 720T406 738Q421 738 426 722T431 669V633H471V672Q470 712 468 719 459 749 433 766T378 784 331 766 307 724V720L305 723Q303 726 300 729T292 738 280 749 265 761 247 772 225 780 199 786 168 789Q108 789 60 758T12 671Q12 650 19 631T46 590 94 553 172 525 282 510H296V488Q296 454 290 438 268 367 193 367 175 367 159 368T134 372 126 375Q148 390 148 419 148 444 132 458ZM104 672Q104 703 128 727T187 752Q225 752 254 729T293 669Q295 662 296 603 296 545 295 545 287 545 274 546T229 555 171 575 125 612 104 672Z"
   :B "M231 637Q204 637 199 638T194 649Q194 676 205 682Q206 683 335 683Q594 683 608 681Q671 671 713 636T756 544Q756 480 698 429T565 360L555 357Q619 348 660 311T702 219Q702 146 630 78T453 1Q446 0 242 0Q42 0 39 2Q35 5 35 10Q35 17 37 24Q42 43 47 45Q51 46 62 46H68Q95 46 128 49Q142 52 147 61Q150 65 219 339T288 628Q288 635 231 637ZM649 544Q649 574 634 600T585 634Q578 636 493 637Q473 637 451 637T416 636H403Q388 635 384 626Q382 622 352 506Q352 503 351 500L320 374H401Q482 374 494 376Q554 386 601 434T649 544ZM595 229Q595 273 572 302T512 336Q506 337 429 337Q311 337 310 336Q310 334 293 263T258 122L240 52Q240 48 252 48T333 46Q422 46 429 47Q491 54 543 105T595 229Z"
   :C "M50 252Q50 367 117 473T286 641T490 704Q580 704 633 653Q642 643 648 636T656 626L657 623Q660 623 684 649Q691 655 699 663T715 679T725 690L740 705H746Q760 705 760 698Q760 694 728 561Q692 422 692 421Q690 416 687 415T669 413H653Q647 419 647 422Q647 423 648 429T650 449T651 481Q651 552 619 605T510 659Q484 659 454 652T382 628T299 572T226 479Q194 422 175 346T156 222Q156 108 232 58Q280 24 350 24Q441 24 512 92T606 240Q610 253 612 255T628 257Q648 257 648 248Q648 243 647 239Q618 132 523 55T319 -22Q206 -22 128 53T50 252Z"
   :D "M287 628Q287 635 230 637Q207 637 200 638T193 647Q193 655 197 667T204 682Q206 683 403 683Q570 682 590 682T630 676Q702 659 752 597T803 431Q803 275 696 151T444 3L430 1L236 0H125H72Q48 0 41 2T33 11Q33 13 36 25Q40 41 44 43T67 46Q94 46 127 49Q141 52 146 61Q149 65 218 339T287 628ZM703 469Q703 507 692 537T666 584T629 613T590 629T555 636Q553 636 541 636T512 636T479 637H436Q392 637 386 627Q384 623 313 339T242 52Q242 48 253 48T330 47Q335 47 349 47T373 46Q499 46 581 128Q617 164 640 212T683 339T703 469Z"
   :E "M492 213Q472 213 472 226Q472 230 477 250T482 285Q482 316 461 323T364 330H312Q311 328 277 192T243 52Q243 48 254 48T334 46Q428 46 458 48T518 61Q567 77 599 117T670 248Q680 270 683 272Q690 274 698 274Q718 274 718 261Q613 7 608 2Q605 0 322 0H133Q31 0 31 11Q31 13 34 25Q38 41 42 43T65 46Q92 46 125 49Q139 52 144 61Q146 66 215 342T285 622Q285 629 281 629Q273 632 228 634H197Q191 640 191 642T193 659Q197 676 203 680H757Q764 676 764 669Q764 664 751 557T737 447Q735 440 717 440H705Q698 445 698 453L701 476Q704 500 704 528Q704 558 697 578T678 609T643 625T596 632T532 634H485Q397 633 392 631Q388 629 386 622Q385 619 355 499T324 377Q347 376 372 376H398Q464 376 489 391T534 472Q538 488 540 490T557 493Q562 493 565 493T570 492T572 491T574 487T577 483L544 351Q511 218 508 216Q505 213 492 213Z"
   :F "M48 1Q31 1 31 11Q31 13 34 25Q38 41 42 43T65 46Q92 46 125 49Q139 52 144 61Q146 66 215 342T285 622Q285 629 281 629Q273 632 228 634H197Q191 640 191 642T193 659Q197 676 203 680H742Q749 676 749 669Q749 664 736 557T722 447Q720 440 702 440H690Q683 445 683 453Q683 454 686 477T689 530Q689 560 682 579T663 610T626 626T575 633T503 634H480Q398 633 393 631Q388 629 386 623Q385 622 352 492L320 363H375Q378 363 398 363T426 364T448 367T472 374T489 386Q502 398 511 419T524 457T529 475Q532 480 548 480H560Q567 475 567 470Q567 467 536 339T502 207Q500 200 482 200H470Q463 206 463 212Q463 215 468 234T473 274Q473 303 453 310T364 317H309L277 190Q245 66 245 60Q245 46 334 46H359Q365 40 365 39T363 19Q359 6 353 0H336Q295 2 185 2Q120 2 86 2T48 1Z"
   :G "M50 252Q50 367 117 473T286 641T490 704Q580 704 633 653Q642 643 648 636T656 626L657 623Q660 623 684 649Q691 655 699 663T715 679T725 690L740 705H746Q760 705 760 698Q760 694 728 561Q692 422 692 421Q690 416 687 415T669 413H653Q647 419 647 422Q647 423 648 429T650 449T651 481Q651 552 619 605T510 659Q492 659 471 656T418 643T357 615T294 567T236 496T189 394T158 260Q156 242 156 221Q156 173 170 136T206 79T256 45T308 28T353 24Q407 24 452 47T514 106Q517 114 529 161T541 214Q541 222 528 224T468 227H431Q425 233 425 235T427 254Q431 267 437 273H454Q494 271 594 271Q634 271 659 271T695 272T707 272Q721 272 721 263Q721 261 719 249Q714 230 709 228Q706 227 694 227Q674 227 653 224Q646 221 643 215T629 164Q620 131 614 108Q589 6 586 3Q584 1 581 1Q571 1 553 21T530 52Q530 53 528 52T522 47Q448 -22 322 -22Q201 -22 126 55T50 252Z"
   :H "M228 637Q194 637 192 641Q191 643 191 649Q191 673 202 682Q204 683 219 683Q260 681 355 681Q389 681 418 681T463 682T483 682Q499 682 499 672Q499 670 497 658Q492 641 487 638H485Q483 638 480 638T473 638T464 637T455 637Q416 636 405 634T387 623Q384 619 355 500Q348 474 340 442T328 395L324 380Q324 378 469 378H614L615 381Q615 384 646 504Q674 619 674 627T617 637Q594 637 587 639T580 648Q580 650 582 660Q586 677 588 679T604 682Q609 682 646 681T740 680Q802 680 835 681T871 682Q888 682 888 672Q888 645 876 638H874Q872 638 869 638T862 638T853 637T844 637Q805 636 794 634T776 623Q773 618 704 340T634 58Q634 51 638 51Q646 48 692 46H723Q729 38 729 37T726 19Q722 6 716 0H701Q664 2 567 2Q533 2 504 2T458 2T437 1Q420 1 420 10Q420 15 423 24Q428 43 433 45Q437 46 448 46H454Q481 46 514 49Q520 50 522 50T528 55T534 64T540 82T547 110T558 153Q565 181 569 198Q602 330 602 331T457 332H312L279 197Q245 63 245 58Q245 51 253 49T303 46H334Q340 38 340 37T337 19Q333 6 327 0H312Q275 2 178 2Q144 2 115 2T69 2T48 1Q31 1 31 10Q31 12 34 24Q39 43 44 45Q48 46 59 46H65Q92 46 125 49Q139 52 144 61Q147 65 216 339T285 628Q285 635 228 637Z"
   :I "M43 1Q26 1 26 10Q26 12 29 24Q34 43 39 45Q42 46 54 46H60Q120 46 136 53Q137 53 138 54Q143 56 149 77T198 273Q210 318 216 344Q286 624 286 626Q284 630 284 631Q274 637 213 637H193Q184 643 189 662Q193 677 195 680T209 683H213Q285 681 359 681Q481 681 487 683H497Q504 676 504 672T501 655T494 639Q491 637 471 637Q440 637 407 634Q393 631 388 623Q381 609 337 432Q326 385 315 341Q245 65 245 59Q245 52 255 50T307 46H339Q345 38 345 37T342 19Q338 6 332 0H316Q279 2 179 2Q143 2 113 2T65 2T43 1Z"
   :J "M447 625Q447 637 354 637H329Q323 642 323 645T325 664Q329 677 335 683H352Q393 681 498 681Q541 681 568 681T605 682T619 682Q633 682 633 672Q633 670 630 658Q626 642 623 640T604 637Q552 637 545 623Q541 610 483 376Q420 128 419 127Q397 64 333 21T195 -22Q137 -22 97 8T57 88Q57 130 80 152T132 174Q177 174 182 130Q182 98 164 80T123 56Q115 54 115 53T122 44Q148 15 197 15Q235 15 271 47T324 130Q328 142 387 380T447 625Z"
   :K "M285 628Q285 635 228 637Q205 637 198 638T191 647Q191 649 193 661Q199 681 203 682Q205 683 214 683H219Q260 681 355 681Q389 681 418 681T463 682T483 682Q500 682 500 674Q500 669 497 660Q496 658 496 654T495 648T493 644T490 641T486 639T479 638T470 637T456 637Q416 636 405 634T387 623L306 305Q307 305 490 449T678 597Q692 611 692 620Q692 635 667 637Q651 637 651 648Q651 650 654 662T659 677Q662 682 676 682Q680 682 711 681T791 680Q814 680 839 681T869 682Q889 682 889 672Q889 650 881 642Q878 637 862 637Q787 632 726 586Q710 576 656 534T556 455L509 418L518 396Q527 374 546 329T581 244Q656 67 661 61Q663 59 666 57Q680 47 717 46H738Q744 38 744 37T741 19Q737 6 731 0H720Q680 3 625 3Q503 3 488 0H478Q472 6 472 9T474 27Q478 40 480 43T491 46H494Q544 46 544 71Q544 75 517 141T485 216L427 354L359 301L291 248L268 155Q245 63 245 58Q245 51 253 49T303 46H334Q340 37 340 35Q340 19 333 5Q328 0 317 0Q314 0 280 1T180 2Q118 2 85 2T49 1Q31 1 31 11Q31 13 34 25Q38 41 42 43T65 46Q92 46 125 49Q139 52 144 61Q147 65 216 339T285 628Z"
   :L "M228 637Q194 637 192 641Q191 643 191 649Q191 673 202 682Q204 683 217 683Q271 680 344 680Q485 680 506 683H518Q524 677 524 674T522 656Q517 641 513 637H475Q406 636 394 628Q387 624 380 600T313 336Q297 271 279 198T252 88L243 52Q243 48 252 48T311 46H328Q360 46 379 47T428 54T478 72T522 106T564 161Q580 191 594 228T611 270Q616 273 628 273H641Q647 264 647 262T627 203T583 83T557 9Q555 4 553 3T537 0T494 -1Q483 -1 418 -1T294 0H116Q32 0 32 10Q32 17 34 24Q39 43 44 45Q48 46 59 46H65Q92 46 125 49Q139 52 144 61Q147 65 216 339T285 628Q285 635 228 637Z"
   :M "M289 629Q289 635 232 637Q208 637 201 638T194 648Q194 649 196 659Q197 662 198 666T199 671T201 676T203 679T207 681T212 683T220 683T232 684Q238 684 262 684T307 683Q386 683 398 683T414 678Q415 674 451 396L487 117L510 154Q534 190 574 254T662 394Q837 673 839 675Q840 676 842 678T846 681L852 683H948Q965 683 988 683T1017 684Q1051 684 1051 673Q1051 668 1048 656T1045 643Q1041 637 1008 637Q968 636 957 634T939 623Q936 618 867 340T797 59Q797 55 798 54T805 50T822 48T855 46H886Q892 37 892 35Q892 19 885 5Q880 0 869 0Q864 0 828 1T736 2Q675 2 644 2T609 1Q592 1 592 11Q592 13 594 25Q598 41 602 43T625 46Q652 46 685 49Q699 52 704 61Q706 65 742 207T813 490T848 631L654 322Q458 10 453 5Q451 4 449 3Q444 0 433 0Q418 0 415 7Q413 11 374 317L335 624L267 354Q200 88 200 79Q206 46 272 46H282Q288 41 289 37T286 19Q282 3 278 1Q274 0 267 0Q265 0 255 0T221 1T157 2Q127 2 95 1T58 0Q43 0 39 2T35 11Q35 13 38 25T43 40Q45 46 65 46Q135 46 154 86Q158 92 223 354T289 629Z"
   :N "M234 637Q231 637 226 637Q201 637 196 638T191 649Q191 676 202 682Q204 683 299 683Q376 683 387 683T401 677Q612 181 616 168L670 381Q723 592 723 606Q723 633 659 637Q635 637 635 648Q635 650 637 660Q641 676 643 679T653 683Q656 683 684 682T767 680Q817 680 843 681T873 682Q888 682 888 672Q888 650 880 642Q878 637 858 637Q787 633 769 597L620 7Q618 0 599 0Q585 0 582 2Q579 5 453 305L326 604L261 344Q196 88 196 79Q201 46 268 46H278Q284 41 284 38T282 19Q278 6 272 0H259Q228 2 151 2Q123 2 100 2T63 2T46 1Q31 1 31 10Q31 14 34 26T39 40Q41 46 62 46Q130 49 150 85Q154 91 221 362L289 634Q287 635 234 637Z"
   :O "M740 435Q740 320 676 213T511 42T304 -22Q207 -22 138 35T51 201Q50 209 50 244Q50 346 98 438T227 601Q351 704 476 704Q514 704 524 703Q621 689 680 617T740 435ZM637 476Q637 565 591 615T476 665Q396 665 322 605Q242 542 200 428T157 216Q157 126 200 73T314 19Q404 19 485 98T608 313Q637 408 637 476Z"
   :n "M33 744H47Q86 744 94 730V722Q94 713 94 699T94 668 95 629 95 587Q95 556 95 521T94 462V439Q91 420 80 414T35 405H17V382Q17 359 19 359L29 358Q39 357 57 356T94 354Q111 353 130 352T159 349 170 348H173V388Q173 426 174 426T179 421 191 406 210 388 239 369 277 353Q297 348 328 348 442 352 455 461 456 468 456 600V686Q456 724 458 731T469 741Q490 744 518 744H534V790H526L502 789Q479 788 452 788T414 787Q311 787 302 790H294V744H310Q371 744 371 728 372 726 372 590 371 455 370 447 364 419 350 405T326 388 300 386Q255 386 221 420 194 447 187 475T179 558V622 682Q179 712 180 722T183 735 192 741Q213 744 241 744H257V790H249L226 789Q202 788 175 788T137 787Q34 787 25 790H17V744H33Z"
   :P "M287 628Q287 635 230 637Q206 637 199 638T192 648Q192 649 194 659Q200 679 203 681T397 683Q587 682 600 680Q664 669 707 631T751 530Q751 453 685 389Q616 321 507 303Q500 302 402 301H307L277 182Q247 66 247 59Q247 55 248 54T255 50T272 48T305 46H336Q342 37 342 35Q342 19 335 5Q330 0 319 0Q316 0 282 1T182 2Q120 2 87 2T51 1Q33 1 33 11Q33 13 36 25Q40 41 44 43T67 46Q94 46 127 49Q141 52 146 61Q149 65 218 339T287 628ZM645 554Q645 567 643 575T634 597T609 619T560 635Q553 636 480 637Q463 637 445 637T416 636T404 636Q391 635 386 627Q384 621 367 550T332 412T314 344Q314 342 395 342H407H430Q542 342 590 392Q617 419 631 471T645 554Z"
   :Q "M399 -80Q399 -47 400 -30T402 -11V-7L387 -11Q341 -22 303 -22Q208 -22 138 35T51 201Q50 209 50 244Q50 346 98 438T227 601Q351 704 476 704Q514 704 524 703Q621 689 680 617T740 435Q740 255 592 107Q529 47 461 16L444 8V3Q444 2 449 -24T470 -66T516 -82Q551 -82 583 -60T625 -3Q631 11 638 11Q647 11 649 2Q649 -6 639 -34T611 -100T557 -165T481 -194Q399 -194 399 -87V-80ZM636 468Q636 523 621 564T580 625T530 655T477 665Q429 665 379 640Q277 591 215 464T153 216Q153 110 207 59Q231 38 236 38V46Q236 86 269 120T347 155Q372 155 390 144T417 114T429 82T435 55L448 64Q512 108 557 185T619 334T636 468ZM314 18Q362 18 404 39L403 49Q399 104 366 115Q354 117 347 117Q344 117 341 117T337 118Q317 118 296 98T274 52Q274 18 314 18Z"
   :R "M230 637Q203 637 198 638T193 649Q193 676 204 682Q206 683 378 683Q550 682 564 680Q620 672 658 652T712 606T733 563T739 529Q739 484 710 445T643 385T576 351T538 338L545 333Q612 295 612 223Q612 212 607 162T602 80V71Q602 53 603 43T614 25T640 16Q668 16 686 38T712 85Q717 99 720 102T735 105Q755 105 755 93Q755 75 731 36Q693 -21 641 -21H632Q571 -21 531 4T487 82Q487 109 502 166T517 239Q517 290 474 313Q459 320 449 321T378 323H309L277 193Q244 61 244 59Q244 55 245 54T252 50T269 48T302 46H333Q339 38 339 37T336 19Q332 6 326 0H311Q275 2 180 2Q146 2 117 2T71 2T50 1Q33 1 33 10Q33 12 36 24Q41 43 46 45Q50 46 61 46H67Q94 46 127 49Q141 52 146 61Q149 65 218 339T287 628Q287 635 230 637ZM630 554Q630 586 609 608T523 636Q521 636 500 636T462 637H440Q393 637 386 627Q385 624 352 494T319 361Q319 360 388 360Q466 361 492 367Q556 377 592 426Q608 449 619 486T630 554Z"
   :S "M308 24Q367 24 416 76T466 197Q466 260 414 284Q308 311 278 321T236 341Q176 383 176 462Q176 523 208 573T273 648Q302 673 343 688T407 704H418H425Q521 704 564 640Q565 640 577 653T603 682T623 704Q624 704 627 704T632 705Q645 705 645 698T617 577T585 459T569 456Q549 456 549 465Q549 471 550 475Q550 478 551 494T553 520Q553 554 544 579T526 616T501 641Q465 662 419 662Q362 662 313 616T263 510Q263 480 278 458T319 427Q323 425 389 408T456 390Q490 379 522 342T554 242Q554 216 546 186Q541 164 528 137T492 78T426 18T332 -20Q320 -22 298 -22Q199 -22 144 33L134 44L106 13Q83 -14 78 -18T65 -22Q52 -22 52 -14Q52 -11 110 221Q112 227 130 227H143Q149 221 149 216Q149 214 148 207T144 186T142 153Q144 114 160 87T203 47T255 29T308 24Z"
   :T "M40 437Q21 437 21 445Q21 450 37 501T71 602L88 651Q93 669 101 677H569H659Q691 677 697 676T704 667Q704 661 687 553T668 444Q668 437 649 437Q640 437 637 437T631 442L629 445Q629 451 635 490T641 551Q641 586 628 604T573 629Q568 630 515 631Q469 631 457 630T439 622Q438 621 368 343T298 60Q298 48 386 46Q418 46 427 45T436 36Q436 31 433 22Q429 4 424 1L422 0Q419 0 415 0Q410 0 363 1T228 2Q99 2 64 0H49Q43 6 43 9T45 27Q49 40 55 46H83H94Q174 46 189 55Q190 56 191 56Q196 59 201 76T241 233Q258 301 269 344Q339 619 339 625Q339 630 310 630H279Q212 630 191 624Q146 614 121 583T67 467Q60 445 57 441T43 437H40Z"
   :t "M79 358Q132 354 161 302T193 180V165H233V349H368V395H233V539Q234 664 234 680T241 712Q255 751 290 751 334 751 344 680 345 672 345 634V599H385V634 646Q385 723 343 763 316 790 273 790 239 790 214 778T176 747 157 712 150 680Q149 673 149 532V395H70V358H79Z"
   :theta "M35 200Q35 302 74 415T180 610T319 704Q320 704 327 704T339 705Q393 701 423 656Q462 596 462 495Q462 380 417 261T302 66T168 -10H161Q125 -10 99 10T60 63T41 130T35 200ZM383 566Q383 668 330 668Q294 668 260 623T204 521T170 421T157 371Q206 370 254 370L351 371Q352 372 359 404T375 484T383 566ZM113 132Q113 26 166 26Q181 26 198 36T239 74T287 161T335 307L340 324H145Q145 321 136 286T120 208T113 132Z"})

(def numbers
  {:0 "M57 184Q113 103 210 103 258 103 306 129T384 221Q421 304 421 449 421 604 378 686 358 728 323 753T262 784 211 791Q185 791 159 785T98 753 43 686Q0 604 0 449 0 275 57 184ZM282 172Q252 140 211 140 169 140 139 172 114 198 106 244T98 436Q98 594 106 644T142 723Q170 753 211 753 251 753 279 723 308 693 315 639T323 436Q323 291 315 245T282 172Z"
   :1 "M180 210 167 215Q153 220 127 225T69 232H50V186H69Q116 184 156 171T212 147 240 125Q242 122 252 122 261 122 269 128V427L270 727Q277 734 282 736T306 740 368 742H394V788H383Q362 785 224 785 88 785 67 788H55V742H81Q103 742 119 742T144 741 160 738 168 736 174 731 180 727V210Z"
   :2 "M70 361Q43 361 27 343T11 299Q11 228 64 176T196 124Q287 124 348 180T410 325Q410 368 390 407T342 475 262 549Q226 580 162 641L103 697 179 698Q336 698 346 693 353 691 370 604V601H410V604Q409 607 397 695T382 787V790H11V771 759Q11 752 17 744T47 709Q76 677 97 653 106 643 131 616T165 579 194 546 222 512 245 482 266 450 281 421 294 389 301 359 304 326Q304 263 270 217T173 171Q140 171 115 188T80 221 70 240Q70 241 75 241 93 241 112 255T131 301Q131 326 115 343T70 361Z"
   :3 "M93 313Q66 313 51 296T35 252Q35 197 83 154T199 111Q234 111 243 112 317 124 356 165T396 254Q396 306 362 355T268 426L265 428Q265 429 274 431T303 440 341 461Q423 514 423 601 423 680 361 739T204 798Q124 798 66 755T8 646Q8 618 26 601T71 583Q99 583 117 601T135 646Q135 657 132 666T125 682 114 694 102 702 92 706 84 709L80 710Q131 755 204 755 259 755 287 702 304 669 304 601V581Q304 486 240 454 225 448 179 447L137 446 134 444Q132 441 132 428 132 410 140 410 168 410 198 405 232 400 260 363T288 251V243Q288 186 253 164 231 150 206 150 174 150 147 161T109 184 98 196H101Q104 197 109 198T119 203 131 210 141 221 149 236 152 256Q152 278 138 295T93 313Z"
   :4 "M462 800Q444 797 333 797 217 797 199 800H190V754H221Q241 754 248 754T265 752 279 747 286 739Q287 737 287 685V635H28V589L179 358Q332 126 334 125 336 123 355 123H373L379 129V589H471V635H379V686Q379 727 379 734T385 746Q393 753 442 754H471V800H462ZM293 589V255L74 588 183 589H293Z"
   :5 "M131 614Q131 638 115 654T76 670H69Q115 749 191 749 261 749 293 689 312 656 312 561 312 458 285 422 259 389 227 389H221Q143 389 103 457 99 464 96 465T81 467Q64 467 62 461 60 457 60 286V157Q60 107 65 107 67 105 69 105 70 105 90 113T145 129 220 137Q291 137 356 109 364 105 369 105 377 105 377 123V136Q295 233 172 233 141 233 116 227L106 225V397Q125 383 136 375T172 359 223 351Q304 351 360 416T416 570Q416 662 352 727T196 793Q115 793 66 739T17 617Q17 593 28 579T51 561 74 557Q99 557 115 574T131 614Z"
   :6 "M25 458Q25 295 106 200T286 105Q355 105 385 141T415 221Q415 246 401 261T362 276Q339 276 324 262T309 223Q309 179 356 170 334 148 294 145 223 145 177 205 130 271 130 407L131 411Q136 405 139 398 180 338 246 338H250Q296 338 331 357 355 371 379 397T418 454Q439 503 439 561V579Q439 602 434 622 423 681 370 737T236 793Q208 793 182 785T126 755 75 696 39 599 25 458ZM240 374Q210 374 188 391T154 436 137 493 131 555Q131 638 143 674T181 732Q205 750 234 750 285 750 312 712 325 694 330 667T335 562Q335 482 330 455T312 410Q285 374 240 374Z"
   :7 "M23 314Q24 312 40 205L56 98Q56 96 76 96H96V100Q96 110 111 117T163 126 332 128H453V167L385 260Q376 272 355 300T328 337 307 369 287 405 273 442 260 488 252 542 246 610 243 692Q243 706 243 720T242 744V753Q238 770 223 782T189 794Q178 794 168 791T147 772 136 732Q136 574 233 404 253 372 317 283L363 220H270Q96 220 87 226 81 229 76 250T66 293L63 314V317H23V314Z"
   :8 "M44 355T44 278 98 154 222 106Q293 106 348 148T403 257Q403 287 392 313T366 355 335 383 309 401 298 409L312 418Q326 428 340 438T356 449Q431 508 431 598 431 677 373 735T223 794Q133 794 75 743T17 617Q17 509 146 437L128 424Q107 411 101 404 44 355 44 278ZM260 386 266 382Q272 378 275 376T285 369 297 359 308 347 319 334 329 318 338 301 343 281 345 259Q345 216 316 186T249 148Q242 147 216 147 175 147 139 173T102 238Q102 261 115 280T141 309 191 341Q198 346 202 348L260 386ZM224 751Q282 751 324 717T366 635Q366 618 361 603T349 578 327 556 304 538 275 519 248 502Q234 493 218 483T192 466L184 461Q178 461 155 478T107 533 81 615Q81 674 124 712T224 751Z"
   :9 "M321 490Q273 566 201 566 123 566 73 507T13 381Q11 365 11 341V333Q11 240 80 171 140 111 212 111 214 111 218 111T226 112H230Q242 112 255 114T292 126 339 158 382 217Q425 305 425 443 425 583 365 680 330 736 281 767T177 799Q116 799 77 770T37 684 90 628Q112 628 127 642T142 681Q142 699 133 712T117 728 104 733L100 734Q100 736 107 740T133 750 175 755H181Q241 755 282 691 321 635 321 497V490ZM213 529Q261 529 290 480T320 347Q320 269 312 235 310 225 306 215T292 189 262 162 215 152Q177 152 150 179 129 201 123 231T116 336Q116 419 121 448T141 495Q166 529 213 529Z"})

(def characters
  {:left-paren  "M137 500Q137 431 147 369T170 262 207 174 245 107 287 55 320 21 345 0H358 362Q376 0 376 9 376 12 359 30T318 83 269 169 227 307 210 500 227 692 268 831 317 917 359 970 376 991Q376 1000 361 1000H358 345L317 976Q223 891 180 764T137 500Z"
   :right-paren "M271 1 275 0Q280 0 285 0H297L325 24Q419 109 462 236T505 500Q505 568 495 631T472 738 435 826 397 893 356 944 324 977 301 996Q298 999 297 1000H285Q277 1000 274 1000T269 997 266 988Q267 987 277 975 432 814 432 500T277 25Q267 13 266 12 266 4 271 1Z"
   :angle       "M16 988 13 986Q10 985 8 983T3 977 0 968Q0 966 2 960 12 945 291 627 342 568 419 480 540 340 561 317T592 294 606 300 611 314Q611 320 608 325 607 326 572 366T469 485 335 638L65 947 331 948H598Q611 958 611 968 611 980 596 988H16Z"})

(defn pi-over-6 [x y scale]
  [:svg {:width "2.168ex" :height "4.676ex" :view-box "0 -1221.9 933.5 2013.3"
         :transform (str "translate(" x "," y ")" "scale(" scale "," (- scale) ")")}
   [:defs
    [:path {:stroke-width "1" :id "E1-MJMATHI-3C0" :d "M132 -11Q98 -11 98 22V33L111 61Q186 219 220 334L228 358H196Q158 358 142 355T103 336Q92 329 81 318T62 297T53 285Q51 284 38 284Q19 284 19 294Q19 300 38 329T93 391T164 429Q171 431 389 431Q549 431 553 430Q573 423 573 402Q573 371 541 360Q535 358 472 358H408L405 341Q393 269 393 222Q393 170 402 129T421 65T431 37Q431 20 417 5T381 -10Q370 -10 363 -7T347 17T331 77Q330 86 330 121Q330 170 339 226T357 318T367 358H269L268 354Q268 351 249 275T206 114T175 17Q164 -11 132 -11Z"}]
    [:path {:stroke-width "1" :id "E1-MJMAIN-36" :d "M42 313Q42 476 123 571T303 666Q372 666 402 630T432 550Q432 525 418 510T379 495Q356 495 341 509T326 548Q326 592 373 601Q351 623 311 626Q240 626 194 566Q147 500 147 364L148 360Q153 366 156 373Q197 433 263 433H267Q313 433 348 414Q372 400 396 374T435 317Q456 268 456 210V192Q456 169 451 149Q440 90 387 34T253 -22Q225 -22 199 -14T143 16T92 75T56 172T42 313ZM257 397Q227 397 205 380T171 335T154 278T148 216Q148 133 160 97T198 39Q222 21 251 21Q302 21 329 59Q342 77 347 104T352 209Q352 289 347 316T329 361Q302 397 257 397Z"}]]
   [:g {:stroke "currentColor" :fill "currentColor" :stroke-width "0" :transform "matrix(1 0 0 -1 0 0)"}
    [:g {:transform (str "translate(" x "," y ")" "scale(" scale "," scale ")")}
     [:rect {:stroke "none" :width "693" :height "60" :x "0" :y "220"}]
     [:use {:xlink-href "#E1-MJMATHI-3C0" :x "60" :y "676"}]
     [:use {:xlink-href "#E1-MJMAIN-36" :x "96" :y "-687"}]]]])

(defn round
  "Rounds n to decimal precision x: (round 9.278 10) -> 9.3"
  [n x]
  (/ (.round js/Math (* x n)) x))

(defn abs [n] (.abs js/Math n))

(defn close-enough? [n]
  (> 0.0000001 (abs (- n (round n 1)))))

(defn sqrt [n]
  (.sqrt js/Math n))

(defn mult-sqrt
  "Returns the number which n is a multiple 
   of its square root, if one exists."
  [n]
  (->>
   (for [i (range 2 1000)
         :while (< (sqrt i) (/ n 2))]
     {:i i
      :mult? (close-enough? (/ n (sqrt i)))})
   (filter :mult?)
   first
   :i))

(defn sqrt-tex
  "Renders a number as a TeX string, taking into account that
   it could be a multiple of the square root of an integer."
  [n]
  (if (not (mult-sqrt n))
    n
    (let [sqrt-of (mult-sqrt n)]
      (str (round (/ n (sqrt sqrt-of)) 1)
           "\\sqrt{" sqrt-of "}"))))

(sqrt-tex 4.242640687119285)

(defn digits [n]
  (if (>= n 10)
    (conj (digits (quot n 10))
          (rem n 10))
    [n]))

(defn render-num [n x y]
  (let [digits (digits n)
        count  (count digits)]
    (into [:svg {:width    (* 9 count)
                 :view-box (str "0 0 " (* 500 count) " " 1000)}]
          (for [d (range count)]
            [:path {:transform (str "translate(" (+ x (* d 35)) "," y ") scale(0.1)")
                    :stroke    "#000000"
                    :d         ((keyword (str (get digits d))) numbers)}]))))

(defn render-letter 
  ([k x y scale on-click]
   (render-letter k x y scale "#ffcc00"))
  ([k x y scale on-click color]
   [:path {:transform (str "translate(" x "," y ")" "scale(" scale "," (- scale) ")")
           :fill    color
           :d         (k letters)
           :on-click on-click}]))

(defn render-letters [[letters] x y]
  (let [count  (count letters)]
    (into [:svg {:width    (* 22 count)
                 :height   (* 9 count)
                 :view-box (str "0 0 " (* 1000 count) " " 2000)}]
          (for [c (range count)]
            [:path {:transform (str "translate(" (+ x  (* c 1000)) "," y ") scale(2)")
                    :stroke    "#000000"
                    :d         (get letters c)}]))))