module utilitiesMod
contains

  ! ====================================================== !
  ! === ProgressBar program                            === !
  ! ====================================================== !
  subroutine show__progressBar( index, LoopMax )
    implicit none
    integer, intent(in) :: index, LoopMax
    integer             :: j
    integer, parameter  :: ndigit = 50

    print'(a,$)',' ['
    do j=1, index*ndigit/LoopMax
       print'(a,$)','='
    enddo
    print'(a,$)','>'

    do j=index*ndigit/LoopMax+1,ndigit
       print'(a,$)',' '
    enddo
    print '(a,$)', ']'
    print '(f6.1,a,a,$)', 100.d0*index/LoopMax, '%', char(13)

    ! -- Finalize -- !
    if ( index.eq.LoopMax ) print *, ''

    return
  end subroutine show__progressBar


  ! ====================================================== !
  ! === display program title logo                     === !
  ! ====================================================== !
  subroutine show__programLogo
    implicit none
    character(1)  :: frame
    character(3)  :: side
    character(25) :: line1, line2, line3, line4, line5

    frame = "="
    side  = "==="
    line1 = " _____ ____  _  ___  _   "
    line2 = "|_   _|  _ \| |/ / || |  "
    line3 = "  | | | |_) | ' /| || |_ "
    line4 = "  | | |  _ <| . \|__   _|"
    line5 = "  |_| |_| \_\_|\_\  |_|  "

    write(6,*)
    write(6,*)
    write(6,'((a55))') repeat( frame, 55 )
    write(6,'(a3,12x,a25,12x,a3)')  side,  line1,  side
    write(6,'(a3,12x,a25,12x,a3)')  side,  line2,  side
    write(6,'(a3,12x,a25,12x,a3)')  side,  line3,  side
    write(6,'(a3,12x,a25,12x,a3)')  side,  line4,  side
    write(6,'(a3,12x,a25,12x,a3)')  side,  line5,  side
    write(6,'((a55))') repeat( frame, 55 )
    write(6,*)
    write(6,*)

    return
  end subroutine show__programLogo


  ! ====================================================== !
  ! === display program end logo                       === !
  ! ====================================================== !
  subroutine show__endLogo
    implicit none
    character(1)  :: frame
    character(3)  :: side
    character(19) :: line1, line2, line3, line4, line5

    frame = "="
    side  = "==="
    line1 = " _____ _   _ ____  "
    line2 = "| ____| \ | |  _ \ "
    line3 = "|  _| |  \| | | | |"
    line4 = "| |___| |\  | |_| |"
    line5 = "|_____|_| \_|____/ "

    write(6,*)
    write(6,*)
    write(6,"((a49))") repeat( frame, 49 )
    write(6,"(a3,12x,a19,12x,a3)")  side,  line1,  side
    write(6,"(a3,12x,a19,12x,a3)")  side,  line2,  side
    write(6,"(a3,12x,a19,12x,a3)")  side,  line3,  side
    write(6,"(a3,12x,a19,12x,a3)")  side,  line4,  side
    write(6,"(a3,12x,a19,12x,a3)")  side,  line5,  side
    write(6,"((a49))") repeat(  frame, 49 )
    write(6,*)
    write(6,*)

    return
  end subroutine show__endLogo


  ! ====================================================== !
  ! === count number of lines                          === !
  ! ====================================================== !
  subroutine count__nlines( FileName, count )
    use variablesMod
    implicit none
    character(cLen) :: FileName
    integer         :: count
    
    open(lun,file=trim(FileName),status='old')
    count = 0
    do
       read(lun,*,end=100)
       count = count + 1
    end do
100 close(lun)
    return
  end subroutine count__nlines
  

end module utilitiesMod
