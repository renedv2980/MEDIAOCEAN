*          DATA SET ACSCR1B    AT LEVEL 094 AS OF 09/02/15                      
*PHASE T60C1BA,+0                                                               
*&&ONLIN SET   Y                                                                
*                                                                               
***********************************************************************         
*                                                                     *         
* THIS VERSION MATCHES THE UK VERSION LEVEL 93 AS OF 12/16/11         *         
*                                                                     *         
***********************************************************************         
*                                                                               
* PID  LVl DATE    COMMENTS                                                     
* ---------------------------------                                             
* SMAN 093 16DEC11 PR002242 UK/US MERGE                                         
*                                                                               
         TITLE 'Column or Row Properties'                                       
T60C1B   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,0C1B,RA,R8,CLEAR=YES,RR=RE                                     
*                                                                               
         USING TWAD,R5                                                          
         USING WORKD,R7                                                         
         USING LWSD,RC                                                          
         L     RC,APALOCAL         4K AREA                                      
*                                                                               
         ST    RE,APRELO                                                        
         ST    RB,APBASE1                                                       
         ST    RA,APBASE2                                                       
         ST    RD,APWORKA                                                       
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         MVC   FVXTRA,SPACES                                                    
         EJECT ,                                                                
         USING RESRECD,R2                                                       
         LA    R2,IOKEY                                                         
         SR    RF,RF                                                            
         IC    RF,APMODE                                                        
         SLL   RF,2                                                             
         B     *(RF)                                                            
*                                                                               
         B     VALKEY                                                           
         B     VALREC                                                           
         B     DISKEY                                                           
         B     DISREC                                                           
         B     EXIT                DELETE                                       
         B     EXIT                RESTORE                                      
         B     EXIT                VALSEL                                       
         B     EXIT                GETSEL                                       
         B     EXIT                DISSEL                                       
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                LSTSCR                                       
         B     EXIT                VALREQ                                       
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                COPY                                         
         B     EXIT                                                             
         B     EXIT                                                             
*                                                                               
EXIT     CLI   APPFKEY,PFKNEXT                                                  
         BE    EXIT97                                                           
         CLI   APMODE,APMVALK                                                   
         BE    EXIT95                                                           
         CLI   APMODE,APMDISK                                                   
         BE    EXIT95                                                           
         TM    TWASWPST,TWASWAP    SHOULD WE SWAP?                              
         BZ    EXIT95              NO                                           
*                                                                               
         CLI   APPFKEY,PFKHLP                                                   
         BNE   *+10                                                             
         XC    ACURDEF,ACURDEF     SET TO BEGINING OF HELP                      
         XC    APCURSOR,APCURSOR   DON'T SET CURSOR ON WRONG SCREEN             
         MVI   APPFKEY,0                                                        
         MVI   APMODE,APMSWP                                                    
         MVC   APPARM(1),TWASWPRE          SWAP TO NEW RECORD                   
         MVC   APPARM+1(1),TWASWPAC        SWAP TO NEW ACTION                   
*                                                                               
EXIT95   OI    TWALSCTL,TWALSHLD                                                
*                                                                               
EXIT97   OI    TWALSCTL,TWALSRTN                                                
*                                                                               
EXIT99   CLC   FVMSGNO,=AL2(FVFOK)                                              
         BE    XIT                                                              
         CLI   APACTN,ACTCHA                                                    
         BNE   XIT                                                              
*                                                                               
XIT      XIT1                                                                   
         EJECT ,                                                                
***********************************************************************         
*  VALKEY                                                             *         
***********************************************************************         
         SPACE 1                                                                
VALKEY   MVI   NEWKEY,NO           RESET TO NO                                  
         NI    ACINDS,TURNOFF-ACINOHLP                                          
         MVI   SEQNUM,1            Default                                      
         MVC   RESKEY,SPACES                                                    
         MVI   RESKTYP,RESKTYPQ    X'2D'                                        
         MVI   RESKSUB,RESKSUBQ    X'02'                                        
         MVC   RESKCPY,CUABIN      COMPANY CODE                                 
*                                                                               
*        TM    TWAMODE,TWAMLSM                                                  
*        BO    VALKEY05                                                         
         GOTO1 AFVAL,PRPCODEH                                                   
         MVC   RESKFORM,FVIFLD                                                  
         BE    VALKEY05                                                         
         TM    PRPCODEH+4,FVITHIS  NEW FORMAT INPUT?                            
         BZ    *+10                                                             
         MVC   SAVFORM,SPACES                                                   
         CLC   SAVFORM,FVIFLD                                                   
         BNH   IVALEKEY                                                         
         MVI   CURRCOLN,0                                                       
         MVC   PRPCODE,SAVFORM                                                  
         OI    PRPCODEH+6,FVOXMT                                                
         MVC   RESKFORM,SAVFORM                                                 
*                                                                               
VALKEY05 MVC   SAVFORM,RESKFORM                                                 
         TM    TWAMODE,TWAMLSM                                                  
         BO    VALKEY08                                                         
         CLC   SAVFORM,FVIFLD                                                   
         BE    *+10                                                             
         XC    SAVRECK,SAVRECK                                                  
*                                                                               
VALKEY08 CLI   PRPNUM,C' '         Left justify potential number                
         BH    VALKEY10                                                         
         MVC   PRPNUM(1),PRPNUM+1                                               
         MVI   PRPNUM+1,C' '                                                    
*                                                                               
VALKEY10 GOTO1 AFVAL,PRPNUMH       Verify input                                 
         BE    VALKEY12            Yes have some                                
         SR    RF,RF                                                            
         ICM   RF,1,CURCOL#                                                     
         BNZ   *+8                                                              
         LA    RF,1                Default to row/col 1                         
         CVD   RF,APDUB                                                         
         OI    APDUB+7,X'0F'                                                    
         UNPK  PRPNUM,APDUB                                                     
         CLI   PRPNUM,C'0'                                                      
         BH    *+14                                                             
         MVC   PRPNUM,PRPNUM+1     Left justify                                 
         MVI   PRPNUM+1,C' '                                                    
         GOTO1 AFVAL,PRPNUMH                                                    
*                                                                               
VALKEY12 TM    FVIIND,FVINUM                                                    
         BZ    IVALNUM                                                          
         L     R1,SCFULL                                                        
         STC   R1,SEQNUM           Save off value                               
*&&DO                                                                           
         GOTO1 AFVAL,PRPCODEH                                                   
         MVC   RESKFORM,FVIFLD                                                  
         BE    VALKEY15                                                         
         TM    PRPCODEH+4,FVITHIS  ANY INPUT?                                   
         BZ    *+10                                                             
         MVC   SAVFORM,SPACES                                                   
         CLC   SAVFORM,SPACES                                                   
         BNH   IVALEKEY            ENTER KEY                                    
         MVC   PRPCODE,SAVFORM                                                  
         OI    PRPCODEH+6,FVOXMT   TRANSMIT                                     
         MVC   RESKFORM,SAVFORM                                                 
*                                                                               
VALKEY15 MVC   SAVFORM,RESKFORM                                                 
*&&                                                                             
VALKEY18 MVC   APRECKEY(L'RESKEY),RESKEY                                        
         LA    R1,IORD+IOACCFIL+IO1                                             
         CLI   APACTN,ACTDIS                                                    
         BE    *+8                                                              
         LA    R1,IOLOCK(,R1)      READ FOR UPDATE                              
         GOTO1 AIO                                                              
         BL    VALKEY99            IO ERROR                                     
         BNE   VALKEY20                                                         
*                                                                               
         L     R2,AIOAREA1                                                      
         GOTO1 GETTYPE,(R2)                                                     
         OI    SCRTYPH+6,FVOXMT                                                 
         MVC   SCRTYP(L'APREPCDE),APREPCDE                                      
         MVI   APINDS,APIOKDIS+APIOKCHA                                         
         B     VALKEY90                                                         
*                                                                               
VALKEY20 MVI   SEQNUM,1            Default to 1                                 
*                                                                               
VALKEY90 CLC   INREC,TWALREC               Same record type ?                   
         BNE   VALKEY91                    No                                   
         CLC   SEQNUM,CURCOL#              Same row/col as last time ?          
         BNE   VALKEY91                    No                                   
         CLC   APRECKEY(L'RESKEY),SAVRECK  Same fromat as last time ?           
         BE    VALKEY92                    Yes                                  
*                                                                               
VALKEY91 MVC   CURCOL#,SEQNUM                                                   
         GOTO1 =A(INISCRN),RR=APRELO                                            
         OC    ACURELEM,ACURELEM                                                
         BZ    IVALRCN                    Invalid row or column number          
         OI    INFLAG1,INFDISR                Force re-display                  
         GOTO1 =A(BLDSCRN),RR=APRELO                                            
         GOTO1 =A(SETADRS),RR=APRELO                                            
         B     VALKEY98                                                         
*                                                                               
VALKEY92 GOTO1 =A(INISCRN),RR=APRELO                                            
         OC    ACURELEM,ACURELEM                                                
         BZ    IVALRCN                    Invalid row or column number          
         GOTO1 =A(SETADRS),RR=APRELO                                            
*                                                                               
VALKEY98 MVC   FVMSGNO,=AL2(FVFOK)                                              
*                                                                               
VALKEY99 B     EXIT                                                             
         DROP  R2                                                               
         EJECT ,                                                                
***********************************************************************         
*  VALREC                                                             *         
***********************************************************************         
         SPACE 1                                                                
         USING RESRECD,R2                                                       
         USING RFLELD,R9                                                        
VALREC   L     R2,AIOAREA1                                                      
         NI    ACINDS,TURNOFF-ACINOHLP                                          
         MVI   CURSEQ#,1                      Set to 1, line sequence #         
         GOTO1 AFVAL,PRPNMEH                                                    
         BNE   VREC005                        Name has not been input           
         GOTO1 ADDNAME,APPARM,(R2),PRPNMEH    Get format name                   
         BNE   VREC999                        On error exit                     
*                                                                               
VREC005  MVC   RESKEY,APRECKEY                                                  
         XC    ELEMENT,ELEMENT                                                  
         L     R3,ACURELEM                                                      
         SR    RF,RF                                                            
         IC    RF,1(,R3)           Length of element                            
         BCTR  RF,0                                                             
         EXMVC RF,ELEMENT,0(R3)                                                 
         CLI   INREC,RECROWPR                                                   
         BE    VREC100             Process row, not column                      
                                                                                
***********************************************************************         
*        Column heading 1                                                       
***********************************************************************         
                                                                                
ELM      USING RCLELD,ELEMENT                                                   
PRO      USING RFLELD,PROFELEM                                                  
                                                                                
VREC009A ICM   R4,15,AHEAD1                                                     
         BZ    VREC010                                                          
         SR    RF,RF                                                            
         IC    RF,ELM.RCLDATLN        Length of keyword data                    
         LA    R5,ELM.RCLNDATA(RF)    Point to end of keyword data              
*                                                                               
         GOTO1 AFVAL,(R4)                                                       
         MVC   ELM.RCLHD1LN,FVILEN                                              
         SR    RF,RF                                                            
         IC    RF,FVILEN                                                        
         SHI   RF,1                                                             
         BM    VREC010                                                          
         EXMVC RF,0(R5),FVIFLD     Move in heading 1                            
         LA    R5,1(RF,R5)         Point to end of head 1                       
                                                                                
***********************************************************************         
*        Column heading 2                                                       
***********************************************************************         
VREC010  ICM   R4,15,AHEAD2                                                     
         BZ    VREC015                                                          
         GOTO1 AFVAL,(R4)                                                       
         MVC   ELM.RCLHD2LN,FVILEN                                              
         SR    RF,RF                                                            
         IC    RF,FVILEN                                                        
         SHI   RF,1                                                             
         BM    VREC012                                                          
         EXMVC RF,0(R5),FVIFLD                                                  
*                                                                               
VREC012  IC    RF,ELM.RCLDATLN     Length of keyword data                       
         SR    R1,R1                                                            
         IC    R1,ELM.RCLHD1LN     Length of heading 1                          
         AR    RF,R1                                                            
         IC    R1,ELM.RCLHD2LN     Lenght of heading 2                          
         LA    R1,RCLNLNQ(R1,RF)   Total element length                         
         STC   R1,ELM.RCLLN                                                     
                                                                                
***********************************************************************         
*        Column width                                                           
***********************************************************************         
VREC015  ICM   R4,15,AWIDTH                                                     
         BZ    VREC020                                                          
         CLI   8(R4),C' '          Delete leading blanks                        
         BH    VREC016                                                          
         MVC   8(1,R4),9(R4)                                                    
         MVI   9(R4),C' '                                                       
*                                                                               
         USING DEFTABD,R6                                                       
VREC016  GOTO1 AFVAL,(R4)                                                       
         BE    VREC018               Missing input ?                            
         MVI   ELM.RCLWDTH,12        Default if all else fails                  
         ICM   R6,15,AKEYWORD        Yes                                        
         BZ    VREC020                                                          
         MVC   ELM.RCLWDTH,DEFWDTH   Code default width                         
         TM    ELM.RCLOPT2,RCLNAME                                              
         BZ    VREC020                                                          
         MVI   ELM.RCLWDTH,36        Name default width                         
         B     VREC020                                                          
*                                                                               
VREC018  TM    FVIIND,FVINUM         Is it a number ?                           
         BZ    IVALNUM               No                                         
         L     R1,SCFULL                                                        
         STC   R1,ELM.RCLWDTH                                                   
                                                                                
***********************************************************************         
*        Print column                                                           
***********************************************************************         
VREC020  ICM   R4,15,APRINT                                                     
         BZ    VREC025                                                          
         NI    ELM.RCLOPT,TURNOFF-(RCLHIDE+RCLMERGE)                            
         NI    ELM.RCLOPT2,TURNOFF-RCLCZERO                                     
*&&US*&& NI    ELM.RCLOPT4,TURNOFF-RCLXCOL                                      
         GOTO1 AFVAL,(R4)                                                       
         BNE   VREC025             Default is yes                               
         CLC   APYES,FVIFLD                                                     
         BE    VREC025                                                          
         CLC   APNO,FVIFLD         Not to print                                 
         BNE   VREC021                                                          
         OI    ELM.RCLOPT,RCLHIDE                                               
         B     VREC025                                                          
*                                                                               
VREC021  DS    0H                                                               
*&&US                                                                           
         CLI   FVIFLD,C'X'             Process but hide and no sort             
         BNE   *+12                                                             
         OI    ELM.RCLOPT4,RCLXCOL                                              
         B     VREC025                                                          
*&&                                                                             
         CLI   FVIFLD,C'M'             Stange merge option                      
         BNE   VREC022                                                          
         OI    ELM.RCLOPT,RCLMERGE                                              
         TM    ELM.RCLOPT,RCLACCM      Amount column ?                          
         BZ    VREC025                                                          
         OI    ELM.RCLOPT2,RCLCZERO    Turn this option on                      
         B     VREC025                                                          
*                                                                               
VREC022  TM    ELM.RCLOPT,RCLACCM  Amount column ?                              
         BZ    IVALIPUT            At this point must be amount column          
         CLI   FVIFLD,C'C'                                                      
         BNE   VREC023                                                          
         OI    ELM.RCLOPT2,RCLCZERO    Turn this option on                      
         B     VREC025                                                          
*                                                                               
VREC023  CLI   FVIFLD,C'H'             Hide and "C" option                      
         BNE   IVALIPUT                                                         
         OI    ELM.RCLOPT2,RCLCZERO    Turn this option on                      
         OI    ELM.RCLOPT,RCLHIDE                                               
                                                                                
***********************************************************************         
*        Total on column                                                        
***********************************************************************         
VREC025  ICM   R4,15,ATOTAL        Total on column ?                            
         BZ    VREC030                                                          
         CLI   INREC,RECCOLPR                                                   
         BNE   VREC030                                                          
         NI    ELM.RCLOPT,TURNOFF-(RCLSUPP+RCLNOTOT)                            
         NI    ELM.RCLOPT2,TURNOFF-RCLTOT                                       
         GOTO1 AFVAL,(R4)                                                       
         BNE   VREC030                                                          
         TM    ELM.RCLOPT,RCLACCM  Amount column ?                              
         BZ    VREC028             Check non-amount options                     
         CLC   APONLY,FVIFLD       Only show total option                       
         BNE   *+8                                                              
         OI    ELM.RCLOPT,RCLSUPP  Suppress detail, show total                  
*                                                                               
         CLI   FVIFLD,C'S'         Only show detail option                      
         BNE   VREC030                                                          
         OI    ELM.RCLOPT,RCLNOTOT Suppress totals, show detail                 
         B     VREC030                                                          
*                                                                               
VREC028  CLC   APNO,FVIFLD         No total                                     
         BE    VREC030                                                          
         CLC   APYES,FVIFLD                                                     
         BNE   IVALITOT                                                         
         OI    ELM.RCLOPT2,RCLTOT  Set to total                                 
                                                                                
***********************************************************************         
*        Redundant data or repeat constant                                      
***********************************************************************         
VREC030  ICM   R4,15,AREDUN        Redundant                                    
         BNZ   VREC032                                                          
         ICM   R4,15,AREPET        Repeat constant                              
         BZ    VREC035                                                          
*                                                                               
VREC032  NI    ELM.RCLOPT3,TURNOFF-(RCLRDATY+RCLRDATN)                          
         GOTO1 AFVAL,(R4)                                                       
         BNE   VREC035             Set to default                               
         BAS   RE,LEFTJFY                                                       
         CLC   APOPNPRN,FVIFLD     Is 1st char. "("                             
         BE    VREC035             Yes, so default                              
         CLC   APNO,FVIFLD         No                                           
         BNE   *+8                                                              
         OI    ELM.RCLOPT3,RCLRDATN                                             
         CLC   APYES,FVIFLD        Yes                                          
         BNE   *+8                                                              
         OI    ELM.RCLOPT3,RCLRDATY                                             
                                                                                
***********************************************************************         
*        Over-ride column sort                                                  
***********************************************************************         
VREC035  DS    0H                                                               
         ICM   R4,15,ACSORT                                                     
         BZ    VREC040                                                          
         MVI   ELM.RCLSORTN,0                                                   
         GOTO1 AFVAL,(R4)                                                       
         BNE   VREC040             No input                                     
         BAS   RE,LEFTJFY                                                       
         MVC   8(2,R4),FVIFLD                                                   
         GOTO1 AFVAL,(R4)                                                       
*                                                                               
         TM    FVIIND,FVINUM       Is it a number                               
         BZ    IVALNUM                                                          
         L     R1,SCFULL                                                        
         STC   R1,ELM.RCLSORTN                                                  
         CLC   ELM.RCLSORTN,LASTCOL#  Can't be > then # of columns              
         BH    IVALCOLN                                                         
         SR    RF,RF                                                            
         LA    RE,COLARRAY-1(R1)                                                
         CLI   0(RE),0             Is this available                            
         BE    *+14                Yes                                          
         CLC   ELM.RCLSEQ,0(RE)                                                 
         BNE   IVALDUPL            Duplicate sort number                        
*                                                                               
         CLC   MAXSORT#,ELM.RCLSORTN                                            
         BH    *+10                                                             
         MVC   MAXSORT#,ELM.RCLSORTN                                            
         MVC   0(1,RE),ELM.RCLSEQ  Save off column number                       
         IC    RF,MAXSORT#         Number of sort over-rides                    
         LA    RE,COLARRAY         Check for errors                             
*                                                                               
VREC038  CLI   0(RE),0                                                          
         BE    IVALSORT            Must be contiguous from start                
         LA    RE,1(,RE)                                                        
         BCT   RF,VREC038                                                       
         B     VREC040                                                          
                                                                                
***********************************************************************         
*        Column stack                                                           
***********************************************************************         
VREC040  DS    0H                                                               
         ICM   R4,15,ASTACK                                                     
         BZ    VREC045                                                          
         MVI   ELM.RCLSTACK,0                                                   
         GOTO1 AFVAL,(R4)                                                       
         BNE   VREC045             Input not found                              
         BAS   RE,LEFTJFY                                                       
         CLC   APCOLCHR,FVIFLD     Prefix of column char                        
         BNE   IVALIPUT                                                         
         MVC   8(3,R4),FVIFLD+1    Remove char                                  
         GOTO1 AFVAL,(R4)          Go again to validate number                  
         TM    FVIIND,FVINUM       Is it a number ?                             
         BZ    IVALNUM             No                                           
         L     R1,SCFULL                                                        
         LTR   R1,R1               Can't be zero                                
         BZ    IVALNUM                                                          
*        CLM   R1,1,LASTCOL#       Last column                                  
*        BNL   IVALCOLN            Invalid column number                        
         CLM   R1,1,ELM.RCLSEQ     Stack # must be less than cur col            
         BNL   IVALCOLN            Invalid column stack number                  
         TM    ELM.RCLOPT,RCLHIDE  Hidden column ?                              
         BO    IVALSTCK                                                         
         CLI   ELM.RCLWDTH,0       Zero width ?                                 
         BE    IVALSTCK                                                         
*                                                                               
         USING RCLELD,R3                                                        
         SR    RF,RF               Find column stacking under                   
         L     R3,AIOAREA1                                                      
         AH    R3,DATADISP                                                      
VREC042A CLI   0(R3),EOR           End of record                                
         BNE   *+6                                                              
         DC    H'00'                                                            
*                                                                               
         CLI   0(R3),RCLELQ        X'C3' - Column element                       
         BNE   VREC042B                                                         
         CLM   R1,1,RCLSEQ         Match on column                              
         BE    VREC042C                                                         
VREC042B IC    RF,1(,R3)                                                        
         AR    R3,RF                                                            
         B     VREC042A                                                         
*                                                                               
VREC042C TM    RCLOPT,RCLHIDE      Hidden column                                
         BO    IVALSTCK                                                         
         CLI   RCLWDTH,0                                                        
         BE    IVALSTCK                                                         
         STC   R1,RCLSTACK         Save stack value                             
         DROP  R3                                                               
                                                                                
***********************************************************************         
*        Round or decimals                                                      
***********************************************************************         
VREC045  DS    0H                                                               
         ICM   R4,15,AROUND                                                     
         BZ    VREC050                                                          
         MVI   ELM.RCLDCMLS,0                                                   
         GOTO1 AFVAL,(R4)                                                       
         BNE   VREC050                                                          
         BAS   RE,LEFTJFY                                                       
         CLC   APOPNPRN,FVIFLD     Is 1st char. "("                             
         BE    VREC050             Yes                                          
*                                                                               
         L     R3,=A(DECMTAB)                                                   
         A     R3,APRELO                                                        
VREC046  CLI   0(R3),EOT           End of table ?                               
         BE    IVALIPUT                                                         
         CLC   FVIFLD(2),0(R3)                                                  
         BE    VREC047                                                          
         LA    R3,DECMLEN(,R3)                                                  
         B     VREC046                                                          
*                                                                               
VREC047  MVC   ELM.RCLDCMLS,2(R3)  Store converted value                        
                                                                                
***********************************************************************         
*        Minus notation                                                         
***********************************************************************         
VREC050  DS    0H                                                               
         ICM   R4,15,AMINUS                                                     
         BZ    VREC055                                                          
         NI    ELM.RCLEDOPT,TURNOFF-RCLEDTRL-RCLEDBKT-RCLEDLED-RCLEDCR          
         GOTO1 AFVAL,(R4)                                                       
         BNE   VREC055                                                          
         BAS   RE,LEFTJFY                                                       
         CLC   APOPNPRN,FVIFLD     Is 1st char. "("                             
         BE    VREC055             Yes, so default                              
         CLC   FVIFLD(1),APNO      Absolute value ?                             
         BNE   *+8                                                              
         OI    ELM.RCLEDOPT,RCLEDABS  Turns both RCLEDTRL & RCLEDLED on         
         CLI   FVIFLD,C'T'         Trailing  ?                                  
         BNE   *+8                                                              
         OI    ELM.RCLEDOPT,RCLEDTRL                                            
         CLI   FVIFLD,C'B'         Bracketed ?                                  
         BNE   *+8                                                              
         OI    ELM.RCLEDOPT,RCLEDBKT                                            
         CLI   FVIFLD,C'L'         Leading   ?                                  
         BNE   *+8                                                              
         OI    ELM.RCLEDOPT,RCLEDLED                                            
         CLI   FVIFLD,C'S'         CR / DR   ?                                  
         BNE   *+8                                                              
         OI    ELM.RCLEDOPT,RCLEDCR                                             
                                                                                
***********************************************************************         
*        Print commas                                                           
***********************************************************************         
VREC055  DS    0H                                                               
         ICM   R4,15,ACOMMA                                                     
         BZ    VREC060                                                          
         NI    ELM.RCLEDOPT,TURNOFF-(RCLEDCMY+RCLEDCMN)                         
         GOTO1 AFVAL,(R4)                                                       
         BNE   VREC060                                                          
         BAS   RE,LEFTJFY                                                       
         CLC   APOPNPRN,FVIFLD     Is 1st char. "("                             
         BE    VREC060             Yes, so default                              
         CLC   APNO,FVIFLD         Print with commas ?                          
         BNE   *+8                                                              
         OI    ELM.RCLEDOPT,RCLEDCMN     No                                     
         CLC   APYES,FVIFLD                                                     
         BNE   *+8                                                              
         OI    ELM.RCLEDOPT,RCLEDCMY     Yes                                    
                                                                                
***********************************************************************         
*        Underline                                                              
***********************************************************************         
VREC060  DS    0H                                                               
         ICM   R4,15,AUNDER                                                     
         BZ    VREC065                                                          
         NI    ELM.RCLOPT3,TURNOFF-RCLUNLNA-RCLUNLNB-RCLUNLNN-RCLUNLDB          
         GOTO1 AFVAL,(R4)                                                       
         BNE   VREC065                                                          
         BAS   RE,LEFTJFY                                                       
         CLC   APOPNPRN,FVIFLD     Is 1st char. "("                             
         BE    VREC065             Yes, so default                              
         CLI   FVIFLD,C'A'         Single above total                           
         BNE   *+8                                                              
         OI    ELM.RCLOPT3,RCLUNLNA                                             
         CLI   FVIFLD,C'B'         Single below total                           
         BNE   *+8                                                              
         OI    ELM.RCLOPT3,RCLUNLNB                                             
         CLI   FVIFLD,C'D'         Double above total                           
         BNE   *+8                                                              
         OI    ELM.RCLOPT3,RCLUNLNA+RCLUNLDB                                    
         CLI   FVIFLD,C'E'         Double below total                           
         BNE   *+8                                                              
         OI    ELM.RCLOPT3,RCLUNLNB+RCLUNLDB                                    
                                                                                
***********************************************************************         
*        Print zero amounts                                                     
***********************************************************************         
VREC065  DS    0H                                                               
         ICM   R4,15,AZEROA                                                     
         BZ    VREC070                                                          
         NI    ELM.RCLEDOPT,TURNOFF-(RCLEDZRY-RCLEDZRN)                         
         GOTO1 AFVAL,(R4)                                                       
         BNE   VREC070                                                          
         BAS   RE,LEFTJFY                                                       
         CLC   APOPNPRN,FVIFLD     Is 1st char. "("                             
         BE    VREC070             Yes, so default                              
         CLC   APNO,FVIFLD         Print zero amounts ?                         
         BNE   *+8                                                              
         OI    ELM.RCLEDOPT,RCLEDZRN     No                                     
         CLC   APYES,FVIFLD                                                     
         BNE   *+8                                                              
         OI    ELM.RCLEDOPT,RCLEDZRY     Yes                                    
                                                                                
***********************************************************************         
*        Print zero totals                                                      
***********************************************************************         
VREC070  DS    0H                                                               
         ICM   R4,15,AZEROT                                                     
         BZ    VREC075                                                          
         NI    ELM.RCLOPT4,TURNOFF-(RCLZRTTY+RCLZRTTN)                          
         GOTO1 AFVAL,(R4)                                                       
         BNE   VREC075                                                          
         BAS   RE,LEFTJFY                                                       
         CLC   APOPNPRN,FVIFLD     Is 1st char. "("                             
         BE    VREC075             Yes, so default                              
         CLC   APNO,FVIFLD         Print zero totals  ?                         
         BNE   *+8                                                              
         OI    ELM.RCLOPT4,RCLZRTTN      No                                     
         CLC   APYES,FVIFLD                                                     
         BNE   *+8                                                              
         OI    ELM.RCLOPT4,RCLZRTTY      Yes                                    
                                                                                
***********************************************************************         
*        Date control                                                           
***********************************************************************         
         USING DTFORMD,RE                                                       
VREC075  DS    0H                                                               
         ICM   R4,15,ADATES                                                     
         BZ    VREC080                                                          
         MVI   ELM.RCLDTEFM,0      Set to 0, indicating default                 
         GOTO1 AFVAL,(R4)                                                       
         BNE   VREC080             No input                                     
         L     RE,=A(DATETAB)                                                   
         A     RE,APRELO                                                        
         SR    R1,R1                                                            
         IC    R1,FVXLEN                                                        
*                                                                               
VREC076  CLI   DTFLANG,EOT         End of table ?                               
         BE    VREC080             Not in table                                 
         CLC   DTFLANG,CULANG                                                   
         BNE   VREC077                                                          
         EXCLC R1,DTFNAME,FVIFLD                                                
         BE    VREC078                                                          
*                                                                               
VREC077  LA    RE,DTFLNQ(,RE)                                                   
         B     VREC076                                                          
*                                                                               
VREC078  MVC   ELM.RCLDTEFM,DTFCODE                                             
         DROP  RE                                                               
                                                                                
***********************************************************************         
*        Right justify data                                                     
***********************************************************************         
VREC080  ICM   R4,15,ARJUST        Right justify                                
         BZ    VREC200                                                          
         NI    ELM.RCLOPT4,TURNOFF-RCLRJUST                                     
         GOTO1 AFVAL,(R4)                                                       
         BNE   VREC200                                                          
         CLC   APYES,FVIFLD                                                     
         BNE   VREC200                                                          
         OI    ELM.RCLOPT4,RCLRJUST                                             
         B     VREC200                                                          
         EJECT 1                                                                
***********************************************************************         
*        Process row element                                          *         
***********************************************************************         
                                                                                
ELM      USING RRWELD,ELEMENT                                                   
                                                                                
VREC100  DS    0H                                                               
         NI    ELM.RRWOPT,TURNOFF-RRWPAGE                                       
         ICM   R4,15,ARTYPE        row type                                     
         BZ    VREC110                                                          
         GOTO1 AFVAL,(R4)                                                       
         MVC   ELM.RRWTYPE,FVIFLD                                               
         CLI   ELM.RRWTYPE,RRWMID  Is it a mid-line ?                           
         BNE   VREC105             No                                           
         CLC   ELM.RRWSEQ,MIDLINE# Yes. Is this allowed ?                       
         BNL   VREC102             Yes                                          
         SR    R1,R1               Not sure yet                                 
         IC    R1,ELM.RRWSEQ       May be able to make mid                      
         AHI   R1,1                                                             
         CLM   R1,1,MIDLINE#       If one before 1st mid                        
         BNE   IVALHEAD            No                                           
*                                                                               
VREC102  TM    ELM.RRWOPT,RRWADR   Can't have address with mid                  
         BO    IVALADR                                                          
*&&US                                                                           
         TM    ELM.RRWOPT3,RRWBDR  Can't have business address with mid         
         BO    IVALADR                                                          
*&&                                                                             
         B     VREC110                                                          
*                                                                               
VREC105  CLC   ELM.RRWSEQ,MIDLINE#      Check 1st mid-line                      
         BH    IVALHEAD                 Only mids allow one we have mid         
         CLI   ELM.RRWTYPE,C'N'                                                 
         BE    VREC110                                                          
         CLI   ELM.RRWTYPE,RRWLHEAD     Left   heading                          
         BE    VREC110                                                          
         CLI   ELM.RRWTYPE,RRWRHEAD     Rigth  heading                          
         BE    VREC110                                                          
         CLI   ELM.RRWTYPE,RRWCHEAD     Center heading                          
         BNE   IVALHEAD                 Not valid                               
                                                                                
VREC110  DS    0H                                                               
         NI    ELM.RRWOPT,TURNOFF-(RRWTOT+RRWTOTSP)                             
         NI    ELM.RRWOPT2,TURNOFF-RRWBTM                                       
         ICM   R4,15,ATOTAL         Row total option                            
         BZ    VREC120                                                          
         GOTO1 AFVAL,(R4)                                                       
         BE    *+10                                                             
         MVC   FVIFLD(1),APNO       Default value                               
*                                                                               
         CLC   APNO,FVIFLD                                                      
         BE    VREC120                                                          
         OI    ELM.RRWOPT,RRWTOT   Set to total                                 
         CLC   APYES,FVIFLD        Totaling ?                                   
         BE    VREC120                                                          
         CLI   FVIFLD,C'S'          Total put on seperate page ?                
         BNE   *+8                                                              
         OI    ELM.RRWOPT,RRWTOTSP  Yes                                         
         CLI   FVIFLD,C'B'          Totals put on bottom of page ?              
         BNE   VREC120                                                          
         OI    ELM.RRWOPT2,RRWBTM   Yes                                         
                                                                                
VREC120  DS    0H                                                               
         MVI   FVILEN,0            Make sure zero                               
         ICM   R4,15,APRFIX         Row prefix                                  
         BZ    VREC122                                                          
         GOTO1 AFVAL,(R4)                                                       
*                                                                               
VREC122  MVC   ELM.RRWPFXLN,FVILEN                                              
         SR    RF,RF                                                            
         IC    RF,ELM.RRWDATLN      Length of keyword                           
         LA    RE,ELM.RRWNDATA(RF)  Point at end of data                        
         SR    R1,R1                                                            
         ICM   R1,1,FVILEN          Length of data                              
         BZ    VREC125              No prefix                                   
         SHI   R1,1                                                             
         EXMVC R1,0(RE),FVIFLD                                                  
         AHI   R1,1                                                             
*                                                                               
VREC125  LA    R1,RRWNLNQ(RF,R1)                                                
         STC   R1,ELM.RRWLN        Save new length of element                   
                                                                                
         USING DTFORMD,RE                                                       
VREC130  DS    0H                                                               
         ICM   R4,15,ADATES                                                     
         BZ    VREC200                                                          
         MVI   ELM.RRWDTEFM,0      Set to 0, indicating default                 
         GOTO1 AFVAL,(R4)                                                       
         BNE   VREC200             No input                                     
         L     RE,=A(DATETAB)                                                   
         A     RE,APRELO                                                        
         SR    R1,R1                                                            
         IC    R1,FVXLEN                                                        
*                                                                               
VREC132  CLI   DTFLANG,EOT         End of table ?                               
         BE    VREC200             Not in table                                 
         CLC   DTFLANG,CULANG                                                   
         BNE   VREC133                                                          
         EXCLC R1,DTFNAME,FVIFLD                                                
         BE    VREC134                                                          
VREC133  LA    RE,DTFLNQ(,RE)                                                   
         B     VREC132                                                          
*                                                                               
VREC134  MVC   ELM.RRWDTEFM,DTFCODE                                             
         DROP  RE                                                               
         EJECT ,                                                                
***********************************************************************         
*        More column adjustment                                       *         
***********************************************************************         
                                                                                
VREC200  DS    0H                                                               
         L     R6,ACURELEM         Load row or column element                   
         MVI   0(R6),X'FF'         mark for deletion                            
         CLI   INREC,RECROWPR      Row properties ?                             
         BE    VREC400                                                          
*                                                                               
         USING STYELD,RE                                                        
         ICM   RE,15,ASTYELEM                                                   
         BNZ   *+6                                                              
         DC    H'00'                                                            
*------------------------------------------*                                    
*        Check & Change report size        *                                    
*------------------------------------------*                                    
         USING RCLELD,R6                                                        
         SR    R1,R1                                                            
         SR    RF,RF                                                            
         L     R6,AIOAREA1                                                      
         AH    R6,DATADISP                                                      
VREC305  CLI   0(R6),EOR           End of record ?                              
         BE    VREC320                                                          
         CLI   0(R6),RCLELQ        Column element ?                             
         BNE   VREC310                                                          
         ICM   RF,1,RCLWDTH        Get width of column                          
         BZ    VREC310             Must be zero                                 
         TM    RCLOPT,RCLHIDE      Is it a hidden column ?                      
         BO    VREC310             Yes                                          
         CLI   RCLSTACK,0          Stacked column ?                             
         BNE   VREC310             Yes                                          
         LA    R1,1(RF,R1)         Add up report width                          
VREC310  IC    RF,1(,R6)                                                        
         AR    R6,RF                                                            
         B     VREC305                                                          
         DROP  R6                                                               
*                                                                               
ELM      USING RCLELD,ELEMENT                                                   
*                                                                               
VREC320  DS    0H                                                               
         LA    R1,1(,R1)                                                        
         ICM   RF,1,ELM.RCLWDTH    New column width                             
         BZ    VREC322             Width is zero                                
         CLI   ELM.RCLOPT,RCLHIDE  Is it hidden column ?                        
         BO    VREC322             Yes                                          
         CLI   ELM.RCLSTACK,0      Is it a stacked column ?                     
         BNE   VREC322             Yes                                          
         LA    R1,1(RF,R1)                                                      
*                                                                               
         USING STYELD,RE                                                        
VREC322  L     RE,ASTYELEM                                                      
         CLI   0(RE),STYELQ        X'25' element                                
         BE    *+6                                                              
         DC    H'00'               Should have been                             
*                                                                               
PRO      USING RPFELD,PROFELEM                                                  
                                                                                
         STCM  R1,3,STYWIDTH           Replace with new width                   
*        TM    PRO.RPFPOPT2,RPFDOWN    Restriction on width ?                   
         TM    PRO.RPFDNOPT,RPFDDOWN   Restriction on width ?                   
         BO    VREC350                 No, downloading                          
         MVC   APCURSOR,AWIDTH                                                  
         CHI   R1,MAXRPTWD         Max report width                             
         BH    IVALWIDE            Report too wide                              
         DROP  RE                                                               
         DROP  ELM                                                              
*                                                                               
VREC350  DS    0H                                                               
*                                                                               
VREC400  L     R2,AIOAREA1                                                      
         MVI   APELCODE,X'FF'                                                   
         GOTO1 DELEL,(R2)                                                       
         SR    R1,R1                                                            
         IC    R1,ELEMENT+1                                                     
         BCTR  R1,0                                                             
         EXMVC R1,APELEM,ELEMENT                                                
         GOTO1 ADDEL,(R2)                                                       
*                                                                               
         LA    R1,IOADD+IOACCFIL+IO1                                            
         TM    APINDS,APIOKADD           ADD A RECORD?                          
         BO    VREC480                                                          
         LA    R1,IOWRITE+IOACCFIL+IO1                                          
         TM    APINDS,APIOKCHA           CHANGE A RECORD?                       
         BO    VREC480                                                          
         DC    H'00'                     WHAT THE HELL?                         
                                                                                
VREC480  GOTO1 AIO                                                              
         BE    VREC500                                                          
         TM    IOERR,IOEDUP        DOES RECORD EXIST BUT DELETED                
         BO    *+6                                                              
         DC    H'00'               BAD WRITE OR SOMETHING DUDE                  
         MVC   FVMSGNO,=AL2(FVFCCDR)                                            
                                                                                
ELM      USING CONELD,APELEM                                                    
                                                                                
VREC500  DS    0H                                                               
         MVI   LCINDS,0            Local indicators                             
         MVI   SVTYPE,CONTROW      Default to row                               
         CLI   INREC,RECROWPR      Row profile ?                                
         BE    *+8                                                              
         MVI   SVTYPE,CONTCOL      Switch  to column                            
         MVC   IOKEY,RESKEY        User format key                              
         LA    R2,IOKEY                                                         
         MVI   RESKSEQ,RESKS5TH    Make this record 5                           
*                                                                               
         L     R2,AIOAREA3                                                      
         LA    R1,IORD+IOACCFIL+IO3                                             
         CLI   APACTN,ACTDIS                                                    
         BE    *+8                                                              
         LA    R1,IOLOCK+IORDEL(,R1)    Read for update                         
         GOTO1 AIO                                                              
         BNL   *+6                                                              
         DC    H'00'               IO error (Low)                               
         BNE   VREC504             Not found or deleted                         
*                                                                               
         MVI   LCINDS,LCCHAREC                                                  
         LR    R3,R2                                                            
         AH    R3,DATADISP                                                      
VREC501  CLI   0(R3),EOR           EOR ?                                        
         BNE   VREC502                                                          
*                                                                               
         TM    LCINDS,LCDELELM     Did we mark any for deletion ?               
         BZ    VREC508             No                                           
         MVI   APELCODE,X'FF'                                                   
         GOTO1 DELEL,(R2)          Yes                                          
         B     VREC508                                                          
*                                                                               
         USING CONELD,R3                                                        
VREC502  CLI   0(R3),CONELQ        X'??'                                        
         BNE   VREC503             No                                           
         CLC   CONTYPE,SVTYPE                                                   
         BNE   VREC503             Didn't match                                 
         CLC   CONNUM,SEQNUM                                                    
         BNE   VREC503                                                          
         MVI   CONEL,X'FF'         Mark for deletion                            
         OI    LCINDS,LCDELELM                                                  
         DROP  R3                                                               
*                                                                               
VREC503  SR    RF,RF                                                            
         IC    RF,1(,R3)                                                        
         AR    R3,RF                                                            
         B     VREC501                                                          
*                                                                               
VREC504  DS    0H                                                               
*                                                                               
         TM    IOERR,IOEDEL        Is record marked deleted                     
         BZ    VREC505             No, record not found                         
         MVI   LCINDS,LCCHAREC                                                  
         L     R2,AIOAREA3                                                      
         NI    RESRSTA,TURNOFF-X'80'   Restore record                           
         LR    R3,R2                                                            
         AH    R3,DATADISP                                                      
         MVI   0(R3),0                       Mark EOR                           
         MVC   RESRLEN,=AL2(RESRFST-RESKEY)  New length of record               
         B     VREC508                                                          
*                                                                               
VREC505  MVI   LCINDS,LCADDREC     Record not on file, so ok to add             
         XC    RESKEY(256),RESKEY  Reset AIO area                               
         MVC   RESKEY,IOKEY                                                     
         EJECT                                                                  
***********************************************************************         
*        Build UF or UC keyword data   (Generic form)                 *         
***********************************************************************         
                                                                                
VREC508  MVC   FVMSGNO,=AL2(FVFOK)                                              
         ICM   R4,15,AKYWUSF       1st field of User field                      
         BZ    VREC550                                                          
*                                  Find extended field                          
VREC510  TM    1(R4),FVAXTND       Extended field header ?                      
         BZ    VREC535             Yes                                          
*                                                                               
         USING TWAXTHDR,RE                                                      
VREC520  DS    0H                                                               
         GOTO1 AFVAL,(R4)                                                       
         SR    RF,RF                                                            
         IC    RF,0(,R4)           Get length                                   
         SHI   RF,8                                                             
         LA    RE,0(R4,RF)                                                      
         CLI   TWAXTFD#,FLDPFKLN   Is it a PF key field ?                       
         BE    VREC900             Yes, so finished                             
         CLI   TWAXTUSR+1,FLDFRCDE From code                                    
         BNE   VREC535             No, so get next                              
         CLI   FVILEN,0            Any input ?                                  
         BE    VREC535             No, so get next field                        
         DROP  RE                                                               
*                                                                               
         BAS   RE,ELMINIT          Initialize element                           
         BNE   VREC999                                                          
         MVC   ELM.CONFRLN,FVILEN     Length of FROM data                       
         SR    RF,RF                                                            
         IC    RF,FVXLEN              Ex length                                 
         EXMVC RF,ELM.CONFROM,FVIFLD                                            
         LA    RF,CONLNQ+1(,RF)                                                 
*                                                                               
         XC    ELEMENT,ELEMENT                                                  
         MVI   ELEMENT+C'?',X'FF'     Wild card char.                           
         STC   RF,ELM.CONLN           Save element length                       
         IC    RF,FVILEN                                                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         TRT   FVIFLD(0),ELEMENT                                                
         BZ    *+8                                                              
         OI    ELM.CONIND,CONWILDF    Wild card in FROM                         
*                                                                               
         IC    RF,0(,R4)           Bump past C'='                               
         AR    R4,RF                                                            
         IC    RF,0(,R4)           Point to TO field                            
         AR    R4,RF                                                            
         TM    1(R4),FVAXTND       Extended field header ?                      
         BO    *+6                                                              
         DC    H'00'                                                            
*                                                                               
         USING TWAXTHDR,RE                                                      
         IC    RF,0(,R4)                                                        
         SHI   RF,8                                                             
         LA    RE,0(R4,RF)                                                      
         CLI   TWAXTUSR+1,FLDTOCDE    Verify TO data                            
         BE    *+6                                                              
         DC    H'00'               Error in coding ?                            
         DROP  RE                                                               
*                                                                               
         GOTO1 AFVAL,(R4)                                                       
         BNE   VREC530                                                          
         BAS   RE,XLATE                                                         
         TM    XLATIND,XLATACTV                                                 
         BZ    *+8                                                              
         OI    ELM.CONIND,CONMACRO   Indicate marco in line                     
         MVC   ELM.CONTOLN,FVILEN    Length of TO   data                        
         SR    RF,RF                                                            
         IC    RF,ELM.CONFRLN                                                   
         LA    RE,ELM.CONFROM(RF)  Point to end of FROM data                    
         IC    RF,FVXLEN           Ex length                                    
         EXMVC RF,0(RE),FVIFLD                                                  
         SR    R1,R1                                                            
         IC    R1,ELM.CONLN        Current element length                       
         LA    RF,1(R1,RF)                                                      
         STC   RF,ELM.CONLN        Save element length                          
*                                                                               
         XC    ELEMENT,ELEMENT                                                  
         MVI   ELEMENT+C'?',X'FF'  Wild card char.                              
         IC    RF,FVILEN           Scan for concatenation symbol                
         EX    RF,*+8                                                           
         B     *+10                                                             
         TRT   FVIFLD(0),ELEMENT                                                
         BZ    *+8                                                              
         OI    ELM.CONIND,CONWILDT     Wild card in TO                          
*                                                                               
VREC530  L     R2,AIOAREA3         A(Record)                                    
         GOTO1 ADDEL,(R2)                                                       
         BNE   VREC999             Error                                        
         OI    LCINDS,LCADDELM     Data added                                   
         SR    RF,RF                                                            
         IC    RF,CURSEQ#                                                       
         AHI   RF,1                                                             
         STC   RF,CURSEQ#                                                       
*                                                                               
VREC535  SR    RF,RF               Find extended field                          
         IC    RF,0(,R4)           No ,get next field                           
         AR    R4,RF                                                            
         B     VREC510                                                          
         EJECT                                                                  
***********************************************************************         
*        Build BILTYC keyword data                                              
***********************************************************************         
                                                                                
VREC550  ICM   R4,15,AKYWBTY       BILTYC keyword                               
         BZ    VREC900                                                          
*                                                                               
VREC555  TM    1(R4),FVAXTND       Extended field header ?                      
         BZ    VREC570             Yes                                          
*                                                                               
         USING TWAXTHDR,RE                                                      
         SR    RF,RF                                                            
         IC    RF,0(,R4)           Get length                                   
         SHI   RF,8                                                             
         LA    RE,0(R4,RF)                                                      
         CLI   TWAXTFD#,FLDPFKLN   Is it a PF key field ?                       
         BE    VREC900             Finished                                     
         MVC   EXTFLD#,TWAXTUSR+1  Save field number                            
         GOTO1 AFVAL,(R4)                                                       
         BNE   VREC570                                                          
         DROP  RE                                                               
*                                                                               
         BAS   RE,ELMINIT                                                       
         BNE   VREC999                                                          
         MVI   ELM.CONFRLN,1                                                    
         MVC   ELM.CONFROM,EXTFLD#                                              
         LA    RE,ELM.CONFROM+1                                                 
         SR    RF,RF                                                            
         IC    RF,FVXLEN                                                        
         EXMVC RF,0(RE),FVIFLD                                                  
         MVC   ELM.CONTOLN,FVILEN                                               
         LA    RF,1(RF,RE)                                                      
         LA    RE,ELM.CONELD                                                    
         SR    RF,RE                                                            
         STC   RF,ELM.CONLN                                                     
*                                                                               
         L     R2,AIOAREA3         A(Record)                                    
         GOTO1 ADDEL,(R2)                                                       
         BNE   VREC999             Error                                        
         OI    LCINDS,LCADDELM     Data added                                   
         SR    RF,RF                                                            
         IC    RF,CURSEQ#                                                       
         AHI   RF,1                                                             
         STC   RF,CURSEQ#                                                       
*                                                                               
VREC570  SR    RF,RF               Find extended field                          
         IC    RF,0(,R4)           No ,get next field                           
         AR    R4,RF                                                            
         B     VREC555                                                          
*                                                                               
VREC900  DC    0H                                                               
         TM    LCINDS,LCADDELM+LCDELELM   Any elements changed ?                
         BZ    VREC999                    No, so finished                       
         TM    LCINDS,LCDELELM                                                  
         BZ    VREC905                                                          
         LR    R3,R2                                                            
         AH    R3,DATADISP                                                      
         CLI   0(R3),EOR           End of record ?                              
         BNE   VREC905                                                          
         OI    RESRSTA,X'80'       Mark deleted                                 
*                                                                               
VREC905  LA    R1,IOADD+IOACCFIL+IO3                                            
         TM    LCINDS,LCADDREC           Add a record ?                         
         BO    VREC920                                                          
         LA    R1,IOWRITE+IOACCFIL+IO3                                          
         TM    LCINDS,LCCHAREC           CHANGE A RECORD?                       
         BO    VREC920                                                          
         DC    H'00'                     WHAT THE HELL?                         
                                                                                
VREC920  GOTO1 AIO                                                              
         BE    VREC999                                                          
         TM    IOERR,IOEDUP        DOES RECORD EXIST BUT DELETED                
         BO    *+6                                                              
         DC    H'00'               BAD WRITE OR SOMETHING DUDE                  
         MVC   FVMSGNO,=AL2(FVFCCDR)                                            
                                                                                
*                                                                               
VREC999  OI    ACINDS,ACINOHLP                                                  
         CLC   FVMSGNO,=AL2(FVFOK)                                              
         BE    DISREC                                                           
*&&DO                                                                           
         SR    R1,R1                                                            
         IC    R1,CURRCOLN                                                      
         CVD   R1,APDUB                                                         
         OI    APDUB+7,X'0F'                                                    
         UNPK  APWORK(2),APDUB                                                  
         LA    RF,1                                                             
         LA    RE,APWORK                                                        
         CLI   APWORK,C'0'                                                      
         BNE   *+8                                                              
         LA    RE,APWORK+1                                                      
         LA    RF,0                                                             
         EXMVC RF,COLNUM,0(RE)     RESTORE OLD NUMBER                           
*&&                                                                             
         B     EXIT                                                             
         DROP  R9                                                               
         EJECT ,                                                                
***********************************************************************         
*        Left justify what is in FVIFLD                               *         
***********************************************************************         
                                                                                
LEFTJFY  ST    RE,SVRE                                                          
         SR    RF,RF                                                            
         IC    RF,FVILEN           Length of data                               
         LA    RE,FVIFLD                                                        
         CLI   0(RE),C' '                                                       
         BH    LEFTJXIT            Already left justified                       
*                                                                               
LEFTJ10  CLI   0(RE),C' '          Find first real char                         
         BH    LEFTJ20             Found                                        
         LA    RE,1(,RE)           Next field                                   
         BCT   RF,LEFTJ10                                                       
         B     LEFTJXIT            No data                                      
*                                                                               
LEFTJ20  BCTR  RF,0                                                             
         EXMVC RF,FVIFLD,0(RE)                                                  
         STC   RF,FVXLEN           Save new EX length                           
         LA    RE,1(,RF)                                                        
         IC    RF,FVILEN           Old length                                   
         STC   RE,FVILEN           Save new length                              
         SR    RF,RE                                                            
         BCTR  RF,0                                                             
         LA    RE,FVIFLD(RE)       Point to end of data                         
         EXMVC RF,0(RE),SPACES                                                  
                                                                                
LEFTJXIT L     RE,SVRE                                                          
         BR    RE                                                               
         EJECT ,                                                                
***********************************************************************         
*        Initialize element to add CONELD                                       
***********************************************************************         
                                                                                
ELMINIT  NTR1                                                                   
         XC    APELEM,APELEM                                                    
         MVI   ELM.CONEL,CONELQ      X'??'                                      
         MVI   ELM.CONTYPE,CONTCOL   Default to column type                     
         MVC   ELM.CONSEQ,CURSEQ#    Current sequence number                    
         CLI   INREC,RECCOLPR        Column profile ?                           
         BE    *+8                                                              
         MVI   ELM.CONTYPE,CONTROW   Switch  to row    type                     
         L     R3,ACURELEM                                                      
         MVC   ELM.CONNUM,2(R3)      Move in row or column number               
         MVI   ELM.CONCDE,CONEQ      All are equal here                         
*                                                                               
         USING DEFTABD,R6                                                       
         ICM   R6,15,AKEYWORD                                                   
         BZ    ELMINO                                                           
         MVC   ELM.CONFROM#,DEFDDNUM                                            
         MVC   ELM.CONTO#,DEFDDNUM                                              
         SR    RE,RE                                                            
*                                                                               
ELMINO   LTR   RE,RE                                                            
         B     XIT                                                              
         DROP  R6                                                               
         EJECT ,                                                                
***********************************************************************         
*  Translate keywords in TO value                                     *         
***********************************************************************         
         SPACE 1                                                                
XLATE    NTR1                                                                   
         MVI   XLATIND,0                                                        
         XC    ELEMENT,ELEMENT                                                  
         MVI   ELEMENT+C'+',X'FF'  Concatenation symbol                         
         XC    APHALF,APHALF       Number of keywords                           
         MVC   XLATEFLD,SPACES                                                  
         SR    R6,R6                                                            
         IC    R6,FVILEN           R6 = Length of data remaining                
         LA    R3,FVIFLD           R3 = Data to scan & translate                
         LA    R4,XLATEFLD         R4 = New translated data                     
         LA    R5,FVIFLD(R6)       R5 = End of data                             
         LA    R0,1                Case &KYW                                    
         CLI   0(R3),C'&&'         Macro keyword symbol                         
         BE    XLATE15             1st character is a keyword marco             
*                                                                               
XLATE10  SR    R0,R0               Case DATA                                    
         CLI   0(R3),C'+'          Concatenationn symbol                        
         BNE   XLATE50             Move in character to XLATFLD                 
         LA    R0,2                Case DATA+&KYW or &KYW+&KYW                  
         CLI   1(R3),C'&&'         Keyword macro  symbol                        
         BE    XLATE15                                                          
*                                                                               
         SR    R0,R0               Case DATA1+DATA2                             
         TM    XLATIND,XLATKYWD    Did we just process a keyword ?              
         BZ    XLATE50             No, so keep C'+'                             
         NI    XLATIND,TURNOFF-XLATKYWD                                         
         LA    R3,1(,R3)                                                        
         SHI   R6,1                Loose the C'+', implied                      
         BP    XLATE10             Case &KYW+DATA                               
         AHI   R6,1                Restore length by one                        
         SHI   R3,1                Bump back to restore                         
         B     XLATE50             Case &KYW+                                   
*                                                                               
XLATE15  AR    R3,R0                                                            
         XC    APFULL,APFULL                                                    
         MVC   FAKEFLD,SPACES                                                   
         LR    RF,R5               Point to end of data (in R5)                 
         LR    R1,R5               R1 = end of data                             
         SR    RF,R3               Length remaining                             
         SHI   RF,1                                                             
         BNP   XLATE40             Case &NONKYW or DATA+&NONKYW                 
*                                                                               
         EX    RF,*+8                                                           
         B     *+10                                                             
         TRT   0(0,R3),ELEMENT     Scan for C'+'                                
*                                                                               
         ST    R1,APFULL           Location of C'+' or end                      
         SR    R1,R3               Length of possible keyword                   
         STC   R1,FAKEFLDH         Set length of fake field                     
         STC   R1,FAKEFLDH+5                                                    
         SHI   R1,1                                                             
         BNP   XLATE40             Case &NONKYW or DATA+&NONKYW                 
*                                                                               
         EXMVC R1,FAKEFLD,0(R3)                                                 
         MVI   REPMODE,REPEVRY                                                  
         GOTO1 VALDEF,FAKEFLDH     Find keyword                                 
         BNE   XLATE40             Case &NONKYW or DATA+&NONKYW                 
         OI    XLATIND,XLATKYWD                                                 
         OI    XLATIND,XLATACTV    Active                                       
*                                                                               
         USING DEFTABD,R1                                                       
         MVI   0(R4),X'03'         Marker and length (len,keyword(2))           
         MVC   1(2,R4),DEFDDNUM                                                 
         LA    R4,3(,R4)                                                        
         DROP  R1                                                               
*                                                                               
         L     R3,APFULL           New location                                 
         LR    R6,R5               R5 = End of data                             
         SR    R6,R3               Length of what is length                     
         BP    XLATE10                                                          
         B     XLATE60                                                          
*                                                                               
XLATE40  SR    R3,R0               Case &NONKYW or DATA+&NONKYW                 
         NI    XLATIND,TURNOFF-XLATKYWD                                         
         B     XLATE50             Not a potential keyword                      
*                                                                               
XLATE50  MVC   0(1,R4),0(R3)       Move in data                                 
         LA    R3,1(,R3)                                                        
         LA    R4,1(,R4)                                                        
         BCT   R6,XLATE10                                                       
*                                                                               
XLATE60  LA    RE,XLATEFLD                                                      
         SR    R4,RE                                                            
         STC   R4,FVILEN           New length of data                           
         BCTR  R4,0                Move data to FVIFLD                          
         STC   R4,FVXLEN           New excute length                            
         EXMVC R4,FVIFLD,XLATEFLD                                               
*                                                                               
XLATEXIT B     XIT                                                              
         EJECT ,                                                                
***********************************************************************         
*  Expand converted keyword data                                      *         
***********************************************************************         
                                                                                
XPAND    NTR1                                                                   
         LR    R4,R1                                                            
         L     R3,0(,R1)           Data to expand                               
         SR    R0,R0                                                            
         ICM   R0,1,0(R1)          Length of data passed                        
         BNZ   *+6                                                              
         DC    H'00'                                                            
*                                                                               
         MVI   APBYTE,0                                                         
         XC    0(4,R1),0(R1)       Clear return data                            
         LA    R6,XPANDFLD                                                      
         MVC   XPANDFLD,SPACES                                                  
*                                                                               
XPAND10  CLI   0(R3),03            Marco detection                              
         BNE   XPAND50                                                          
         CLI   APBYTE,1                                                         
         BNE   XPAND12                                                          
         MVI   0(R6),C'+'                                                       
         LA    R6,1(,R6)                                                        
*                                                                               
XPAND12  MVI   0(R6),C'&&'         Move marco symbol C'&'                       
         LA    R6,1(,R6)                                                        
         MVC   APWORK(7),SPACES                                                 
         MVI   APWORK,ESCLEFTJ     Left justified                               
         MVC   APWORK+1(2),1(R3)   Dictionary number                            
         MVI   APWORK+3,6          length of data                               
         GOTO1 VDICTAT,APPARM,C'SU  ',(6,APWORK),0                              
*                                                                               
         LA    RE,APWORK+6                                                      
         LA    R1,6                                                             
XPAND15  CLI   0(RE),C' '          Figure out length of keyword                 
         BH    XPAND18                                                          
         BCTR  RE,0                                                             
         BCT   R1,XPAND15                                                       
         DC    H'00'               Can't be zero                                
*                                                                               
XPAND18  EXMVC R1,0(R6),APWORK     Move in keyword                              
         LA    R3,3(,R3)           Bump past field                              
         LA    R6,1(R1,R6)                                                      
         SHI   R0,3                                                             
         BNP   XPAND60                                                          
         MVI   0(R6),C'+'          Concatenation field                          
         LA    R6,1(,R6)                                                        
         B     XPAND10                                                          
*                                                                               
XPAND50  MVC   0(1,R6),0(R3)                                                    
         MVI   APBYTE,1                                                         
         LA    R3,1(,R3)                                                        
         LA    R6,1(,R6)                                                        
         BCT   R0,XPAND10                                                       
*                                                                               
XPAND60  LA    RE,XPANDFLD                                                      
         ST    RE,0(,R4)           Location of expanded data                    
         SR    R6,RE                                                            
         STC   R6,0(,R4)           Length of expanded data                      
*                                                                               
XPANDXIT B     XIT                                                              
         EJECT ,                                                                
***********************************************************************         
*  DISKEY                                                             *         
***********************************************************************         
         SPACE 1                                                                
         USING RESRECD,R2                                                       
DISKEY   LA    R2,APRECKEY                                                      
         MVC   PRPCODE,RESKFORM                                                 
         NI    TWASWPST,TURNOFF-TWASWAP                                         
         B     EXIT                                                             
         DROP  R2                                                               
         EJECT ,                                                                
***********************************************************************         
*  DISREC                                                             *         
***********************************************************************         
         SPACE 1                                                                
PRO      USING RPFELD,PROFELEM                                                  
*                                                                               
         USING RESRECD,R2                                                       
DISREC   DS    0H                                                               
         NI    ACINDS,TURNOFF-ACINOHLP                                          
         L     R5,ATWA             Reload - A(Screen base)                      
         MVC   PRPKYW,SPACES                                                    
         MVC   PRPRNG,SPACES                                                    
         LA    R4,PRPTAGH                                                       
         SR    RF,RF                                                            
         LR    R3,R4                                                            
DR010    TM    1(R4),FVAXTND       Extended field header ?                      
         BO    DR012                                                            
DR011    IC    RF,0(,R4)                                                        
         LR    R3,R4               R3 = Privious field                          
         AR    R4,RF                                                            
         B     DR010                                                            
*                                                                               
         USING TWAXTHDR,RE                                                      
DR012    IC    RF,0(,R4)                                                        
         SHI   RF,8                                                             
         LA    RE,0(R4,RF)                                                      
         CLI   TWAXTFD#,FLDPFKLN   PF Key line ?                                
         BNE   DR011                                                            
         DROP  RE                                                               
*                                                                               
         TWAXC PRPNMEH,(R3)                                                     
         L     R2,AIOAREA1         COLUMN ELEMENTS                              
         GOTO1 GETNAME,APPARM,(R2),PRPNMEH                                      
         GOTO1 GETPER,APPARM,(R2),PRPOWNH                                       
*                                                                               
         ICM   R3,15,ACURELEM      Load element based on                        
         OI    PRPKYWH+6,FVOXMT                                                 
         SR    R1,R1                                                            
         IC    R1,2(,R3)           Column or row number                         
         CVD   R1,APDUB                                                         
         OI    APDUB+7,X'0F'                                                    
         OI    PRPNUMH+6,FVOXMT                                                 
         UNPK  PRPNUM,APDUB                                                     
         CLI   PRPNUM,C'0'                                                      
         BH    *+8                                                              
         MVI   PRPNUM,C' '                                                      
                                                                                
***********************************************************************         
*        Column keyword                                                         
***********************************************************************         
         USING RCLELD,R3                                                        
         CLI   0(R3),RCLELQ        Column element ?                             
         BNE   DR100                                                            
         MVC   PRPRNG(2),=C'1-'                                                 
         SR    RF,RF                                                            
         IC    RF,LASTCOL#                                                      
         CVD   RF,APDUB                                                         
         OI    APDUB+7,X'0F'                                                    
         UNPK  PRPRNG+2(2),APDUB                                                
         CLI   PRPRNG+2,C'0'                                                    
         BH    DR013                                                            
         MVC   PRPRNG+2(1),PRPRNG+3                                             
         MVI   PRPRNG+3,C' '                                                    
*                                                                               
DR013    SR    RF,RF                                                            
         IC    RF,RCLDATLN         Length of data                               
         BCTR  RF,0                                                             
         EXMVC RF,PRPKYW,RCLNDATA  Move in keyword data for col                 
         LA    RE,RCLNDATA+1(RF)   Point to end                                 
                                                                                
***********************************************************************         
*        Column heading 1                                                       
***********************************************************************         
         ICM   R4,15,AHEAD1        Column heading 1 ?                           
         BZ    DR014                                                            
         OI    6(R4),FVOXMT        Re-Transmit                                  
         ICM   RF,1,RCLHD1LN       Length of heading 1                          
         BZ    DR014                                                            
         BCTR  RF,0                                                             
         EXMVC RF,8(R4),0(RE)                                                   
         LA    RE,1(RE,RF)                                                      
                                                                                
***********************************************************************         
*        Column heading 2                                                       
***********************************************************************         
DR014    ICM   R4,15,AHEAD2        Column heading 2 ?                           
         BZ    DR015                                                            
         OI    6(R4),FVOXMT        Re-Transmit                                  
         SR    RF,RF                                                            
         ICM   RF,1,RCLHD2LN       Length of heading 2                          
         BZ    DR015                                                            
         BCTR  RF,0                                                             
         EXMVC RF,8(R4),0(RE)                                                   
                                                                                
***********************************************************************         
*        Date format                                                            
***********************************************************************         
                                                                                
         USING DTFORMD,RE                                                       
DR015    ICM   R4,15,ADATES                                                     
         BZ    DR020                                                            
         OI    6(R4),FVOXMT        Re-Transmit                                  
         L     RE,=A(DATETAB)                                                   
         A     RE,APRELO                                                        
         MVC   8(L'DTFNAME,R4),DTFNAME                                          
         CLI   RCLDTEFM,0                                                       
         BE    DR020                                                            
DR016    CLI   DTFLANG,EOT         End of table ?                               
         BNE   *+6                                                              
         DC    H'00'                                                            
*                                                                               
         CLC   DTFLANG,CULANG                                                   
         BNE   DR017                                                            
         CLC   DTFCODE,RCLDTEFM                                                 
         BE    DR018                                                            
*                                                                               
DR017    LA    RE,DTFLNQ(,RE)                                                   
         B     DR016                                                            
*                                                                               
DR018    MVC   8(L'DTFNAME,R4),DTFNAME                                          
         DROP  RE                                                               
                                                                                
***********************************************************************         
*        Column width                                                           
***********************************************************************         
DR020    ICM   R4,15,AWIDTH                                                     
         BZ    DR025                                                            
         OI    6(R4),FVOXMT        Re-Transmit                                  
         SR    RF,RF                                                            
         IC    RF,RCLWDTH                                                       
         CVD   RF,APDUB                                                         
         OI    APDUB+7,X'0F'                                                    
         UNPK  8(2,R4),APDUB                                                    
         CLI   8(R4),C'0'                                                       
         BH    *+8                                                              
         MVI   8(R4),C' '                                                       
                                                                                
***********************************************************************         
*        Print column                                                           
***********************************************************************         
DR025    ICM   R4,15,APRINT                                                     
         BZ    DR026                                                            
         OI    6(R4),FVOXMT        Re-Transmit                                  
         MVC   8(1,R4),APYES       Default                                      
         TM    RCLOPT,RCLHIDE      Display column ?                             
         BZ    DR026               No                                           
         MVC   8(1,R4),APNO        Yes, Hide                                    
         TM    RCLOPT2,RCLCZERO    Hide & Zero                                  
         BZ    DR030               No                                           
         MVI   8(R4),C'H'                                                       
         B     DR030                                                            
*                                                                               
DR026    TM    RCLOPT2,RCLCZERO    Zero elimination                             
         BZ    *+8                 No                                           
         MVI   8(R4),C'C'          Yes                                          
*                                                                               
         TM    RCLOPT,RCLMERGE     Merge option                                 
         BZ    *+8                 No                                           
         MVI   8(R4),C'M'          Yes                                          
                                                                                
***********************************************************************         
*        Total on column                                                        
***********************************************************************         
DR030    ICM   R4,15,ATOTAL        Total                                        
         BZ    DR035                                                            
         OI    6(R4),FVOXMT        Re-Transmit                                  
         MVI   8(R4),C' '                                                       
         TM    RCLOPT,RCLACCM      Amount field                                 
         BO    DR032               Yes                                          
         MVC   8(1,R4),APNO        Default not to total                         
         TM    RCLOPT2,RCLTOT      Total ?                                      
         BZ    DR035               No                                           
         MVC   8(1,R4),APYES                                                    
         B     DR035                                                            
*                                                                               
DR032    TM    RCLOPT,RCLNOTOT                                                  
         BZ    *+8                                                              
         MVI   8(R4),C'S'                                                       
         TM    RCLOPT,SUPPRESS                                                  
         BZ    *+10                                                             
         MVC   8(1,R4),APONLY                                                   
                                                                                
***********************************************************************         
*        Redundant column data                                                  
***********************************************************************         
DR035    ICM   R4,15,AREDUN        Redunant data ?                              
         BZ    DR040                                                            
         OI    6(R4),FVOXMT        Re-transmit                                  
         MVC   8(1,R4),APOPNPRN    Character "("                                
         MVC   9(1,R4),APNO                                                     
         MVC   10(1,R4),APCLSPRN   Character ")"                                
         TM    PRO.RPFPOPT,RPFRDAT Reduntand default ?                          
         BZ    *+10                                                             
         MVC   9(1,R4),APYES                                                    
         TM    RCLOPT3,RCLRDATY+RCLRDATN                                        
         BZ    DR040                                                            
         MVC   8(3,R4),SPACES                                                   
         TM    RCLOPT3,RCLRDATY    Yes redundant                                
         BZ    *+10                                                             
         MVC   9(1,R4),APYES                                                    
         TM    RCLOPT3,RCLRDATN    No redundant                                 
         BZ    DR040                                                            
         MVC   9(1,R4),APNO                                                     
                                                                                
***********************************************************************         
*        Repeat constant column                                                 
***********************************************************************         
DR040    ICM   R4,15,AREPET        Repeat constant ?                            
         BZ    DR045                                                            
         OI    6(R4),FVOXMT        Re-transmit                                  
         MVC   8(1,R4),APNO        Default                                      
         TM    RCLOPT3,RCLRDATY                                                 
         BZ    *+10                                                             
         MVC   8(1,R4),APYES                                                    
                                                                                
***********************************************************************         
*        Over-ride sort column                                                  
***********************************************************************         
DR045    ICM   R4,15,ACSORT        Resort columns ?                             
         BZ    DR050                                                            
         OI    6(R4),FVOXMT        Re-transmit                                  
         SR    R1,R1                                                            
         MVC   8(2,R4),SPACES                                                   
         ICM   R1,1,RCLSORTN       Sort over-ride ?                             
         BZ    DR050                                                            
         CVD   R1,APDUB                                                         
         OI    APDUB+7,X'0F'                                                    
         UNPK  8(2,R4),APDUB                                                    
         CLI   8(R4),C'0'                                                       
         BNE   *+8                                                              
         MVI   8(R4),C' '          Clear leading zero                           
                                                                                
***********************************************************************         
*        Stack column                                                           
***********************************************************************         
DR050    ICM   R4,15,ASTACK        Stack report ?                               
         BZ    DR055                                                            
         OI    6(R4),FVOXMT        Re-transmit                                  
         SR    R1,R1                                                            
         ICM   R1,1,RCLSTACK       Stack report ?                               
         BZ    DR055                                                            
         MVC   8(1,R4),APCOLCHR    Column character                             
         MVC   APWORK,SPACES                                                    
         CVD   R1,APDUB                                                         
         OI    APDUB+7,X'0F'                                                    
         UNPK  APWORK(2),APDUB                                                  
         MVC   9(2,R4),APWORK                                                   
         CLI   APWORK,C'0'                                                      
         BNE   *+10                                                             
         MVC   9(2,R4),APWORK+1                                                 
                                                                                
***********************************************************************         
*        Round / Decimal column                                                 
***********************************************************************         
DR055    ICM   R4,15,AROUND        Rounding / Decimal ?                         
         BZ    DR060                                                            
         OI    6(R4),FVOXMT        Re-transmit                                  
         MVC   8(1,R4),APOPNPRN    Character "("                                
         MVC   9(1,R4),PRO.RPFRND                                               
         TM    RCLOPT,RCLPCT       Is it a percentage column ?                  
         BZ    *+10                No                                           
         MVC   9(1,R4),PRO.RPFPCTS                                              
         MVI   10(R4),C' '                                                      
         MVC   11(1,R4),APCLSPRN   Character ")"                                
*                                                                               
         ICM   RF,1,RCLDCMLS                                                    
         BZ    DR060                                                            
         MVC   8(4,R4),SPACES                                                   
         BCTR  RF,0                Adjust by one                                
         CLI   RCLDCMLS,5          Is it set ?                                  
         BL    DR056                                                            
         SR    RF,RF                                                            
         BCTR  RF,0                Get -1 in RF                                 
         ICM   RF,1,RCLDCMLS                                                    
         LPR   RF,RF                                                            
         MVI   9(R4),C'-'          Minus sign                                   
*                                                                               
DR056    CVD   RF,APDUB                                                         
         OI    APDUB+7,X'0F'                                                    
         UNPK  10(1,R4),APDUB                                                   
                                                                                
***********************************************************************         
*        Minus sign notation                                                    
***********************************************************************         
DR060    ICM   R4,15,AMINUS        Minus notation ?                             
         BZ    DR065                                                            
         OI    6(R4),FVOXMT            Re-transmit                              
         MVC   8(1,R4),APOPNPRN        Character "("                            
         MVC   9(1,R4),APNO            Set to None                              
         MVC   10(1,R4),APCLSPRN       Character ")"                            
         TM    PRO.RPFEDOPT,RPFEDTRL   Trailing minus ?                         
         BZ    *+8                     Yes                                      
         MVI   9(R4),C'T'              Trailing (default)                       
                                                                                
         TM    PRO.RPFEDOPT,RPFEDBKT   Bracket minus ?                          
         BZ    *+8                     No                                       
         MVI   9(R4),C'B'              Yes                                      
                                                                                
         TM    PRO.RPFEDOPT,RPFEDLED   Leading minus ?                          
         BZ    *+8                     No                                       
         MVI   9(R4),C'L'              Yes                                      
                                                                                
         TM    PRO.RPFEDOPT,RPFEDCR    CR or DR sign ?                          
         BZ    *+8                     No                                       
         MVI   9(R4),C'S'              Yes, Sign                                
*                                                                               
DR062    TM    RCLEDOPT,RCLEDTRL+RCLEDBKT+RCLEDLED+RCLEDCR                      
         BZ    DR065               Option not on                                
         MVC   8(3,R4),SPACES                                                   
         TM    RCLEDOPT,RCLEDTRL                                                
         BZ    *+8                                                              
         MVI   9(R4),C'T'          Trailing                                     
         TM    RCLEDOPT,RCLEDBKT                                                
         BZ    *+8                                                              
         MVI   9(R4),C'B'          Bracketed                                    
         TM    RCLEDOPT,RCLEDLED                                                
         BZ    *+8                                                              
         MVI   9(R4),C'L'          Leading                                      
         TM    RCLEDOPT,RCLEDCR                                                 
         BZ    *+8                                                              
         MVI   9(R4),C'S'          Signed                                       
         TM    RCLEDOPT,RCLEDABS                                                
         BNO   *+10                                                             
         MVC   9(1,R4),APNO        Absolute value ?                             
                                                                                
***********************************************************************         
*        Comma                                                                  
***********************************************************************         
DR065    ICM   R4,15,ACOMMA        Commas ?                                     
         BZ    DR070                                                            
         OI    6(R4),FVOXMT        Re-transmit                                  
         MVC   8(1,R4),APOPNPRN    Character "("                                
         MVC   9(1,R4),APNO        Default to no                                
         MVC   10(1,R4),APCLSPRN   Character ")"                                
         TM    PRO.RPFEDOPT,RPFEDCMA   Commas ?                                 
         BZ    *+10                    No                                       
         MVC   9(1,R4),APYES           Yes                                      
         TM    RCLEDOPT,RCLEDCMY+RCLEDCMN                                       
         BZ    DR070                                                            
         MVC   8(3,R4),SPACES                                                   
         MVC   9(1,R4),APYES                                                    
         TM    RCLEDOPT,RCLEDCMY                                                
         BO    DR070                                                            
         MVC   9(1,R4),APNO                                                     
                                                                                
***********************************************************************         
*        Underline totals                                                       
***********************************************************************         
DR070    ICM   R4,15,AUNDER        Under-line ?                                 
         BZ    DR075                                                            
         OI    6(R4),FVOXMT        Re-transmit                                  
         MVC   8(1,R4),APOPNPRN    Character "("                                
         MVI   9(R4),C'B'          Underline below total (default)              
         MVC   10(1,R4),APCLSPRN   Character ")"                                
*                                                                               
         TM    PRO.RPFPOPT3,RPFDBUL   Double underline ?                        
         BZ    *+8                    No                                        
         MVI   9(R4),C'D'             Yes                                       
*                                                                               
         TM    PRO.RPFPOPT3,RPFNOUL   No underlining ?                          
         BZ    *+10                                                             
         MVC   9(1,R4),APNO           No                                        
*                                                                               
         TM    PRO.RPFPOPT3,RPFTPUL   Underline above ?                         
         BZ    DR072                                                            
         MVI   9(R4),C'A'             Above                                     
         TM    PRO.RPFPOPT3,RPFDBUL   Double underline ?                        
         BZ    DR072                  No                                        
         MVI   9(R4),C'E'             Yes                                       
*                                                                               
DR072    TM    RCLOPT3,RCLUNLNA+RCLUNLNB+RCLUNLNN                               
         BZ    DR075                                                            
         MVC   8(3,R4),SPACES                                                   
         MVC   9(1,R4),APNO                                                     
         TM    RCLOPT3,RCLUNLNN    No underline ?                               
         BO    DR075                                                            
         TM    RCLOPT3,RCLUNLNA    Above underline ?                            
         BZ    DR074                                                            
         MVI   9(R4),C'A'                                                       
         TM    RCLOPT3,RCLUNLDB    Double underline ?                           
         BZ    DR074                                                            
         MVI   9(R4),C'E'          Double above underline                       
*                                                                               
DR074    TM    RCLOPT3,RCLUNLNB    Below underline ?                            
         BZ    DR075                                                            
         MVI   9(R4),C'B'                                                       
         TM    RCLOPT3,RCLUNLDB    Double underline ?                           
         BZ    DR075                                                            
         MVI   9(R4),C'D'          Double above underline                       
                                                                                
***********************************************************************         
*        Print zero amounts                                                     
***********************************************************************         
DR075    ICM   R4,15,AZEROA        Zero amount ?                                
         BZ    DR080                                                            
         OI    6(R4),FVOXMT        Re-transmit                                  
         MVC   8(1,R4),APOPNPRN    Character "("                                
         MVC   9(1,R4),APNO        Don't print zero for zero amts               
         MVC   10(1,R4),APCLSPRN       Character ")"                            
         TM    PRO.RPFEDOPT,RPFEDZRO   Print zero amounts ?                     
         BZ    *+10                                                             
         MVC   9(1,R4),APYES       Print zero for zero amts                     
*                                                                               
         TM    RCLEDOPT,RCLEDZRY+RCLEDZRN                                       
         BZ    DR080                                                            
         MVC   8(3,R4),SPACES                                                   
         TM    RCLEDOPT,RCLEDZRY                                                
         BZ    *+10                                                             
         MVC   9(1,R4),APYES       Print zero for zero amts                     
         TM    RCLEDOPT,RCLEDZRN                                                
         BZ    *+10                                                             
         MVC   9(1,R4),APNO        Don't print zero for zero amts               
                                                                                
***********************************************************************         
*        Print zero totals                                                      
***********************************************************************         
DR080    ICM   R4,15,AZEROT        Zero totals ?                                
         BZ    DR085                                                            
         OI    6(R4),FVOXMT        Re-transmit                                  
         MVC   8(1,R4),APOPNPRN    Character "("                                
         MVC   9(1,R4),APNO        Don't print zero for zero totals             
         MVC   10(1,R4),APCLSPRN       Character ")"                            
         TM    PRO.RPFPOPT,RPFZEROT    Print zero totals ?                      
         BZ    *+10                                                             
         MVC   9(1,R4),APYES       Print zero for zero totals                   
*                                                                               
         TM    RCLOPT4,RCLZRTTY+RCLZRTTN                                        
         BZ    DR085                                                            
         MVC   8(3,R4),SPACES                                                   
         TM    RCLOPT4,RCLZRTTY                                                 
         BZ    *+10                                                             
         MVC   9(1,R4),APYES       Print zero for zero totals                   
         TM    RCLOPT4,RCLZRTTN                                                 
         BZ    *+10                                                             
         MVC   9(1,R4),APNO        Don't print zero for zero totals             
                                                                                
***********************************************************************         
*        Right justify data                                                     
***********************************************************************         
DR085    ICM   R4,15,ARJUST        Right justify ?                              
         BZ    DR100                                                            
         OI    6(R4),FVOXMT        Re-transmit                                  
         MVC   8(1,R4),APNO                                                     
         TM    RCLOPT4,RCLRJUST                                                 
         BZ    DR100                                                            
         MVC   8(1,R4),APYES                                                    
                                                                                
***********************************************************************         
*        Process row properties                                                 
***********************************************************************         
         USING RRWELD,R3                                                        
DR100    CLI   0(R3),RRWELQ        Row    element ?                             
         BNE   DR200                                                            
         MVC   PRPRNG(2),=C'1-'                                                 
         SR    RF,RF                                                            
         IC    RF,LASTROW#                                                      
         CVD   RF,APDUB                                                         
         OI    APDUB+7,X'0F'                                                    
         UNPK  PRPRNG+2(2),APDUB                                                
         CLI   PRPRNG+2,C'0'                                                    
         BH    DR013                                                            
         MVC   PRPRNG+2(1),PRPRNG+3                                             
         MVI   PRPRNG+3,C' '                                                    
*                                                                               
         SR    RF,RF                                                            
         IC    RF,RRWDATLN         Length of keyword                            
         BCTR  RF,0                                                             
         EXMVC RF,PRPKYW,RRWNDATA                                               
         LA    RE,RRWNDATA+1(RF)                                                
                                                                                
***********************************************************************         
*        Row prefix                                                             
***********************************************************************         
         ICM   R4,15,APRFIX                                                     
         BZ    DR105                                                            
         OI    6(R4),FVOXMT        Re-transmit                                  
         IC    RF,0(,R4)           Length of field                              
         SHI   RF,17                                                            
         EXMVC RF,8(R4),SPACES     Clear field                                  
         ICM   RF,1,RRWPFXLN       Length of prefix                             
         BZ    DR105                                                            
         BCTR  RF,0                                                             
         EXMVC RF,8(R4),0(RE)                                                   
                                                                                
***********************************************************************         
*        Row type                                                               
***********************************************************************         
DR105    ICM   R4,15,ARTYPE                                                     
         BZ    DR110                                                            
         OI    6(R4),FVOXMT        Re-transmit                                  
         MVC   8(1,R4),RRWTYPE                                                  
         CLI   RRWTYPE,C' '                                                     
         BH    *+8                                                              
         MVI   8(R4),RRWLHEAD      Default is left heading                      
         TM    RRWOPT2,RRWHIDE                                                  
         BZ    *+10                                                             
         MVC   8(1,R4),APNO                                                     
                                                                                
***********************************************************************         
*        Total option                                                           
***********************************************************************         
DR110    ICM   R4,15,ATOTAL        Row total                                    
         BZ    DR115                                                            
         OI    6(R4),FVOXMT        Re-transmit                                  
         MVC   8(1,R4),APNO        Default not to total                         
         TM    RRWOPT,RRWTOT       Total on row ?                               
         BZ    DR115               No                                           
         MVC   8(1,R4),APYES       Set to total                                 
         TM    RRWOPT,RRWTOTSP     Seperate page on total                       
         BZ    *+8                                                              
         MVI   8(R4),C'S'                                                       
         TM    RRWOPT2,RRWBTM      Total at bottom of page                      
         BZ    *+8                                                              
         MVI   8(R4),C'B'                                                       
                                                                                
***********************************************************************         
*        Total for over-ride                                                    
***********************************************************************         
DR115    ICM   R4,15,ATOTFR        Total for                                    
         BZ    DR120                                                            
         OI    6(R4),FVOXMT        Re-transmit                                  
                                                                                
***********************************************************************         
*        Date format                                                            
***********************************************************************         
                                                                                
         USING DTFORMD,RE                                                       
DR120    ICM   R4,15,ADATES                                                     
         BZ    DR200                                                            
         OI    6(R4),FVOXMT        Re-Transmit                                  
         L     RE,=A(DATETAB)                                                   
         A     RE,APRELO                                                        
         MVC   8(L'DTFNAME,R4),DTFNAME                                          
*                                                                               
         CLI   RRWDTEFM,0                                                       
         BE    DR200                                                            
DR121    CLI   DTFLANG,EOT         End of table ?                               
         BNE   *+6                                                              
         DC    H'00'                                                            
*                                                                               
         CLC   DTFLANG,CULANG                                                   
         BNE   DR122                                                            
         CLC   DTFCODE,RRWDTEFM                                                 
         BE    DR124                                                            
*                                                                               
DR122    LA    RE,DTFLNQ(,RE)                                                   
         B     DR121                                                            
*                                                                               
DR124    MVC   8(L'DTFNAME,R4),DTFNAME                                          
         DROP  RE                                                               
                                                                                
**********************************************************************          
*        Handle row or column user field special                     *          
**********************************************************************          
DR200    LA    R2,IOKEY                                                         
         MVI   RESKSEQ,RESKS5TH    Make this record 5                           
         GOTO1 AIO,IORD+IOACCFIL+IO3                                            
         BE    DR205                                                            
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     DRXIT                                                            
*                                                                               
DR205    MVI   SVTYPE,CONTROW      Default row type                             
         CLI   INREC,RECROWPR      Row profile ?                                
         BE    *+8                                                              
         MVI   SVTYPE,CONTCOL      Switch to column profile                     
         L     R2,AIOAREA3                                                      
*                                                                               
         USING CONELD,R3                                                        
         ICM   R4,15,AKYWUSF       User field                                   
         BZ    DR300                                                            
         LR    R3,R2                                                            
         AH    R3,DATADISP                                                      
DR220    CLI   0(R3),EOR            ?                                           
         BE    DR300                                                            
         CLI   0(R3),CONELQ        X'??'                                        
         BNE   DR222                                                            
         CLC   CONTYPE,SVTYPE      Match on row or col type                     
         BNE   DR222                                                            
         CLC   CONNUM,SEQNUM       Match on row or col sequence                 
         BE    DR225                                                            
*                                                                               
DR222    SR    RF,RF                                                            
         IC    RF,1(,R3)                                                        
         AR    R3,RF                                                            
         B     DR220                                                            
*                                                                               
DR225    TM    1(R4),FVAXTND       Extended field header ?                      
         BO    DR230                                                            
*                                                                               
DR228    SR    RF,RF                                                            
         IC    RF,0(,R4)           Get field length                             
         AR    R4,RF                                                            
         B     DR225                                                            
*                                                                               
         USING TWAXTHDR,RE                                                      
DR230    SR    RF,RF                                                            
         IC    RF,0(,R4)                                                        
         SHI   RF,8                                                             
         LA    RE,0(R4,RF)         Point to extended header                     
         CLI   TWAXTFD#,FLDPFKLN   Is it a PF key field ?                       
         BNE   *+6                 Have too many elements to display            
         DC    H'00'                                                            
*                                                                               
         OI    6(R4),FVOXMT                                                     
         CLI   TWAXTUSR+1,FLDFRCDE     FROM field                               
         BNE   DR228               No, so get next                              
         IC    RF,CONFRLN                                                       
         BCTR  RF,0                                                             
         EXMVC RF,8(R4),CONFROM                                                 
*                                                                               
         IC    RF,0(,R4)           Next field                                   
         AR    R4,RF                                                            
         IC    RF,0(,R4)           Get past C'='                                
         AR    R4,RF                                                            
         IC    RF,0(,R4)           Length of TO field                           
         SHI   RF,8                                                             
         LA    RE,0(R4,RF)                                                      
         CLI   TWAXTUSR+1,FLDTOCDE     TO field                                 
         BE    *+6                                                              
         DC    H'00'               Unexpected field                             
         DROP  RE                                                               
*                                                                               
         OI    6(R4),FVOXMT                                                     
         SR    RF,RF                                                            
         IC    RF,CONFRLN          get from legnth                              
         LA    R6,CONFROM(RF)      Point to end of FROM                         
         ICM   RF,1,CONTOLN        Any thing to convert to ?                    
         BZ    DR240               No                                           
         TM    CONIND,CONMACRO     Keyword macro                                
         BZ    DR238                                                            
         GOTO1 XPAND,APPARM,((RF),(R6))                                         
         L     R6,0(,R1)                                                        
         SR    RF,RF                                                            
         IC    RF,0(,R1)                                                        
*                                                                               
DR238    BCTR  RF,0                                                             
         EXMVC RF,8(R4),0(R6)                                                   
*                                                                               
DR240    SR    RF,RF                                                            
         IC    RF,0(,R4)                                                        
         AR    R4,RF                                                            
         B     DR222                                                            
*                                                                               
DR300    ICM   R4,15,AKYWBTY       BTLTYC keyword display                       
         BZ    DRXIT                                                            
                                                                                
         LR    R3,R2                                                            
         AH    R3,DATADISP                                                      
DR310    CLI   0(R3),EOR           End of record ?                              
         BE    DR400                                                            
         CLI   0(R3),CONELQ        X'??'                                        
         BNE   DR312                                                            
         CLC   CONTYPE,SVTYPE      Match on row or col type                     
         BNE   DR312                                                            
         CLC   CONNUM,SEQNUM       Match on row or col sequence                 
         BE    DR315                                                            
*                                                                               
DR312    SR    RF,RF                                                            
         IC    RF,1(,R3)                                                        
         AR    R3,RF                                                            
         B     DR310                                                            
*                                                                               
DR315    L     R4,AKYWBTY          Re-load                                      
*                                                                               
         USING TWAXTHDR,RF                                                      
DR318    TM    1(R4),FVAXTND       Extended field header ?                      
         BZ    DR320                                                            
         SR    RF,RF                                                            
         IC    RF,0(,R4)           length of field                              
         AR    RF,R4                                                            
         SHI   RF,8                                                             
         CLI   TWAXTFD#,FLDPFKLN   PF Key line                                  
         BNE   *+6                                                              
         DC    H'00'               Should alway find a match                    
*                                                                               
         CLC   CONFROM(1),TWAXTUSR+1                                            
         BNE   DR320               Try next                                     
         DROP  RF                                                               
*                                                                               
         SR    RF,RF                                                            
         IC    RF,CONTOLN                                                       
         LA    RE,CONFROM+1                                                     
         BCTR  RF,0                                                             
         EXMVC RF,8(R4),0(RE)                                                   
         B     DR312               Next element                                 
*                                                                               
DR320    SR    RF,RF                                                            
         IC    RF,0(,R4)                                                        
         AR    R4,RF                                                            
         B     DR318                                                            
*                                                                               
DR400    DS    0H                                                               
*                                                                               
DRXIT    OI    ACINDS,ACINOHLP                                                  
         CLI   APACTN,ACTCHA                                                    
         BE    EXIT                                                             
*        LA    RE,COLNUMH          PLACE CURSOR ON COLUMN NUMBER                
*        ST    RE,APCURSOR                                                      
*        SR    RE,RE               SET CONCODE TO YES                           
         B     EXIT                                                             
         DROP  R2,R3                                                            
         EJECT ,                                                                
IVALIPUT LHI   R1,-(X'FFFF'-FVFNOTV+1)     Invalid Input                        
         B     ERRXIT                                                           
                                                                                
IVALCOLN LHI   R1,ACEIVCN          Invalid column number                        
         B     ERRXIT                                                           
                                                                                
IVALRCN  LA    R1,PRPNUMH                                                       
         ST    R1,APCURSOR                                                      
         LHI   R1,ACEIVCN          Invalid column number                        
         CLI   INREC,RECCOLPR      Column profile ?                             
         BE    ERRXIT                                                           
         LHI   R1,1415             Invalid row number                           
         B     ERRXIT                                                           
                                                                                
IVALSORT LHI   R1,ACEIVSS          Invalid sort sequence                        
         B     ERRXIT                                                           
                                                                                
IVALDUPL LHI   R1,ACEDUSN          Duplicate sort sequence                      
         B     ERRXIT                                                           
                                                                                
IVALITOT LHI   R1,ACEIVTO          Invalid total option                         
         B     ERRXIT                                                           
                                                                                
IVALSTCK LHI   R1,ACWHSUC                                                       
         MVI   FVOMTYP,C'W'        Warning                                      
         B     ERRXIT                                                           
                                                                                
IVALHEAD LHI   R1,ACEIVHD                                                       
         B     ERRXIT                                                           
                                                                                
IVALADR  LHI   R1,-(X'FFFF'-FVFNOTV+1)     Invalid Input                        
         B     ERRXIT                                                           
                                                                                
IVALWIDE LHI   R1,ACERTWD          Report too wide                              
         B     ERRXIT                                                           
                                                                                
*VALEKEY MVI   STSEQ,1                                                          
IVALEKEY LHI   R1,-(X'FFFF'-FVFEKEY+1)                                          
         B     ERRXIT                                                           
                                                                                
IVALNUM  LHI   R1,-(X'FFFF'-FVFNOTN+1)     Not numeric                          
         B     ERRXIT                                                           
                                                                                
ERRXIT   STCM  R1,3,FVMSGNO                                                     
         CLC   FVMSGNO,=AL2(FVFOK) SET CONCODE                                  
         B     XIT                                                              
         EJECT ,                                                                
         LTORG                                                                  
         EJECT ,                                                                
***********************************************************************         
*  Create table of screen elements                                    *         
***********************************************************************         
         SPACE 1                                                                
         USING FIELDD,R2                                                        
INISCRN  NTR1  BASE=*,LABEL=*                                                   
         XC    ASCREEN,ASCREEN     Screen table                                 
         XC    ACURELEM,ACURELEM   X'C2' or X'C3' (Row / Col)                   
         XC    ASTYELEM,ASTYELEM   X'25' element                                
         XC    COLARRAY,COLARRAY                                                
         MVI   MIDLINE#,0                                                       
         MVI   MAXSORT#,0                                                       
         MVI   LASTROW#,0                                                       
         MVI   LASTCOL#,0                                                       
         MVI   ALTLEN,0                                                         
*        MVI   SEQNUM,1            Set for 1st row or column                    
         MVI   BYTE,RRWELQ         Get row                                      
*        CLI   CURCOL#,0           Any row or column specified                  
*        BE    *+10                No                                           
*        MVC   SEQNUM,CURCOL#      Yes so use this row or column                
*                                                                               
         L     R2,=A(SCRROW1)      Load default screen for rows                 
         A     R2,APRELO                                                        
         CLI   INREC,RECROWPR      Row properties ?                             
         BE    INIT005             Yes                                          
         L     R2,=A(SCRNON$)      No, load default screen for columns          
         A     R2,APRELO                                                        
         MVI   BYTE,RCLELQ         Get column                                   
*                                                                               
INIT005  SR    RF,RF                                                            
         L     R3,AIOAREA1         A(Format record)                             
         AH    R3,DATADISP                                                      
*                                                                               
INIT010  CLI   0(R3),EOR           End Of Record ?                              
         BE    INIT020                                                          
*                                                                               
         USING STYELD,R3                                                        
         CLI   0(R3),STYELQ        X'25' element                                
         BNE   INIT011                                                          
         ST    R3,ASTYELEM         Save off address                             
         CLI   STYLN,STYLNQ2                                                    
         BE    *+6                                                              
         DC    H'00'               Should have been converted                   
         DROP  R3                                                               
*                                                                               
         USING RRWELD,R3                                                        
INIT011  CLI   0(R3),RRWELQ        X'C2', Row element ?                         
         BNE   INIT016             No, check column element                     
         CLC   SEQNUM,RRWSEQ                                                    
         BNE   INIT012                                                          
         CLI   INREC,RECROWPR      Row properties ?                             
         BNE   INIT012                                                          
         ST    R3,ACURELEM         Save off address of element                  
*                                                                               
INIT012  MVC   LASTROW#,RRWSEQ     Row number                                   
         CLI   RRWTYPE,RRWMID      is it a mid-line ?                           
         BNE   INIT016                                                          
         CLI   MIDLINE#,0                                                       
         BNE   INIT016                                                          
         MVC   MIDLINE#,RRWSEQ     Move in first occurance                      
         DROP  R3                                                               
*                                                                               
         USING RCLELD,R3                                                        
INIT016  CLI   0(R3),RCLELQ        X'C3', Column element ?                      
         BNE   INIT018                                                          
         CLC   SEQNUM,RCLSEQ       Match on column number                       
         BNE   INIT017                                                          
         CLI   INREC,RECCOLPR      Row properties ?                             
         BNE   INIT017                                                          
         ST    R3,ACURELEM         Save off address of element                  
*                                                                               
INIT017  MVC   LASTCOL#,RCLSEQ     Column number                                
         ICM   RF,1,RCLSORTN       Get sort column number                       
         BZ    INIT018             No over-ride                                 
         CLC   MAXSORT#,RCLSORTN   Which is higher ?                            
         BNL   *+10                                                             
         MVC   MAXSORT#,RCLSORTN   Save highest number                          
         LA    RE,COLARRAY-1(RF)                                                
         MVC   0(1,RE),RCLSEQ                                                   
*                                                                               
INIT018  IC    RF,1(,R3)                                                        
         AR    R3,RF                                                            
         B     INIT010                                                          
         DROP  R3                                                               
*                                                                               
INIT020  CLI   MIDLINE#,0                                                       
         BNE   *+10                                                             
         MVC   MIDLINE#,LASTROW#                                                
*                                                                               
         ICM   R3,15,ACURELEM      Found match for element ?                    
         BZ    INIT100             No                                           
*                                                                               
         USING RCLELD,R3                                                        
         CLI   INREC,RECROWPR      Row properties ?                             
         BE    INIT050             Yes                                          
         TM    RCLOPT,RCLACCM      Amount keyword ?                             
         BZ    INIT030             No                                           
         L     R2,=A(SCRCOL$)      Yes, column Screen for Amounts               
         A     R2,APRELO                                                        
         ST    R2,ASCREEN          Set to amount properties                     
         B     INIT400                                                          
*                                                                               
INIT030  MVI   REPMODE,REPCOL      Set for column processing                    
         SR    R1,R1               Build to get keyword in table                
         IC    R1,RCLDATLN         Length of data                               
         STC   R1,FAKEFLDH+5                                                    
         BCTR  R1,0                                                             
         EXMVC R1,FAKEFLD,RCLNDATA                                              
         GOTO1 VALDEF,FAKEFLDH     Find keyword                                 
         BNE   INIT100             Yes                                          
         ST    R1,AKEYWORD                                                      
         B     INIT060             No, use defaults table                       
         DROP  R3                                                               
*                                                                               
         USING RRWELD,R3                                                        
INIT050  MVI   REPMODE,REPROW      Set for row processing                       
         SR    R1,R1               Build to get keyword in table                
         IC    R1,RRWDATLN         Length of data                               
         STC   R1,FAKEFLDH+5                                                    
         BCTR  R1,0                                                             
         EXMVC R1,FAKEFLD,RRWNDATA                                              
         GOTO1 VALDEF,FAKEFLDH     Find keyword                                 
         BNE   INIT100             No, use defaults                             
         ST    R1,AKEYWORD                                                      
         DROP  R3                                                               
*                                                                               
         USING DEFTABD,R1                                                       
         USING SCRKYWD,R4                                                       
INIT060  L     R4,=A(SCRKYW)       Special keywords                             
         A     R4,APRELO                                                        
         TM    DEFTYPE,DEFDTE1                                                  
         BZ    INIT062                                                          
         L     R2,=A(SCRCDTE)      Date type keyword (Column)                   
         CLI   INREC,RECROWPR      Row properties ?                             
         BNE   *+8                                                              
         L     R2,=A(SCRRDTE)      Date type keyword (Row)                      
         A     R2,APRELO                                                        
*                                                                               
INIT062  CLI   0(R4),X'FF'         End of table ?                               
         BE    INIT100             Not a special keyword                        
         CLC   DEFDDNUM,0(R4)      Match on dictionary value                    
         BE    INIT065             Yes                                          
         LA    R4,SCRKYWQ(,R4)     No, check next                               
         B     INIT062                                                          
         DROP  R1                                                               
*                                                                               
INIT065  SR    RE,RE                                                            
         ICM   RE,3,SCRKADR        Special keyword table                        
         MVC   ALTLEN,SCRALT                                                    
         A     RE,APBASE1          RB off of T60C1B                             
         ST    RE,ASCREEN          Save new extended screen                     
         CLI   INREC,RECROWPR      Row properties ?                             
         BE    INIT100             Yes                                          
         L     R2,=A(SCRNON$2)     Non $ screen w/replace                       
         A     R2,APRELO                                                        
         DROP  R4                                                               
*                                                                               
INIT100  LA    R6,SCRBLOCK         Build Screen table here                      
INIT105  CLI   0(R2),EOT           End of table                                 
         BE    INIT120             Yes, no more to move for default             
         MVC   0(FLDLNQ,R6),0(R2)  Move table components to block               
         LA    R2,FLDLNQ(,R2)      Next entry to copy                           
         LA    R6,FLDLNQ(,R6)      Next location to copy to                     
         B     INIT105             Again                                        
*                                                                               
INIT120  ICM   R2,15,ASCREEN       Extended Screen table set ?                  
         BZ    INIT200             No, must have appened already                
         XC    ASCREEN,ASCREEN     Yes, so append to 1st table                  
         B     INIT105             Append these fields to end                   
*                                                                               
INIT200  L     R2,=A(SCRPFK)                                                    
         A     R2,APRELO                                                        
*                                                                               
INIT210  CLI   0(R2),EOT                                                        
         BE    INIT300                                                          
         MVC   0(FLDLNQ,R6),0(R2)                                               
         LA    R2,FLDLNQ(,R2)      Next entry to copy                           
         LA    R6,FLDLNQ(,R6)      Next location to copy to                     
         B     INIT210                                                          
*                                                                               
INIT300  MVI   0(R6),EOT                                                        
         LA    R6,SCRBLOCK                                                      
         ST    R6,ASCREEN                                                       
*                                                                               
INIT400  CLI   0(R3),EOR           End of record                                
         BE    INITXIT                                                          
         CLI   0(R3),RPFELQ        X'C4' Profile element                        
         BE    INIT420                                                          
         SR    RF,RF                                                            
         IC    RF,1(,R3)                                                        
         AR    R3,RF                                                            
         B     INIT400                                                          
*                                                                               
INIT420  SR    RF,RF                                                            
         IC    RF,1(,R3)           Element length                               
         BCTR  RF,0                                                             
         EXMVC RF,PROFELEM,0(R3)                                                
*                                                                               
INITXIT  XIT1                                                                   
***********************************************************************         
*  SET DYNAMIC SCREEN                                                 *         
***********************************************************************         
         SPACE 1                                                                
         USING FIELDD,R2                                                        
         USING TWAELEMD,R4                                                      
PARM     USING TWAPARMD,APPARM                                                  
                                                                                
BLDSCRN  NTR1  BASE=*,LABEL=*                                                   
         ICM   R2,15,ASCREEN       A(Built Screen table)                        
         BZ    BLDSCRX                                                          
         XC    APPARM(24),APPARM                                                
         XC    TWAELEM,TWAELEM                                                  
         ST    R5,PARM.TWAPATWA    Parm 1  (R5=ATWA)                            
         L     R4,AIOAREA3                                                      
         ST    R4,PARM.TWAPAFST    Parm 2  (AIOAREA3)                           
         LA    R3,PRPTAGH                                                       
         SR    RF,RF                                                            
         IC    RF,PRPTAGH                                                       
         AR    R3,RF                                                            
         ST    R3,PARM.TWAPAOUT    Parm 3  (First field)                        
         LR    RE,R4                                                            
         LA    RF,2000                                                          
         XCEF                                                                   
*                                                                               
BLDSCR20 CLI   0(R2),EOT           End of table ?                               
         BE    BLDSCR80            Yes                                          
*                                                                               
         CLI   FLDLLEN,0           Length of label field                        
         BE    BLDSCR30                                                         
         XC    0(TWAELLQ3+80,R4),0(R4)                                          
         MVI   TWAELCD,1                                                        
         MVC   TWAERLN,FLDROW      Relative row number                          
         TM    FLDOPT,FLDFROW      Fixed row                                    
         BZ    *+8                 No, relative row                             
         OI    TWAERLN,TWAERLAB    Yes, absolute row line                       
         MVC   TWAECOL,FLDCOL      Current column position                      
         MVC   TWAEFLN,FLDLLEN     Length of label                              
         OI    TWAEATB,FVAPROT+FVALOW                                           
         TM    FLDOPT,FLDNDICT     Non-data dictionary label ?                  
         BZ    BLDSCR26                                                         
         SR    RF,RF                                                            
         IC    RF,FLDLLEN          Get length of field                          
         BCTR  RF,0                                                             
         SR    RE,RE                                                            
         ICM   RE,3,FLDDD#         Get displacement to label table              
         L     R0,=A(LBLTAB)                                                    
         A     R0,APRELO                                                        
         AR    RE,R0                                                            
         EXMVC RF,TWAEDTA,0(RE)    Move in data to display                      
         AHI   RF,1                Add one back                                 
         B     BLDSCR28                                                         
*                                                                               
BLDSCR26 LA    RF,4                                                             
         CLI   TWAEFLN,1           Is length 1                                  
         BE    BLDSCR28            Yes, can not be dictionary entry             
         MVI   TWAEDTA,ESCLEFTJ                                                 
         MVC   TWAEDTA+1(2),FLDDD#                                              
         MVC   TWAEDTA+3(1),FLDLLEN                                             
         CLI   TWAEFLN,3           Field less than 3                            
         BH    BLDSCR28            Yes                                          
         LA    RF,3                Force length to 3                            
         MVI   TWAEDTA,ESCLEN3     Length 3 field                               
         BE    BLDSCR28                                                         
         MVI   TWAEDTA,ESCLEN2     Length 2 field                               
*&&DO                                                                           
BLDSCR28 LA    RE,TWAELLNQ                                                      
         TM    TWAERLN,TWAERLXA    Atribute length                              
         BZ    *+8                                                              
         LA    RE,TWAELLNQ+TWAELLQ2                                             
         TM    TWAERLN,TWAERLXT    extra info length                            
         BZ    *+8                                                              
         LA    RE,TWAELLNQ+TWAELLQ3                                             
         AR    RF,RE                                                            
*&&                                                                             
BLDSCR28 LA    RF,TWAELLNQ(,RF)                                                 
         STC   RF,TWAELLN          Length of element                            
         AR    R4,RF                                                            
*                                                                               
BLDSCR30 CLI   FLDILEN,0           Length of input field                        
         BE    BLDSCR50            No input, just label                         
*                                                                               
         XC    0(TWAELLQ3+80,R4),0(R4)                                          
         MVI   TWAELCD,1                                                        
         IC    RF,FLDLLEN          Length of label                              
         AHI   RF,1                Space to seperate label and input            
         CLI   FLDLLEN,0           Was there a lable field ?                    
         BNE   BLDSCR32            Yes                                          
         MVC   TWAERLN,FLDROW      No, so set now instead                       
         TM    FLDOPT,FLDFROW      Fixed row                                    
         BZ    *+8                 No, relative row                             
         OI    TWAERLN,TWAERLAB    Yes, absolute row line                       
*                                                                               
         SR    RF,RF                                                            
BLDSCR32 SR    RE,RE                                                            
         IC    RE,FLDCOL           Start of column                              
         AR    RF,RE               RF = Start of input column                   
         STC   RF,TWAECOL          Current column position                      
         MVC   TWAEFLN,FLDILEN     Length of input field                        
         TM    FLDOPT,FLDALT       Use alternate length                         
         BZ    BLDSCR35                                                         
         CLI   ALTLEN,0            Must have a value                            
         BE    BLDSCR35                                                         
         MVC   TWAEFLN,ALTLEN                                                   
*                                                                               
BLDSCR35 TM    FLDOPT,FLDLOW       Mix case allowed ?                           
         BZ    *+8                 No                                           
         OI    TWAEATB,FVALOW      Yes                                          
         TM    FLDOPT,FLDPROT      Force protected field                        
         BZ    *+8                                                              
         OI    TWAEATB,FVAPROT                                                  
         TM    FLDOPT,FLDSET#      Force to set field number too                
         BZ    *+10                                                             
         MVC   TWAEFLD,FLDNUM                                                   
         OI    TWAERLN,TWAERLXT    eXTra data                                   
         MVC   TWAEXUSR+1(1),FLDNUM                                             
         IC    RF,TWAEFLN                                                       
*        IC    RF,FLDILEN                                                       
         LA    RF,TWAELLNQ+TWAELLQ3(,RF)                                        
         STC   RF,TWAELLN                                                       
         AR    R4,RF                                                            
*                                                                               
BLDSCR50 LA    R2,FLDLNQ(,R2)                                                   
         B     BLDSCR20                                                         
*                                                                               
BLDSCR80 DS    0H                                                               
         GOTO1 VTWABLD,APPARM                                                   
         CLI   PARM.TWAPERRS,0                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   PARM.TWAPAOUT,PARM.TWAPANXT                                      
*                                                                               
BLDSCRX  XIT1                                                                   
         DROP  R2,R4                                                            
         DROP  PARM                                                             
         EJECT ,                                                                
***********************************************************************         
*  Create table of screen elements                                    *         
***********************************************************************         
         SPACE 1                                                                
         USING FIELDD,R2                                                        
SETADRS  NTR1  BASE=*,LABEL=*                                                   
         L     R2,ASCREEN                                                       
         LA    R3,PRPTAGH                                                       
SETADR10 CLI   0(R2),EOT                                                        
         BE    SETADRX                                                          
         CLI   FLDILEN,0           Only fields with extended headers            
         BE    SETADR30            Yes                                          
*                                                                               
SETADR18 SR    RF,RF                                                            
         TM    1(R3),FVAXTND       Extended field header ?                      
         BO    SETADR20            Yes                                          
         ICM   RF,1,0(R3)          Bump to first field                          
         BNZ   *+6                                                              
         DC    H'00'               End of screen                                
*                                                                               
         AR    R3,RF               Next field                                   
         B     SETADR18                                                         
*                                                                               
         USING TWAXTHDR,RE                                                      
SETADR20 LR    RE,R3                                                            
         IC    RF,0(,R3)                                                        
         SHI   RF,8                                                             
         AR    RE,RF               Point to extended field header               
*        CLC   FLDNUM,TWAXTFD#     Match field number                           
         CLC   FLDNUM,TWAXTUSR+1                                                
         BE    *+6                                                              
         DC    H'00'               Should match                                 
         DROP  RE                                                               
*                                                                               
         SR    RE,RE                                                            
         ICM   RE,3,FLDADR                                                      
         BZ    SETADR25            If zero, no address                          
         AR    RE,RC               RC = LWSD                                    
         ST    R3,0(,RE)           Save field address                           
*                                                                               
SETADR25 SR    RF,RF                                                            
         IC    RF,0(,R3)           Bump to next field                           
         AR    R3,RF               Next field                                   
*                                                                               
SETADR30 LA    R2,FLDLNQ(,R2)      Next entry in table                          
         B     SETADR10            Get next                                     
*                                                                               
SETADRX  XIT1                                                                   
         EJECT ,                                                                
***********************************************************************         
*  FIELD LENGTH EQUATES                                               *         
***********************************************************************         
         SPACE 1                                                                
LENFFULL EQU   62                  FULL    LINE                                 
LENFHALF EQU   23                  PARTIAL LINE                                 
LENFFCUR EQU   4                   FOREIGN CURRENCY                             
LENFWCTP EQU   12                  WORK    CODE TYPE                            
LENFFLTR EQU   11                  FILTER  LIST                                 
LENFTIMT EQU   5                   TYPE    OF   TIME                            
LENFONE  EQU   1                   ONE                                          
LENFMTHD EQU   3                   METHOD                                       
*                                                                               
LBUDG#   EQU   5                   MAXIMUM LENGTH FOR NUMERIC BUDGET #          
         EJECT ,                                                                
***********************************************************************         
*        Validation table for Round / Decimals column option          *         
***********************************************************************         
DECMTAB  DS    0C                                                               
         DC    CL2'4 ',XL1'05'                                                  
DECMLEN  EQU   *-DECMTAB                                                        
         DC    CL2'3 ',XL1'04'                                                  
         DC    CL2'2 ',XL1'03'                                                  
         DC    CL2'P ',XL1'03'     Pennies                                      
         DC    CL2'1 ',XL1'02'                                                  
         DC    CL2'0 ',XL1'01'                                                  
         DC    CL2'D ',XL1'01'     Dollars                                      
         DC    CL2'-1',XL1'FF'                                                  
         DC    CL2'-2',XL1'FE'                                                  
         DC    CL2'H ',XL1'FE'     Hunders                                      
         DC    CL2'-3',XL1'FD'                                                  
         DC    CL2'T ',XL1'FD'     Thousands                                    
         DC    CL2'-4',XL1'FC'                                                  
         DC    CL2'-5',XL1'FB'                                                  
         DC    CL2'-6',XL1'FA'                                                  
         DC    CL2'M ',XL1'FA'     Milions                                      
         DC    AL1(EOT)                                                         
         EJECT ,                                                                
***********************************************************************         
*        Keywords with special screens                                *         
***********************************************************************         
SCRKYW   DS    0H                                                               
*        DC    AL2(AC#RSSUS,SCRSTT-T60C1B),AL1(0)                               
         DC    AL2(AC#RSUSF,SCRUSF-T60C1B),AL1(0)                               
         DC    AL2(AC#RSUSC,SCRUSF-T60C1B),AL1(0)                               
         DC    AL2(AC#RSBTY,SCRBTY-T60C1B),AL1(11)                              
         DC    AL2(AC#BILTC,SCRBTY-T60C1B),AL1(11)                              
         DC    X'FF'                                                            
         EJECT ,                                                                
***********************************************************************         
*        General screen properties                                    *         
***********************************************************************         
SCRCOL$  DS    0H                  Col - Amount Screen                          
         DC    AL1(FLDWIDTH,0,01,02,17,02),AL2(AC#WIDTH,AWIDTH-LWSD)            
         DC    AL1(FLDHEAD1,FLDLOW,01,02,17,12)                                 
         DC    AL2(AC#HEAD1,AHEAD1-LWSD)                                        
         DC    AL1(FLDHEAD2,FLDLOW,01,02,17,12)                                 
         DC    AL2(AC#HEAD2,AHEAD2-LWSD)                                        
         DC    AL1(FLDPRINT,0,01,02,17,01),AL2(AC#PRINT,APRINT-LWSD)            
         DC    AL1(FLDRPEAT,0,01,02,17,01),AL2(AC#REPET,AREPET-LWSD)            
         DC    AL1(FLDCTOTL,0,01,02,17,01),AL2(AC#TOTAL,ATOTAL-LWSD)            
         DC    AL1(FLDSTACK,0,01,02,17,03),AL2(AC#STACK,ASTACK-LWSD)            
         DC    AL1(FLDROUND,0,01,02,17,04),AL2(AC#ROUND,AROUND-LWSD)            
         DC    AL1(FLDMINUS,0,01,02,17,03),AL2(AC#NEG,AMINUS-LWSD)              
         DC    AL1(FLDCOMMA,0,01,02,17,03),AL2(AC#COMMA,ACOMMA-LWSD)            
         DC    AL1(FLDUNDER,0,01,02,17,03),AL2(AC#UNDLN,AUNDER-LWSD)            
         DC    AL1(FLDZRAMT,0,01,02,17,03),AL2(AC#PRZAM,AZEROA-LWSD)            
         DC    AL1(FLDZRTOT,0,01,02,17,03),AL2(AC#PRZTO,AZEROT-LWSD)            
         DC    AL1(FLDPFKLN,FLDFROW+FLDPROT+FLDSET#,23,02,0,78)                 
         DC    AL2(0,0)                                                         
         DC    AL1(FLDPFKLN,FLDPROT+FLDSET#,01,03,0,77),AL2(0,0)                
         DC    AL1(EOT)                                                         
                                                                                
SCRNON$  DS    0H                  Col - Non-Amount Screen                      
         DC    AL1(FLDWIDTH,0,01,02,17,02),AL2(AC#WIDTH,AWIDTH-LWSD)            
         DC    AL1(FLDHEAD1,FLDLOW,01,02,17,12)                                 
         DC    AL2(AC#HEAD1,AHEAD1-LWSD)                                        
         DC    AL1(FLDHEAD2,FLDLOW,01,02,17,12)                                 
         DC    AL2(AC#HEAD2,AHEAD2-LWSD)                                        
         DC    AL1(FLDCSORT,0,01,02,17,02),AL2(AC#SRTOR,ACSORT-LWSD)            
         DC    AL1(FLDPRINT,0,01,02,17,01),AL2(AC#PRINT,APRINT-LWSD)            
         DC    AL1(FLDCTOTL,0,01,02,17,01),AL2(AC#TOTAL,ATOTAL-LWSD)            
         DC    AL1(FLDSTACK,0,01,02,17,03),AL2(AC#STACK,ASTACK-LWSD)            
         DC    AL1(FLDRDANT,0,01,02,17,03),AL2(AC#REDUN,AREDUN-LWSD)            
         DC    AL1(FLDRIGHT,0,01,02,17,01),AL2(AC#RJUST,ARJUST-LWSD)            
         DC    AL1(EOT)                                                         
                                                                                
SCRNON$2 DS    0H                                                               
         DC    AL1(FLDHEAD1,FLDLOW,01,02,17,12)                                 
         DC    AL2(AC#HEAD1,AHEAD1-LWSD)                                        
         DC    AL1(FLDWIDTH,0,00,41,17,02),AL2(AC#WIDTH,AWIDTH-LWSD)            
         DC    AL1(FLDHEAD2,FLDLOW,01,02,17,12)                                 
         DC    AL2(AC#HEAD2,AHEAD2-LWSD)                                        
         DC    AL1(FLDPRINT,0,00,41,17,01),AL2(AC#PRINT,APRINT-LWSD)            
         DC    AL1(FLDCSORT,0,01,02,17,02),AL2(AC#SRTOR,ACSORT-LWSD)            
         DC    AL1(FLDCTOTL,0,00,41,17,01),AL2(AC#TOTAL,ATOTAL-LWSD)            
         DC    AL1(FLDSTACK,0,01,02,17,03),AL2(AC#STACK,ASTACK-LWSD)            
         DC    AL1(FLDRDANT,0,00,41,17,03),AL2(AC#REDUN,AREDUN-LWSD)            
         DC    AL1(FLDRIGHT,0,01,02,17,01),AL2(AC#RJUST,ARJUST-LWSD)            
         DC    AL1(EOT)                                                         
                                                                                
SCRCDTE  DS    0H                  Col - Date type keyword                      
         DC    AL1(FLDWIDTH,0,01,02,17,02),AL2(AC#WIDTH,AWIDTH-LWSD)            
         DC    AL1(FLDHEAD1,FLDLOW,01,02,17,12)                                 
         DC    AL2(AC#HEAD1,AHEAD1-LWSD)                                        
         DC    AL1(FLDHEAD2,FLDLOW,01,02,17,12)                                 
         DC    AL2(AC#HEAD2,AHEAD2-LWSD)                                        
         DC    AL1(FLDDATES,0,01,02,17,10),AL2(AC#DTFMT,ADATES-LWSD)            
         DC    AL1(FLDCSORT,0,01,02,17,02),AL2(AC#SRTOR,ACSORT-LWSD)            
         DC    AL1(FLDPRINT,0,01,02,17,01),AL2(AC#PRINT,APRINT-LWSD)            
         DC    AL1(FLDCTOTL,0,01,02,17,01),AL2(AC#TOTAL,ATOTAL-LWSD)            
         DC    AL1(FLDSTACK,0,01,02,17,03),AL2(AC#STACK,ASTACK-LWSD)            
         DC    AL1(FLDRDANT,0,01,02,17,03),AL2(AC#REDUN,AREDUN-LWSD)            
         DC    AL1(FLDRIGHT,0,01,02,17,01),AL2(AC#RJUST,ARJUST-LWSD)            
         DC    AL1(EOT)                                                         
                                                                                
SCRROW1  DS    0H                  Row - Screen                                 
         DC    AL1(FLDRTYPE,0,01,02,12,01),AL2(AC#TYPE1,ARTYPE-LWSD)            
         DC    AL1(FLDRTOTL,0,01,02,12,01),AL2(AC#TOTAL,ATOTAL-LWSD)            
         DC    AL1(FLDPRFIX,FLDLOW,01,02,12,14)                                 
         DC    AL2(AC#PREFX,APRFIX-LWSD)                                        
         DC    AL1(FLDTOTFR,0,01,02,12,36),AL2(AC#TOTFR,ATOTFR-LWSD)            
         DC    AL1(EOT)                                                         
                                                                                
SCRRDTE  DS    0H                  Row - Screen with date type                  
         DC    AL1(FLDRTYPE,0,01,02,12,01),AL2(AC#TYPE1,ARTYPE-LWSD)            
         DC    AL1(FLDRTOTL,0,01,02,12,01),AL2(AC#TOTAL,ATOTAL-LWSD)            
         DC    AL1(FLDDATES,0,01,02,12,10),AL2(AC#DTFMT,ADATES-LWSD)            
         DC    AL1(FLDPRFIX,FLDLOW,01,02,12,14)                                 
         DC    AL2(AC#PREFX,APRFIX-LWSD)                                        
         DC    AL1(FLDTOTFR,0,01,02,12,36),AL2(AC#TOTFR,ATOTFR-LWSD)            
         DC    AL1(EOT)                                                         
                                                                                
SCRUSF   DS    0H                  UF keyword special screen                    
         DC    AL1(FLDLABEL,0,02,02,17,00),AL2(AC#VALUE,0)                      
         DC    AL1(FLDLABEL,0,00,41,17,00),AL2(AC#RPLC,0)                       
         DC    AL1(FLDFRCDE,0,01,02,00,32),AL2(0,AKYWUSF-LWSD)                  
         DC    AL1(FLDTOCDE,FLDNDICT,00,41,02,32),AL2(LBLEQUL-LBLTAB,0)         
         DC    AL1(FLDFRCDE,0,01,02,00,32),AL2(0,0)                             
         DC    AL1(FLDTOCDE,FLDNDICT,00,41,02,32),AL2(LBLEQUL-LBLTAB,0)         
         DC    AL1(FLDFRCDE,0,01,02,00,32),AL2(0,0)                             
         DC    AL1(FLDTOCDE,FLDNDICT,00,41,02,32),AL2(LBLEQUL-LBLTAB,0)         
         DC    AL1(FLDFRCDE,0,01,02,00,32),AL2(0,0)                             
         DC    AL1(FLDTOCDE,FLDNDICT,00,41,02,32),AL2(LBLEQUL-LBLTAB,0)         
         DC    AL1(FLDFRCDE,0,01,02,00,32),AL2(0,0)                             
         DC    AL1(FLDTOCDE,FLDNDICT,00,41,02,32),AL2(LBLEQUL-LBLTAB,0)         
         DC    AL1(FLDFRCDE,0,01,02,00,32),AL2(0,0)                             
         DC    AL1(FLDTOCDE,FLDNDICT,00,41,02,32),AL2(LBLEQUL-LBLTAB,0)         
         DC    AL1(EOT)                                                         
*                                                                               
*              Keywords BT, BILTYC or TBT                                       
SCRBTY   DS    0C                                                               
         DC    AL1(FLDBAUTO,FLDALT,02,02,12,20)                                 
         DC    AL2(AC#RSATB,AKYWBTY-LWSD)                                       
*                                                                               
         DC    AL1(FLDBCLI,FLDALT,01,02,12,20),AL2(AC#RSCLB,0)                  
         DC    AL1(FLDBGRP,FLDALT,01,02,12,20),AL2(AC#GRPBL,0)                  
         DC    AL1(FLDBPRGS,FLDALT,01,02,12,20),AL2(AC#RSPGB,0)                 
         DC    AL1(FLDBTOTL,FLDALT,01,02,12,20),AL2(AC#RSTLB,0)                 
         DC    AL1(FLDBEST,FLDALT,01,02,12,20),AL2(AC#RSESB,0)                  
         DC    AL1(FLDBMANL,FLDALT,01,02,12,20),AL2(AC#RSMNB,0)                 
         DC    AL1(FLDBSPCL,FLDALT,01,02,12,20),AL2(AC#RSSPB,0)                 
         DC    AL1(FLDBUNBL,FLDALT,01,02,12,20),AL2(AC#RSUNB,0)                 
         DC    AL1(FLDBONE,FLDALT,01,02,12,20),AL2(AC#RS1BL,0)                  
         DC    AL1(EOT)                                                         
*                                                                               
SCRPFK   DC    AL1(FLDPFKLN,FLDFROW+FLDPROT+FLDSET#,23,02,0,78)                 
         DC    AL2(0,0)                                                         
         DC    AL1(FLDPFKLN,FLDPROT+FLDSET#,01,03,0,77)                         
         DC    AL2(0,0)                                                         
         DC    AL1(EOT)                                                         
         EJECT ,                                                                
DATETAB  DC    AL1(LANGEUS,00),CL10'MMMDD/YY'     DATCON  8 ( 9)                
         DC    AL1(LANGEUS,01),CL10'YYMMDD'               0                     
         DC    AL1(LANGEUS,20),CL10'YYYYMMDD'            20                     
         DC    AL1(LANGEUS,21),CL10'MMMDD/YYYY'          21                     
         DC    AL1(LANGEUS,23),CL10'YYYY-MM-DD'          23                     
         DC    AL1(LANGEUS,10),CL10'MM/DD/YY'            10                     
         DC    AL1(LANGEUS,30),CL10'DDMMMYY'           NONE                     
         DC    AL1(LANGEUS,31),CL10'MM/DD/YYYY'        NONE                     
         DC    AL1(LANGEUS,32),CL10'DDMMYYYY'          NONE                     
         DC    AL1(LANGEUS,33),CL10'MMDDYY'            NONE                     
         DC    AL1(LANGEUS,34),CL10'MMDDYYYY'          NONE                     
*                                                                               
         DC    AL1(LANGEUK,00),CL10'DDMMMYY'      DATCON  8                     
         DC    AL1(LANGEUK,01),CL10'YYMMDD'               0                     
         DC    AL1(LANGEUK,20),CL10'YYYYMMDD'            20                     
         DC    AL1(LANGEUK,21),CL10'DDMMMYYYY'           21                     
         DC    AL1(LANGEUK,23),CL10'YYYY-MM-DD'          23                     
         DC    AL1(LANGEUK,10),CL10'DD/MM/YY'            10                     
         DC    AL1(LANGEUK,11),CL10'MMMDD/YY'            11                     
         DC    AL1(LANGEUK,05),CL10'DD MMM YY'            5                     
         DC    AL1(LANGEUK,13),CL10'DD.MM.YY'            13                     
*                                                                               
         DC    AL1(LANGGER,00),CL10'DD.MM.YY'     DATCON  8                     
         DC    AL1(LANGGER,01),CL10'YYMMDD'               0                     
         DC    AL1(LANGGER,20),CL10'YYYYMMDD'            20                     
         DC    AL1(LANGGER,21),CL10'DD.MM.YYYY'          21                     
         DC    AL1(LANGGER,23),CL10'YYYY-MM-DD'          23                     
         DC    AL1(LANGGER,10),CL10'DD/MM/YY'            10                     
         DC    AL1(LANGGER,11),CL10'MMMDD/YY'            11                     
         DC    AL1(EOT)                                                         
         EJECT ,                                                                
LBLTAB   DS    0H                                                               
LBLEQUL  DC    C'=>'                                                            
         EJECT ,                                                                
ESCLEN2  EQU   X'20'               Data dictionary len=2                        
ESCLEN3  EQU   X'21'               Data dictionary len=3                        
ESCLEFTJ EQU   X'22'               Data dictionary left justify                 
FVALOW   EQU   X'40'                                                            
*                                                                               
FLDHEAD1 EQU   1                                                                
FLDHEAD2 EQU   2                                                                
FLDWIDTH EQU   3                                                                
FLDPRINT EQU   4                                                                
FLDCTOTL EQU   5                                                                
FLDSTACK EQU   6                                                                
FLDCSORT EQU   7                                                                
FLDROUND EQU   8                                                                
FLDRPEAT EQU   9                                                                
FLDRDANT EQU   10                                                               
FLDMINUS EQU   11                                                               
FLDCOMMA EQU   12                                                               
FLDUNDER EQU   13                                                               
FLDZRAMT EQU   14                                                               
FLDZRTOT EQU   15                                                               
FLDDATES EQU   16                                                               
FLDRIGHT EQU   17                  Right justify                                
FLDFRCDE EQU   40                                                               
FLDTOCDE EQU   41                                                               
FLDTONME EQU   41                                                               
FLDBAUTO EQU   C'A'                                                             
FLDBCLI  EQU   C'C'                                                             
FLDBGRP  EQU   C'G'                                                             
FLDBPRGS EQU   C'P'                                                             
FLDBTOTL EQU   C'T'                                                             
FLDBEST  EQU   C'E'                                                             
FLDBMANL EQU   C'M'                                                             
FLDBSPCL EQU   C'S'                                                             
FLDBUNBL EQU   C'U'                                                             
FLDBONE  EQU   C'1'                                                             
FLDPFKLN EQU   254                                                              
FLDLABEL EQU   255                                                              
*                                                                               
FLDRTYPE EQU   1                                                                
FLDRTOTL EQU   2                                                                
FLDPRFIX EQU   3                                                                
FLDTOTFR EQU   4                                                                
         EJECT ,                                                                
***********************************************************************         
*  DSECT FOR LOCAL WORKING STORAGE                                    *         
***********************************************************************         
         SPACE 1                                                                
LWSD     DSECT                                                                  
ASCREEN  DS    A                   Current Screen table                         
SVRE     DS    A                   Save registar RE                             
AKEYWORD DS    A                   A(Keyword from DEFTAB ACSCR01 CORE)          
ASTYELEM DS    A                   STYELQ element address                       
ACURELEM DS    A                   Current row or column element                
*                                                                               
ABLOCK   DS    0A                                                               
AWIDTH   DS    A                   Column width                                 
AHEAD1   DS    A                   Heading 1                                    
AHEAD2   DS    A                   Heading 2                                    
APRINT   DS    A                   Print                                        
ATOTAL   DS    A                   Total                                        
ASTACK   DS    A                   Stack                                        
AROUND   DS    A                   Round or decimals                            
AREPET   DS    A                   Repeat constant                              
ADATES   DS    A                   Date type                                    
AMINUS   DS    A                   Negative notation                            
ACOMMA   DS    A                   Print commas                                 
AUNDER   DS    A                   Underline                                    
AZEROA   DS    A                   Zero amount                                  
AZEROT   DS    A                   Zero total                                   
AREDUN   DS    A                   Redundant                                    
ARJUST   DS    A                   Right Justify                                
ACSORT   DS    A                   Column sort                                  
ARTYPE   DS    A                   Row type                                     
APRFIX   DS    A                   Row prefix                                   
ATOTFR   DS    A                   Row total for                                
AKYWUSF  DS    A                   UF           keyword                         
AKYWBTY  DS    A                   BT or BILTYC keyword                         
ABLOCKQ  EQU   *-ABLOCK                                                         
*                                                                               
MAXPARM  EQU   33                                                               
*                                                                               
LCINDS   DS    XL1                 Local indincation for read                   
LCADDREC EQU   X'80'               .   OK to add    record                      
LCCHAREC EQU   X'20'               .   OK to change record                      
LCADDELM EQU   X'08'               .   Added element                            
LCDELELM EQU   X'04'               .   Deleted element                          
*                                                                               
ELEMENT  DS    CL(L'APELEM)                                                     
BLOCK    DS    (MAXPARM)CL32       DATA BLOCK FOR SCANNER                       
SVKEY    DS    CL(L'IOKEY)         SAVE IOKEY                                   
SVELEM   DS    CL(L'APELEM)        SAVE APELEM                                  
SVTYPE   DS    XL1                 Save off type                                
EXTFLD#  DS    XL1                 Extended field number                        
CHECKUL  DS    CL1                 WILDCARD TO CHECK U/L                        
LDGLISTN DS    XL1                 # OF UNIT/LEDGER IN LIST                     
LDGLIST  DS    CL40                UNIT/LEDGER LIST                             
LASTROW# DS    XL1                 Last row    number                           
LASTCOL# DS    XL1                 Last column number                           
MAXSORT# DS    XL1                 Highest sort # used                          
MIDLINE# DS    XL1                 1st row midline number                       
ALTLEN   DS    XL1                 Alternate input length                       
COLARRAY DS    CL(MAXCOLS)                                                      
FAKEFLDH DS    XL8                                                              
FAKEFLD  DS    CL12                                                             
BYTE     DS    CL1                 One byte work area                           
SEQNUM   DS    XL1                 Column or Row number                         
CURSEQ#  DS    XL1                                                              
*                                                                               
XLATIND  DS    XL1                 Translate indicator                          
XLATACTV EQU   X'80'               .   Keyword found and active                 
XLATKYWD EQU   X'40'               .   Keyword processed                        
*                                                                               
XPANDFLD DS    0CL40                                                            
XLATEFLD DS    CL40                                                             
PROFELEM DS    XL(RPFLN2Q)         Saved off profile element                    
SCRBLOCK DS    XL(60*FLDLNQ+1)     Upto 60 fields on Screen                     
LWSX     DS    0C                                                               
         EJECT ,                                                                
ROUTD    DSECT                                                                  
ROUTFLD  DS    CL1                 FIELD NUMBER                                 
ROUTVAL  DS    AL1                 VALIDATION ROUTINE DISPLACEMENT              
ROUTV01  EQU   1                     ACCOUNT                                    
ROUTV02  EQU   2                     CONTRA                                     
ROUTV03  EQU   3                     OFFICE                                     
ROUTV04  EQU   4                     BUDGET                                     
ROUTV05  EQU   5                     TRANSACTION TYPE                           
ROUTV06  EQU   6                     WORKCODE TYPE                              
ROUTV07  EQU   7                     TYPE OF TIME                               
ROUTV08  EQU   8                     COSTING ACCOUNT                            
ROUTV09  EQU   9                     WORKCODE/TASK                              
ROUTV10  EQU   10                    GENERAL ACCOUNT                            
ROUTV11  EQU   11                    BILLING OR REVENUE                         
ROUTV12  EQU   12                    STATUS - YES/NO                            
ROUTV13  EQU   13                    FILTERS                                    
ROUTV14  EQU   14                    BILLING SOURCE                             
ROUTV15  EQU   15                    FOREIGN CURRENCY                           
ROUTV16  EQU   16                    METHOD TYPE                                
                                                                                
ROUTDIS  DS    AL1                 DISPLAY ROUTINE DISPLACEMENT                 
ROUTD01  EQU   1                     GENERAL          DISPLAY                   
ROUTD02  EQU   2                     BUDGET           DISPLAY                   
ROUTD03  EQU   3                     TRANSACTION TYPE DISPAY                    
ROUTD04  EQU   4                     STATUS           DISPLAY                   
ROUTD05  EQU   5                     METHOD      TYPE DISPLAY                   
ROUTLNQ  EQU   *-ROUTD                                                          
                                                                                
SCRKYWD  DSECT                                                                  
SCRDD#   DS    AL2                                                              
SCRKADR  DS    AL2                                                              
SCRALT   DS    AL1                 Alternate length                             
SCRKYWQ  EQU   *-SCRKYWD                                                        
                                                                                
FIELDD   DSECT                                                                  
FLDNUM   DS    AL1                 Field number                                 
FLDOPT   DS    AL1                 Field options                                
FLDLOW   EQU   X'40'               .  Allow mix case in input field             
FLDPROT  EQU   X'20'               .  Force to protected field                  
FLDALT   EQU   X'10'               .  Use alternate input length                
FLDSET#  EQU   X'04'               .  Set field number                          
FLDFROW  EQU   X'02'               .  Fix row in FLDROW                         
FLDNDICT EQU   X'01'               .  Non-dictionary value                      
FLDROW   DS    AL1                 Relative row                                 
FLDCOL   DS    AL1                 Column start                                 
FLDLLEN  DS    AL1                 Data Dictionary length (Label)               
FLDILEN  DS    AL1                 Input field length                           
FLDDD#   DS    AL2                 Data Dictionary number                       
FLDADR   DS    AL2                 Displacement to store A(FIELD)               
FLDLNQ   EQU   *-FIELDD                                                         
                                                                                
DTFORMD  DSECT                                                                  
DTFLANG  DS    AL1                                                              
DTFCODE  DS    AL1                                                              
DTFNAME  DS    CL10                                                             
DTFLNQ   EQU   *-DTFORMD                                                        
         EJECT ,                                                                
*ACSCRWRK                                                                       
       ++INCLUDE ACSCRWRK                                                       
         EJECT ,                                                                
TWAD     DSECT                                                                  
         ORG   SCROVLYH                                                         
       ++INCLUDE ACSCRD1D                                                       
         SPACE 3                                                                
*DDTWABLDD                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDTWABLDD                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'094ACSCR1B   09/02/15'                                      
         END                                                                    
