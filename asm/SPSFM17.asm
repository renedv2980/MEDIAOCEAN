*          DATA SET SPSFM17    AT LEVEL 005 AS OF 05/26/04                      
*PHASE T21717A,*                                                                
***********************************************************************         
*                                                                     *         
*  TITLE        T21717 - NEW SID RECORD TREND MAINTENANCE             *         
*                                                                     *         
*  CALLED FROM  GENCON VIA T21700 (SFM CONTROLLER)                    *         
*                                                                     *         
*  COMMENTS     SUPPORTS DISPLAY                                      *         
*                                                                     *         
*  INPUTS       SCREEN T217B7 (MAINTENANCE)                           *         
*                                                                     *         
*  OUTPUTS      UPDATED SID DETAIL RECORDS                            *         
*               UPDATED SID COMPETITION RECORDS                       *         
*                                                                     *         
*  REGISTERS    R0 -- WORK                                            *         
*               R1 -- WORK                                            *         
*               R2 -- WORK                                            *         
*               R3 -- WORK                                            *         
*               R4 -- WORK                                            *         
*               R5 -- FIRST SYSD BASE                                 *         
*               R6 -- SECOND SYSD BASE                                *         
*               R7 -- SECOND BASE                                     *         
*               R8 -- WORK                                            *         
*               R9 -- WORK                                            *         
*               RA -- TWA                                             *         
*               RB -- FIRST BASE                                      *         
*               RC -- GEND                                            *         
*               RD -- SYSTEM                                          *         
*               RE -- SYSTEM                                          *         
*               RF -- SYSTEM                                          *         
*                                                                     *         
*  I/O AREAS    IO1 - COMPETITION RECORD                              *         
*               IO2 - SID DETAIL RECORD                               *         
*                                                                     *         
***********************************************************************         
         TITLE 'T21717 - NEW SID RECORD TREND MAINTENANCE'                      
T21717   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T21717                                                         
*                                                                               
         LA    R7,2048(RB)                                                      
         LA    R7,2048(R7)                                                      
         LA    R6,2048(R7)                                                      
         LA    R6,2048(R6)                                                      
         USING T21717,RB,R7,R6                                                  
*                                                                               
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R5,ASYSD                                                         
         USING SYSD,R5                                                          
         ST    RD,SAVERD                                                        
*                                                                               
         MVI   IOOPT,C'N'          I/O FOR GENCON                               
         CLC   CONACT(3),=CL3'SEL'                                              
         BE    BSE050                                                           
         MVI   IOOPT,C'Y'          NO I/O FOR GENCON                            
*                                                                               
BSE050   CLI   MODE,VALKEY         VALIDATE DETAIL KEY                          
         BNE   LOOP50                                                           
         BAS   RE,VK                                                            
         B     STEXIT                                                           
*                                                                               
LOOP50   CLI   MODE,DISPREC        VALIDATE DETAIL RECORD                       
         BE    LOOP70                                                           
         CLI   MODE,VALREC         VALIDATE DETAIL RECORD                       
         BNE   STEXIT                                                           
*                                                                               
LOOP70   BAS   RE,FSTRPT                                                        
         BAS   RE,DISREC                                                        
         B     STEXIT                                                           
*                                                                               
STEXIT   XC    SRTMSG,SRTMSG                                                    
         CLI   NDETSW,X'FF'                                                     
         BNE   STEX10                                                           
         MVC   SRTMSG(45),=C'** WARNING - THIS SCREEN WILL NOT BE SAVEDX        
                **'                                                             
STEX10   OI    SRTMSGH+6,X'80'                                                  
         OI    SRTMSGH+1,X'08'                                                  
         CLC   SVTRESTA,=3X'00'                                                 
         BE    STEX20                                                           
         OI    SRTBK1H+6,X'40'                                                  
         B     STEX40                                                           
STEX20   OI    SRTCSTH+6,X'40'                                                  
STEX40   CLC   CONACT(3),=CL3'SEL'                                              
         BNE   XIT                                                              
         MVC   CONHEAD(39),=C'RECORD DISPLAYED-NOW ENTER NEXT REQUEST'          
         OI    CONHEADH+6,X'80'    XMIT                                         
         GOTO1 SAVEUWKA            SAVE SYSD                                    
XIT      XIT1                                                                   
* VALIDATE KEY                                                                  
*                                                                               
VK       NTR1                                                                   
*                                                                               
         MVI   RECSW,0   SO THIS FLAG DOES NOT GET STUCK ON X'FF' & DIE         
         CLC   CONACT(3),=CL3'SEL'                                              
         BNE   VK03                                                             
         BAS   RE,SETHEAD                                                       
*                                                                               
*--TEST SCREEN FIELDS FOR CHANGE                                                
VK03     TM    SRTSTAH+FVIIND-FVIHDR,FVIVAL                                     
         BZ    VK05                                                             
         TM    SRTDPTH+FVIIND-FVIHDR,FVIVAL                                     
         BZ    VK05                                                             
         TM    SRTSRCH+FVIIND-FVIHDR,FVIVAL                                     
         BZ    VK05                                                             
         TM    SRTDAYH+FVIIND-FVIHDR,FVIVAL                                     
         BZ    VK05                                                             
         TM    SRTTIMEH+FVIIND-FVIHDR,FVIVAL                                    
         BZ    VK05                                                             
         TM    SRTYRH+FVIIND-FVIHDR,FVIVAL                                      
         BZ    VK05                                                             
         TM    SRTPERH+FVIIND-FVIHDR,FVIVAL                                     
         BZ    VK05                                                             
         TM    SRTSCHH+FVIIND-FVIHDR,FVIVAL                                     
         BZ    VK05                                                             
         TM    SRTMNUH+FVIIND-FVIHDR,FVIVAL                                     
         BO    VK08                                                             
*                                                                               
VK05     LA    RE,SYSSPARE+1024    CLEAR APPLICATION STORAGE                    
         LH    RF,=Y(SYSDEND-SYSSPARE-1024)                                     
         MVC   FULL,SAVERD                                                      
         XCEF                                                                   
         MVC   SAVERD,FULL                                                      
*                                                                               
         MVI   TWADEM,FF           INITIALIZE STATION POINTER                   
*                                                                               
         L     R1,ACOMFACS                                                      
         USING COMFACSD,R1                                                      
         MVC   VDEMAND,CDEMAND                                                  
         MVC   VDEMOVAL,CDEMOVAL                                                
         DROP  R1                                                               
*                                                                               
         XC    DMCB(8),DMCB                                                     
         MVC   DMCB+4(4),=X'D9000A71'                                           
         GOTO1 CALLOV,DMCB                                                      
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   VEDITOR,DMCB                                                     
*                                                                               
         XC    DMCB(8),DMCB                                                     
         MVC   DMCB+4(4),=X'D9000A22'                                           
         GOTO1 CALLOV,DMCB                                                      
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   VSPDEMUP,DMCB                                                    
*                                                                               
         XC    DMCB(8),DMCB                                                     
         MVC   DMCB+4(4),=X'D9000A21'                                           
         GOTO1 CALLOV,DMCB                                                      
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   VSPDEMLK,DMCB                                                    
*                                                                               
VK08     XC    SVUPFILE,SVUPFILE   CLEAR SAVED VALUES                           
         XC    SVUPGRD,SVUPGRD                                                  
         XC    SVUPFRBK,SVUPFRBK                                                
         XC    SVUPINP,SVUPINP                                                  
*                                                                               
         LA    R9,SAVEKEY          PUT KEY IN SAVEKEY, THEN MOVE TO KEY         
         USING SIRKEY,R9                                                        
         XC    SAVEKEY,SAVEKEY                                                  
         MVI   SIRKTYPE,SIRKTYPQ   SIR RECORD TYPE                              
*                                                                               
         XC    ELEM,ELEM                                                        
         LA    R2,ELEM                                                          
         MVC   0(9,R2),=XL9'0900000000010000E3'                                 
         GOTO1 VALIMED             FAKE THE MEDIA FIELD TO 'T'                  
         MVC   SIRKAM,BAGYMD                                                    
*                                                                               
         LA    R2,SRTSTAH          MARKET/STATION                               
         CLI   5(R2),0                                                          
         BNE   *+12                                                             
         MVI   ERROR,MISSING                                                    
         B     TRAPERR                                                          
*                                                                               
         GOTO1 VALISTA                                                          
         CLC   SVTRESTA,=3X'00'                                                 
         BE    VK09                                                             
         MVC   BMKTSTA+2(3),FAKSTA                                              
VK09     MVC   SIRKMS,BMKTSTA                                                   
*                                                                               
         LA    R2,SRTDPTH          DAYPART                                      
         CLI   5(R2),0                                                          
         BNE   *+12                                                             
         MVI   ERROR,MISSING                                                    
         B     TRAPERR                                                          
*                                                                               
         MVC   DAYPART,SRTDPT                                                   
         MVC   SIRKDPT,DAYPART                                                  
*                                                                               
         LA    R2,SRTYRH           YEAR                                         
         CLI   5(R2),0                                                          
         BE    VK10                                                             
*                                                                               
         TM    4(R2),X'08'         TEST NUMERIC                                 
         BO    *+12                                                             
         MVI   ERROR,NOTNUM                                                     
         B     TRAPERR                                                          
*                                                                               
         ZIC   R1,5(R2)            GET THE YEAR                                 
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R2)                                                      
         CVB   R3,DUB                                                           
*  YEAR 2000 FIX                                                                
         C     R3,=F'50'                                                        
         BH    *+8                                                              
         A     R3,=F'100'                                                       
*                                                                               
         STC   R3,SIRKYEAR                                                      
         MVC   SCRYEAR,SIRKYEAR                                                 
         XI    SIRKYEAR,X'FF'      YEAR IS IN ONE'S COMPLEMENT FORM             
*                                                                               
VK10     LA    R2,SRTSCHH          SCHEME                                       
         CLI   5(R2),0                                                          
         BNE   VK20                                                             
         MVC   8(3,R2),=C'ALL'                                                  
         OI    6(R2),X'80'         XMIT                                         
         B     VK30                                                             
*                                                                               
VK20     CLC   =C'ALL',8(R2)       TEST SCHEME FOR ENTIRE AGENCY                
         BE    VK30                                                             
         OC    8(3,R2),=XL3'404040'                                             
         GOTO1 CLPACK,DMCB,8(R2),SIRKCODE                                       
         CLI   DMCB,0              TEST VALID SCHEME CODE                       
         BNE   INVSCHE                                                          
*                                                                               
VK30     LA    R9,KEY              CREATE THE SCHEME KEY                        
         XC    KEY,KEY                                                          
         MVC   SIRKEY,SAVEKEY                                                   
         XC    SIRKMS(9),SIRKMS                                                 
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   SIRKEY,KEYSAVE      TEST SCHEME EXISTS                           
         BNE   NOSCHEM                                                          
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         L     R4,AIO                                                           
         MVI   ELCODE,ECLCODEQ     LOOK FOR CLIENT CODE                         
         BAS   RE,GETEL                                                         
         BNE   *+10                                                             
         MVC   SCHCLI(2),2(R4)                                                  
*                                                                               
*--READ SOURCE ELEMENT                                                          
         L     R4,AIO                                                           
         MVI   ELCODE,ERSCODEQ     LOOK FOR DEMO MENU NAME                      
         BAS   RE,GETEL                                                         
         BNE   VK32                                                             
         MVC   DEMOSRC,2(R4)                                                    
         B     VK34                                                             
VK32     MVI   ERROR,MISSING                                                    
         B     TRAPERR                                                          
*                                                                               
VK34     LA    R2,SRTMNUH                                                       
         CLI   5(R2),0                                                          
         BE    VK36                                                             
         MVC   SVDEMNM,8(R2)                                                    
         OC    SVDEMNM,=4X'40'                                                  
         MVC   HLDDMNM,SVDEMNM                                                  
         B     VK39                                                             
*                                                                               
VK36     L     R4,AIO                                                           
         MVI   ELCODE,EDMCODEQ     LOOK FOR DEMO MENU NAME                      
         BAS   RE,GETEL                                                         
         BE    VK37                                                             
         MVC   SVDEMNM,HLDDMNM                                                  
         B     VK38                                                             
VK37     MVC   SVDEMNM,2(R4)                                                    
         OC    SVDEMNM,=4X'40'                                                  
*                                                                               
VK38     LA    R2,SRTMNUH                                                       
         MVI   5(R2),4                                                          
         OI    6(R2),X'80'                                                      
         MVC   SRTMNU,SVDEMNM                                                   
*                                                                               
VK39     L     R4,AIO                                                           
         MVI   ELCODE,EDYCODEQ     LOOK FOR DEFAULT YEAR ELEMENT                
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING EDYELEM,R4                                                       
         MVC   CURYEAR,EDYYEAR     GET THE YEAR                                 
         DROP  R4                                                               
*                                                                               
         LA    R9,SAVEKEY                                                       
         CLI   SIRKYEAR,0          TEST YEAR IS ABSENT                          
         BNE   VK40                NO, WE HAVE ONE                              
         MVC   SIRKYEAR,CURYEAR    USE THE CURRENT YEAR                         
         ZIC   R1,SIRKYEAR                                                      
         CVD   R1,DUB                                                           
         UNPK  SRTYR,DUB                                                        
         OI    SRTYR+1,X'F0'                                                    
         OI    SRTYRH+6,X'80'      XMIT THE CURRENT YEAR                        
         XI    SIRKYEAR,X'FF'      SAVE YEAR IN ONE'S COMPLEMENT FORM           
         B     VK50                                                             
*                                                                               
VK40     ZIC   R1,SIRKYEAR         COMPARE GIVEN YEAR TO CURRENT YEAR           
         X     R1,=F'255'          MAKE YEAR POSITIVE                           
         ZIC   R0,CURYEAR                                                       
         SR    R1,R0                                                            
         LPR   R1,R1               DIFFERENCE IN YEARS                          
         CH    R1,=H'1'            NO GREATER THAN ONE YEAR DIFFERENCE          
         BH    INVYER                                                           
*                                                                               
VK50     L     R4,AIO                                                           
         MVI   ELCODE,EDCCODEQ     LOOK FOR DAYPART CODES                       
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         SR    R0,R0                                                            
         ZIC   R1,1(R4)                                                         
         SH    R1,=H'2'                                                         
         D     R0,=F'8'            R1 = NO. OF DAYPARTS                         
         LA    R4,2(R4)            FIRST DAYPART                                
*                                                                               
VK60     CLC   DAYPART,0(R4)       LOOK FOR THE GIVEN DAYPART                   
         BE    *+16                                                             
         LA    R4,8(R4)            NEXT DAYPART                                 
         BCT   R1,VK60                                                          
         B     NODPT                                                            
*                                                                               
         LA    R3,PRGTYPSV         SAVE PROGTYPE CODE/NAME IN PRGTYPSV          
         XC    PRGTYPSV,PRGTYPSV                                                
         L     R4,AIO                                                           
         MVI   ELCODE,EPCCODEQ                                                  
         BAS   RE,GETEL                                                         
         BNE   VK80                PROGTYP IS OPTIONAL                          
*                                                                               
         USING EPCELEM,R4                                                       
         SR    R0,R0                                                            
         ZIC   R1,EPCLEN                                                        
         SH    R1,=H'2'                                                         
         D     R0,=F'8'            LEAVES NO. OF PROGTYPES IN R1                
*                                                                               
VK70     MVC   0(8,R3),EPCDCODE    CODE+NAME                                    
         LA    R4,8(R4)                                                         
         LA    R3,8(R3)                                                         
         BCT   R1,VK70                                                          
         DROP  R4                                                               
*                                                                               
VK80     L     R4,AIO                                                           
         MVI   ELCODE,EPNCODEQ     PERIOD NAMES ELEMENT                         
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         SR    R0,R0                                                            
         ZIC   R1,1(R4)                                                         
         SH    R1,=H'2'                                                         
         D     R0,=F'5'            R1 = NO. OF PERIODS                          
         LA    R4,2(R4)            FIRST PERIOD NAME                            
*                                                                               
         MVI   PERNUM,0            PERIOD                                       
         LA    R2,SRTPERH                                                       
         CLI   5(R2),0                                                          
         BNE   *+12                                                             
         MVI   ERROR,MISSING                                                    
         B     TRAPERR                                                          
         OC    SRTPER,=C'    '     PAD NAME WITH BLANKS                         
*                                                                               
VK90     CLC   SRTPER,1(R4)        LOOK FOR MATCH OF PERIOD NAME                
         BE    *+16                                                             
         LA    R4,5(R4)                                                         
         BCT   R1,VK90                                                          
         B     INVPER                                                           
*                                                                               
         MVC   PERNUM,0(R4)        SAVE THE PERIOD NUMBER                       
         MVC   SIRKMON,PERNUM                                                   
         OI    SIRKMON,SIRKBUYQ                                                 
*                                                                               
         LA    R9,KEY                                                           
         MVC   SIRKEY,SAVEKEY      BUILD THE PERIOD KEY                         
         XC    SIRKMS,SIRKMS                                                    
         MVI   SIRKDPT,0                                                        
         MVI   SIRKMON,0                                                        
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                LOOK FOR PERIOD KEY                          
         CLC   SIRKEY,KEYSAVE                                                   
         BNE   INVPER                                                           
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         L     R4,AIO                                                           
         MVI   ELCODE,EPDCODEQ     LOOK FOR PERIOD ELEMENT                      
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING EPDELEM,R4                                                       
VK100    CLC   PERNUM,EPDNUM       LOOK FOR THE GIVEN PERIOD                    
         BE    *+16                                                             
         BAS   RE,NEXTEL                                                        
         BE    VK100                                                            
         B     INVPER                                                           
*                                                                               
         MVC   PERBOOKS,EPDBOOKS   DEFAULT BOOKS                                
*                                                                               
         MVI   WORK,C' '           BUILD PERIOD UPGRADE EXPRESSION              
         MVC   WORK+1(L'WORK-1),WORK                                            
         OC    EPDUPFIL(27),EPDUPFIL                                            
         BNZ   *+14                THERE IS A PERIOD DEFAULT UPGRADE            
         MVC   WORK(22),=C'* NO DEFAULT UPGRADE *'                              
         B     VK110                                                            
*                                                                               
         MVC   SVUPFILE(1),EPDUPFIL   STORE UPGRADE EXPRESSION                  
         MVC   SVUPGRD(8),EPDUPGRD                                              
         MVC   SVUPFRBK(2),EPDUPFBK                                             
         MVC   SVUPINP(16),EPDUPINP                                             
*                                                                               
         MVC   BCHUF(1),EPDUPFIL       STORE UPGRADE EXPRESSION                 
         MVC   BCHUP(8),EPDUPGRD                                                
         MVC   BCHFB(2),EPDUPFBK                                                
         MVC   BCHUPIN(16),EPDUPINP                                             
*                                                                               
         MVC   WORK(4),=C'UPX='                                                 
         MVC   WORK+2(1),EPDUPFIL                                               
         MVC   WORK+4(16),EPDUPINP                                              
*                                                                               
         OC    EPDUPFBK,EPDUPFBK   TEST ANY SHARE BOOK                          
         BZ    VK110                                                            
         LA    R3,WORK+21                                                       
         CLI   0(R3),C' '          BACK UP TO NON-BLANK                         
         BNE   *+8                                                              
         BCT   R3,*-8                                                           
         MVI   1(R3),C','          EDIT IN COMMA                                
         LA    R3,2(R3)                                                         
         MVC   0(3,R3),=C'BK='                                                  
         GOTO1 DATCON,DMCB,(3,EPDUPFBK),(6,3(R3))                               
         DROP  R4                                                               
*                                                                               
VK110    MVC   PERUPG,WORK         SAVE THE UPGRADE EXPRESSION                  
         MVI   SIRKYEAR,0          YEAR IS ABSENT IN UPGRADE KEY                
         MVC   SIRKMON,PERNUM      BUT PERIOD IS THERE                          
         OI    SIRKMON,SIRKBUYQ                                                 
         MVC   SIRKMS,BMKTSTA      SO IS MKT/STA                                
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                LOOK FOR STATION DEFAULT UPGRADE KEY         
         CLC   SIRKEY,KEYSAVE                                                   
         BE    VK115               WE HAVE IT                                   
*                                                                               
         MVC   SIRKEY,KEYSAVE      TRY MARKET DEFAULT UPGRADE                   
         XC    SIRKSTA,SIRKSTA                                                  
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   SIRKEY,KEYSAVE      TEST WE HAVE IT                              
         BE    VK115               YES                                          
         MVC   WORK(34),PERUPG     WE DON'T - USE THE PERIOD UPGRADE            
         B     VK130                                                            
*                                                                               
VK115    MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC              GET STATION RECORD                           
         L     R4,AIO                                                           
         MVI   ELCODE,EUPCODEQ     STATION DEFAULT UPGRADE ELEMENT              
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING EUPELEM,R4                                                       
         MVI   WORK,C' '           BUILD STATION UPGRADE EXPRESSION             
         MVC   WORK+1(L'WORK-1),WORK                                            
         MVC   WORK(4),=C'UPX='                                                 
         MVC   WORK+2(1),EUPUPFIL                                               
*                                                                               
         CLI   EUPUPGRD,SPUPTPUT   TEST HUT OR PUT                              
         BNE   VK120               NO, NEITHER ONE                              
         CLI   EUPUPGRD+1,C'P'     TEST PUT                                     
         BNE   VK120               NO, IT'S A HUT                               
*                                                                               
         MVC   WORK(4),=C'PUT/'                                                 
         ZIC   R0,CURYEAR          CURRENT YEAR FROM SCHEME RECORD              
         SR    R1,R1                                                            
         ICM   R1,8,EUPUPGRD+2     RELATIVE PUT BOOK YEAR                       
         SRA   R1,24                                                            
         AR    R1,R0               DETERMINE ABSOLUTE YEAR                      
         STC   R1,EUPUPGRD+2                                                    
         GOTO1 DATCON,DMCB,(3,EUPUPGRD+2),(6,WORK+4)                            
         MVC   WORK+7(2),WORK+8    GET RID OF '/'                               
         MVI   WORK+9,C' '                                                      
         B     *+10                                                             
*                                                                               
VK120    MVC   WORK(16),EUPUPINP                                                
*                                                                               
         MVC   SVUPFILE(1),EUPUPFIL   STORE UPGRADE EXPRESSION                  
         MVC   SVUPGRD(8),EUPUPGRD                                              
         MVC   SVUPINP(16),WORK                                                 
         XC    SVUPFRBK(2),SVUPFRBK                                             
*                                                                               
         MVC   BCHUF(1),EPDUPFIL       STORE UPGRADE EXPRESSION                 
         MVC   BCHUP(8),EPDUPGRD                                                
         MVC   BCHUPIN(16),WORK                                                 
         XC    BCHFB(2),BCHFB                                                   
*                                                                               
         MVC   WORK+4(16),EUPUPINP                                              
         OC    EUPUPFBK,EUPUPFBK   TEST ANY SHARE BOOK                          
         BZ    VK130                                                            
         ZIC   R0,CURYEAR          CURRENT YEAR FROM SCHEME RECORD              
         SR    R1,R1                                                            
         ICM   R1,8,EUPUPFBK       RELATIVE SHARE BOOK YEAR                     
         SRA   R1,24                                                            
         AR    R1,R0               DETERMINE ABSOLUTE YEAR                      
         STC   R1,EUPUPFBK                                                      
         MVC   SVUPFRBK,EUPUPFBK   TEST ANY SHARE BOOK                          
         MVC   BCHFB(2),EPDUPFBK   TEST ANY SHARE BOOK                          
*                                                                               
         LA    R3,WORK+21                                                       
         CLI   0(R3),C' '          BACK UP TO NON-BLANK                         
         BNE   *+8                                                              
         BCT   R3,*-8                                                           
         MVI   1(R3),C','          EDIT IN COMMA                                
         LA    R3,2(R3)                                                         
         MVC   0(3,R3),=C'BK='                                                  
         GOTO1 DATCON,DMCB,(3,EUPUPFBK),(6,3(R3))                               
         DROP  R4                                                               
*                                                                               
VK130    LA    R2,SRTDAYH          DAY                                          
         GOTO1 ANY                                                              
         MVC   SRTDAY,WORK                                                      
         ZIC   R4,5(R2)                                                         
         GOTO1 DAYVAL,DMCB,((R4),SRTDAY),DAY,WORK                               
         CLI   DAY,0                                                            
         BNE   *+12                                                             
         MVI   ERROR,INVDAY                                                     
         B     TRAPERR                                                          
*                                                                               
         LA    R2,SRTTIMEH         TIME                                         
         GOTO1 ANY                                                              
         MVC   SRTTIME,WORK                                                     
         ZIC   R4,5(R2)                                                         
         GOTO1 TIMVAL,DMCB,((R4),SRTTIME),TIME                                  
         CLI   0(R1),X'FF'                                                      
         BNE   *+12                                                             
         MVI   ERROR,INVTIME                                                    
         B     TRAPERR                                                          
*                                                                               
VK140    MVC   KEY(13),SAVEKEY         USE THIS NSID RECORD LATER               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   VK220                                                            
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         L     R4,AIO                                                           
         MVI   ELCODE,EDPCODEQ     DAY/TIME ELEMENT                             
         BAS   RE,GETEL                                                         
         BNE   VK220                                                            
*                                                                               
         USING EDPELEM,R4                                                       
VK150    CLC   DAYTIME,EDPDAY      LOOK FOR THE GIVEN DAY/TIME                  
         BNE   *+14                                                             
         MVC   SEQNUM,EDPSEQ                                                    
         B     VK170                                                            
         DROP  R4                                                               
*                                                                               
         BAS   RE,NEXTEL                                                        
         BE    VK150                                                            
         B     VK220                                                            
*                                                                               
VK170    LA    R9,SAVEKEY                                                       
         MVC   SIRKSEQ,SEQNUM      PUT SEQUENCE NUMBER IN KEY                   
*                                                                               
         CLI   ACTNUM,ACTADD       TEST ACTION ADD                              
         BNE   *+8                                                              
         OI    GENSTAT1,OKADDEL    OK TO ADD BACK DELETED RECORDS               
*                                                                               
         CLC   SVTRESTA,=3X'00'                                                 
         BE    VK200                                                            
         LA    R1,2000                                                          
         L     RF,AIO2                                                          
         L     RE,AIO3                                                          
         MVC   SAVEKEY(13),0(RE)                                                
         MOVE  ((RF),(R1)),(RE)                                                 
         B     VK280                                                            
*                                                                               
VK200    MVC   AIO,AIO2            RESTORE AIO                                  
         LA    R9,KEY                                                           
         XC    SIRKEY,SIRKEY                                                    
         MVC   SIRKEY(13),SAVEKEY  THE DETAIL RECORD KEY                        
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   SIRKEY,KEYSAVE      TEST SCHEME EXISTS                           
         BE    VK250                                                            
*-- IF NO DETAIL RECORD EXISTS                                                  
VK220    L     RE,AIO2             CLEAR THE WORK AREA                          
         LA    RF,2000                                                          
         XCEF                                                                   
*                                                                               
         MVI   NDETSW,X'FF'                                                     
         B     VK280                                                            
*                                                                               
VK250    MVC   SVDETADR,KEY+14                                                  
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC              GET THE DETAIL RECORD                        
*--SET SCREEN FIELDS TO VALIDATED                                               
VK280    OI    SRTSTAH+FVIIND-FVIHDR,FVIVAL                                     
         OI    SRTDPTH+FVIIND-FVIHDR,FVIVAL                                     
         OI    SRTSRCH+FVIIND-FVIHDR,FVIVAL                                     
         OI    SRTDAYH+FVIIND-FVIHDR,FVIVAL                                     
         OI    SRTTIMEH+FVIIND-FVIHDR,FVIVAL                                    
         OI    SRTYRH+FVIIND-FVIHDR,FVIVAL                                      
         OI    SRTPERH+FVIIND-FVIHDR,FVIVAL                                     
         OI    SRTSCHH+FVIIND-FVIHDR,FVIVAL                                     
         OI    SRTMNUH+FVIIND-FVIHDR,FVIVAL                                     
         B     XIT                                                              
         DROP  R9                                                               
         EJECT                                                                  
* FIRST TIME FOR REPORT                                                         
*                                                                               
FSTRPT   NTR1                                                                   
         CLI   TWADEM,X'FF'                                                     
         BNE   FRX                                                              
*                                                                               
         MVC   SRTSRC(1),DEMOSRC   GET BKVALSRC FIELD                           
         MVI   SRTSRCH+5,X'01'                                                  
         LA    R2,SRTSRCH                                                       
         GOTO1 VALISRC                                                          
         LA    R2,SAVED                                                         
         USING SAVED,R2                                                         
         MVC   SVSELMED,DBSELMED                                                
         MVC   SVSELSRC,DBSELSRC                                                
*                                                                               
*****    CLI   SVAPROF+7,C'C'      TEST CANADIAN AGENCY                         
*****    BNE   FR02                NO                                           
*****    CLI   SRTSRC,C'A'         TEST ARBITRON                                
*****    BNE   *+14                NO                                           
*****    MVC   SRTSRC,=C'BBM'      IT'S CANADIAN ARBITRON                       
*****    B     FR02                                                             
*****    MVC   SRTSRC,=C'CSI'      IT'S CANADIAN NIELSON                        
*                                                                               
FR02     XC    SAVPROG,SAVPROG                                                  
*                                                                               
         L     R3,AIO2                                                          
         USING SIRRECD,R3                                                       
*                                                                               
         MVC   AIO,AIO2            RESTORE AIO                                  
         L     R4,AIO                                                           
         MVI   ELCODE,EECCODEQ     DETAIL COST ELEMENT                          
         BAS   RE,GETEL                                                         
         BNE   FR05                                                             
         ICM   RF,15,4(R4)       SAVE DETAIL COST                               
         SR    RE,RE                                                            
         LA    R0,100                                                           
         ST    R0,FULL                                                          
         D     RE,FULL                                                          
         ST    RF,FULL                                                          
         MVC   DETCOST,FULL                                                     
         DROP  R3                                                               
*                                                                               
FR05     SR    R0,R0                                                            
         LA    R4,SRTSCHH          TURN PREV VALIDATED BITS ON                  
*                                                                               
FR10     TM    FVATRB-FVIHDR(R4),FVAPROT                                        
         BO    *+8                                                              
         OI    FVIIND-FVIHDR(R4),FVIVAL                                         
         IC    R0,0(R4)                                                         
         AR    R4,R0                                                            
         CLI   0(R4),0                                                          
         BNE   FR10                                                             
*                                                                               
         XC    SVUPDAY,SVUPDAY     CLEAR SAVED VALUES                           
         XC    SVUPTIM,SVUPTIM                                                  
         XC    SVBKS,SVBKS                                                      
*                                                                               
         MVC   AIO,AIO2            RESTORE AIO                                  
         L     R4,AIO                                                           
         MVI   ELCODE,EUPCODEQ     DETAIL DEFAULT UPGRADE ELEMENT               
         BAS   RE,GETEL                                                         
         BNE   FR15                                                             
         MVI   LABUPEL,X'FF'                                                    
*                                                                               
         USING EUPELEM,R4                                                       
         MVC   SVUPFILE(1),EUPUPFIL   STORE UPGRADE EXPRESSION                  
         MVC   SVUPGRD(8),EUPUPGRD                                              
         MVC   SVUPFRBK(2),EUPUPFBK                                             
         MVC   SVUPINP(16),EUPUPINP                                             
*                                                                               
FR15     L     R4,AIO                                                           
         MVI   ELCODE,EOVCODEQ     DETAIL DEFAULT DAY/TIME ELEMENT              
         BAS   RE,GETEL                                                         
         BNE   FR25                                                             
*                                                                               
         USING EOVELEM,R4                                                       
         MVC   SVUPDAY(1),EOVUPDAY   STORE UPGRADE EXPRESSION                   
         MVC   SVUPTIM(4),EOVUPTIM                                              
*                                                                               
FR25     BAS   RE,DEMO             INSPECT DEMO FIELD                           
*                                                                               
         XC    SRTCST,SRTCST                                                    
         XC    SRTCSTH+5(1),SRTCSTH+5                                           
         BAS   RE,COST             INSPECT THE COST FIELD                       
         BE    *+6                 NO DATA IN FIELD                             
         DC    H'0'                                                             
         L     R3,AIO3             EDIT THE COST                                
         XC    EBLOCK,EBLOCK                                                    
         MVI   EBTIN,C'B'                                                       
         MVI   EBLIN,4                                                          
         LA    R1,DETCOST                                                       
         ST    R1,EBAIN                                                         
         LA    R1,SRTCST                                                        
         ST    R1,EBAOUT                                                        
         MVI   EBALIGN,C'L'                                                     
         MVI   EBLOUT,L'SRTCST                                                  
         MVI   EBDECS,0                                                         
         MVI   EBFILL,0                                                         
         GOTO1 VEDITOR,DMCB,EBLOCK                                              
         OI    SRTCSTH+6,FVOXMT                                                 
*                                                                               
*--DO UPGRADE DISPLAY                                                           
         XC    SRTUPG,SRTUPG                                                    
         XC    SRTUPGH+5(1),SRTUPGH+5                                           
         BAS   RE,UPGRADE          INSPECT UPGRADE FIELD                        
         BE    *+6                 NO DATA IN FIELD                             
         DC    H'0'                                                             
         BAS   RE,DISUPGD                                                       
*                                                                               
*                                                                               
FR32     BAS   RE,BOOKS            INSPECT BOOK FIELDS                          
         OC    SVBKS,SVBKS                                                      
         BNZ   FR60                BOOKS INPUT                                  
*                                                                               
         OC    PERBOOKS,PERBOOKS   NONE - TEST FOR PERIOD DEFINED BOOKS         
         BZ    *+14                                                             
         MVC   SVBKS,PERBOOKS             YES - USE THOSE                       
         B     FR48                                                             
*                                                                               
         XC    DBLOCK,DBLOCK       NO BATCH DEFINED BOOKS -                     
         MVC   DBCOMFCS,ACOMFACS   GET LATEST BOOK FROM DEMAND                  
         MVC   DBFILE,=C'TP '                                                   
         MVC   DBAREC,AIO3                                                      
*****    MVC   DBSELMED,QMED                                                    
*****    CLI   SVAPROF+7,C'C'      TEST CANADIAN AGENCY                         
*****    BNE   *+8                 NO                                           
*****    MVI   DBSELMED,C'C'       MOVE CANADIEN MEDIA                          
*****    MVC   DBSELSRC,BKVALSRC                                                
         MVC   DBSELMED,SVSELMED                                                
         MVC   DBSELSRC,SVSELSRC                                                
         MVC   DBSELSTA,QSTA                                                    
         L     RE,ATWA                                                          
         MVC   DBSELAGY,14(RE)                                                  
         MVI   DBFUNCT,DBGETTLB                                                 
         GOTO1 VDEMAND,DMCB,DBLOCK,0                                            
         XC    SVBKS,SVBKS         CLEAR SAVED BOOKS FIELD                      
         LA    R4,SVBKS+6                                                       
         LA    R1,4                                                             
         MVC   0(2,R4),DBACTBK                                                  
         OC    SVUPFRBK,SVUPFRBK   TEST FOR OVERRIDE SHARE BOOK                 
         BZ    FR46                NO - THEN INCLUDE LATEST BOOK                
*                                                                               
FR34     MVC   0(2,R4),DBACTBK     DETERMINE ALL FOUR BOOKS                     
         OC    SVUPFRBK,SVUPFRBK   TEST FOR OVERRIDE SHARE BOOK                 
         BZ    *+14                                                             
         CLC   0(2,R4),SVUPFRBK    YES - COMPARE TO SHARE BOOK MONTH            
         BE    FR46                                                             
         CLC   0(2,R4),SVUPGRD+2   COMPARE TO PUT MONTH                         
         BE    FR46                                                             
         LA    R0,4                                                             
         LA    RE,MAJBKS                                                        
*                                                                               
FR36     CLC   1(1,R4),0(RE)                                                    
         BE    FR46                                                             
         BL    FR40                                                             
         LA    RE,1(RE)                                                         
         BCT   R0,FR36                                                          
*                                                                               
FR40     CLC   0(1,RE),MAJBKS                                                   
         BNE   FR44                                                             
         MVC   1(1,R4),MAJBKS+3                                                 
         ZIC   RF,DBACTBK                                                       
         BCTR  RF,0                                                             
         STC   RF,DBACTBK                                                       
         STC   RF,0(R4)                                                         
         B     FR46                                                             
*                                                                               
FR44     BCTR  RE,0                                                             
         SR    RF,RF                                                            
FR45     IC    RF,1(R4)                                                         
         BCTR  RF,0                                                             
         STC   RF,1(R4)                                                         
         CLC   1(1,R4),0(RE)                                                    
         BNH   FR46                                                             
         CLC   0(2,R4),SVUPFRBK                                                 
         BE    FR46                                                             
         CLC   0(2,R4),SVUPGRD+2                                                
         BE    FR46                                                             
         B     FR45                                                             
*                                                                               
FR46     ZIC   RF,1(R4)                                                         
         BCTR  RF,0                                                             
         LTR   RF,RF                                                            
         BZ    *+12                                                             
         STC   RF,DBACTBK+1                                                     
         B     FR47                                                             
         MVI   DBACTBK+1,12                                                     
         IC    RF,0(R4)                                                         
         BCTR  RF,0                                                             
         STC   RF,DBACTBK                                                       
*                                                                               
FR47     BCTR  R4,0                                                             
         BCTR  R4,0                                                             
         BCT   R1,FR34                                                          
*                                                                               
FR48     LA    R0,4                DISPLAY THE BOOKS                            
         LA    R4,SVBKS                                                         
         LA    R8,SRTBK1H                                                       
         MVI   WORK+2,1                                                         
*                                                                               
FR49     MVC   WORK(2),0(R4)                                                    
         GOTO1 DATCON,DMCB,(3,WORK),(6,8(R8))                                   
         OI    6(R8),FVOXMT                                                     
FR50     LA    R4,2(R4)                                                         
         LA    R8,SRTBK2H-SRTBK1H(R8)                                           
         BCT   R0,FR49                                                          
*                                                                               
*--READ COMPETITION RECORD SEE IF IT EXISTS                                     
*                                                                               
FR60     SR    RE,RE               SET UP YEAR MONTH FIELD                      
         ICM   RE,1,SCRYEAR                                                     
         OC    SCRYEAR,SCRYEAR     GET YEAR FROM SCREEN                         
         BNZ   FR61                                                             
         ICM   RE,1,CURYEAR        GET YEAR FROM SCHEME                         
FR61     SRDA  RE,32(0)                                                         
         D     RE,=F'10'                                                        
         SLA   RE,4(0)                                                          
         SR    RF,RF                                                            
         ICM   RF,1,PERNUM                                                      
         OR    RE,RF                                                            
         STC   RE,COMPER                                                        
*                                                                               
         L     R3,AIO1                                                          
         USING COMPETE,R3                                                       
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(2),=XL2'0C01'                                                
         MVC   KEY+2(1),SAVEKEY+1  A/M CODE                                     
         MVC   KEY+3(2),SAVEKEY+2  SCHEME CODE                                  
         MVC   KEY+5(1),COMPER     Y/M                                          
         MVC   KEY+6(2),BMKT       MARKET                                       
         MVC   KEY+8(5),DAYTIME    DAY TIME                                     
         MVC   COMPKEY(13),KEY                                                  
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE      TEST COMPETITION EXISTS                     
         BNE   FR79                                                             
*                                                                               
         MVC   AIO,AIO1             RESTORE AIO                                 
         PRINT GEN                                                              
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         PRINT NOGEN                                                            
         MVI   RECSW,X'FF'         RECORD EXISTS                                
         BAS   RE,CHKSRC                                                        
*                                                                               
*--GET PROGRAM ELEMENT FOR STATION                                              
*                                                                               
         OI    SRTSTA+3,X'40'      FILL FIELD WITH BLANKS                       
         LA    R1,CMSSTA                                                        
         LA    R2,20                                                            
         LA    R9,1                                                             
*                                                                               
FR65     CLC   0(4,R1),SRTSTA                                                   
         BE    FR70                                                             
         LA    R9,1(R9)                                                         
         LA    R1,5(R1)                                                         
         BCT   R2,FR65                                                          
*                                                                               
FR70     STC   R9,STANUM                                                        
*                                                                               
         L     R4,AIO1                                                          
         MVI   ELCODE,CMPCODEQ                                                  
         ZIC   R2,STANUM                                                        
         BAS   RE,GETEL                                                         
         BE    FR77                                                             
         DC    H'0'                                                             
*                                                                               
FR74     BAS   RE,NEXTEL                                                        
         BE    FR77                                                             
         LA    R2,SRTSTAH                                                       
         B     NOSTAT             INVALID STATION REQUEST                       
FR77     BCT   R2,FR74                                                          
*                                                                               
         MVC   CKBYTE,2(R4)                                                     
         NI    CKBYTE,X'7F'         TURN OFF OVERRIDE BIT                       
         CLC   CKBYTE,STANUM                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   SAVPROG,3(R4)                                                    
         MVC   SRTPRG,3(R4)                                                     
         OI    SRTPRGH+6,X'80'                                                  
*                                                                               
         BAS   RE,DEMO                                                          
         BAS   RE,GETDEMS          GET THE RATING/SHARE VALUES                  
         BAS   RE,DEMUP            DO THE UPGRADES                              
         B     XIT                                                              
*                                                                               
*                                                                               
*--GET MARKET AND ACTUAL SOURCE                                                 
FR79     LA    R9,4                SET BOOK LOOKUP LOOP                         
         LA    R8,SVBKS                                                         
FR79A    XC    DBLOCK,DBLOCK       FIND RMKT AND ACTSRC                         
         MVI   DBFUNCT,DBGETDEM                                                 
         MVC   DBAREC,AIO3                                                      
         MVC   DBFILE,=C'TP '                                                   
         MVC   DBCOMFCS,ACOMFACS                                                
*****    MVC   DBSELMED,QMED                                                    
*****    CLI   SVAPROF+7,C'C'      TEST CANADIAN AGENCY                         
*****    BNE   *+8                 NO                                           
*****    MVI   DBSELMED,C'C'       MOVE CANADIEN MEDIA                          
*****    MVC   DBSELSRC,BKVALSRC                                                
         MVC   DBSELMED,SVSELMED                                                
         MVC   DBSELSRC,SVSELSRC                                                
         MVC   DBSELUMK,BMKT                                                    
         MVC   DBSELSTA,QSTA                                                    
         L     RE,ATWA                                                          
         MVC   DBSELAGY,14(RE)                                                  
         GOTO1 CLUNPK,DMCB,SCHCLI,DBSELCLI                                      
         MVC   DBSELBK,0(R8)                                                    
         GOTO1 VDEMAND,DMCB,DBLOCK,0                                            
         OC    DBACTRMK,DBACTRMK   SEE IF MARKET FOUND                          
         BNZ   FR79B                                                            
         LA    R8,2(R8)                                                         
         OC    0(2,R8),0(R8)                                                    
         BZ    NODATA                                                           
         BCT   R9,FR79A                                                         
         B     NODATA                                                           
*                                                                               
FR79B    MVC   HLDBOOK,0(R8)                                                    
         MVC   MKTRS,DBACTRMK                                                   
         MVC   SRTSRC(1),DBACTSRC                                               
         MVC   DEMOSRC,SRTSRC                                                   
         MVC   SVSELSRC,DBACTSRC                                                
         MVI   SRTSRCH+5,X'01'                                                  
         DROP  R2                                                               
         LA    R2,SRTSRCH                                                       
         GOTO1 VALISRC                                                          
         LA    R2,SAVED                                                         
         USING SAVED,R2                                                         
*                                                                               
******   CLI   SVAPROF+7,C'C'      TEST CANADIAN AGENCY                         
         CLI   SVSELMED,C'C'                                                    
         BNE   FR80                NO                                           
         CLI   SRTSRC,C'A'         TEST ARBITRON                                
         BNE   *+18                NO                                           
         MVC   SRTSRC,=C'BBM'      IT'S CANADIAN ARBITRON                       
         MVI   DEMOSRC,C'B'                                                     
         B     FR80                                                             
         MVC   SRTSRC,=C'CSI'      IT'S CANADIAN NIELSON                        
         MVI   DEMOSRC,C'C'                                                     
*                                                                               
*                                                                               
*--LOOK FOR PROGRAMMING OVERRIDE ELEMENT                                        
FR80     L     R4,AIO2                                                          
         MVI   ELCODE,EPRCODEQ                                                  
         BAS   RE,GETEL                                                         
         BNE   FR85                                                             
         MVC   SAVPROG,2(R4)                                                    
         MVC   SRTPRG,2(R4)                                                     
         OI    SRTPRGH+6,X'80'                                                  
*-- BUILD COMPETITION RECORD                                                    
FR85     L     RE,AIO1             CLEAR THE WORK AREA                          
         LA    RF,2000                                                          
         XCEF                                                                   
         MVC   CMPKEY(13),KEYSAVE                                               
         MVC   CMSCODE(2),=X'0166'                                              
*                                                                               
*--READ COMPETITION STATION LIST RECORD SEE IF IT EXISTS                        
*                                                                               
         XC    CMSSTA(100),CMSSTA                                               
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(2),=XL2'0D47'                                                
         MVC   KEY+2(1),SAVEKEY+1  A/M CODE                                     
         MVC   KEY+3(2),BMKT       MARKET                                       
         MVC   KEY+5(2),SAVEKEY+2  SCHEME                                       
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE      TEST STATION EXISTS                         
         BE    FR90                                                             
*                                                                               
         MVC   KEY,KEYSAVE                                                      
         XC    KEY+5(2),KEY+5                                                   
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE      TEST STATION EXISTS                         
         BNE   FR120                                                            
*                                                                               
FR90     MVC   AIO,AIO3             RESTORE AIO                                 
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         L     R8,AIO3                                                          
         USING CLSKEY,R8                                                        
*                                                                               
         LA    R1,CLSSTA           LOOK FOR STATION ELEMENT                     
         LA    R9,CMSSTA                                                        
         LA    R4,CLSEL                                                         
         ZIC   RF,1(R4)                                                         
         AR    RF,R4                                                            
         BCTR  RF,0                                                             
         LA    RE,5                                                             
FR100    MVC   0(5,R9),0(R1)                                                    
         LA    R9,5(R9)                                                         
         BXLE  R1,RE,FR100                                                      
         B     FR130                                                            
         DROP  R8                                                               
*                                                                               
*-IF NO STATION LIST RECORD USE THE WHOLE MARKET                                
FR120    XC    DBLOCK,DBLOCK       FIND STATIONS IN THE MARKET                  
         MVI   DBFUNCT,DBGETMS                                                  
         MVC   DBAREC,AIO3                                                      
         MVC   DBFILE,=C'TP '                                                   
         MVC   DBCOMFCS,ACOMFACS                                                
*****    MVC   DBSELMED,QMED                                                    
*****    CLI   SVAPROF+7,C'C'      TEST CANADIAN AGENCY                         
*****    BNE   *+8                 NO                                           
*****    MVI   DBSELMED,C'C'       MOVE CANADIEN MEDIA                          
*****    MVC   DBSELSRC,BKVALSRC                                                
         MVC   DBSELMED,SVSELMED                                                
         MVC   DBSELSRC,SVSELSRC                                                
         MVC   DBSELRMK,MKTRS                                                   
         MVC   DBSELBK,HLDBOOK                                                  
         LA    RE,CMSSTA                                                        
         ST    RE,STAADDR                                                       
         XC    LOOPCT,LOOPCT                                                    
         GOTO1 VDEMAND,DMCB,DBLOCK,STAHOOK,0                                    
*                                                                               
*--GET STATION NUMBER                                                           
FR130    OI    SRTSTA+3,X'40'      FILL FIELD WITH BLANKS                       
         LA    R1,CMSSTA                                                        
         LA    R2,20                                                            
         LA    R9,1                                                             
*                                                                               
FR150    CLC   0(4,R1),SRTSTA                                                   
         BE    FR160                                                            
         LA    R9,1(R9)                                                         
         LA    R1,5(R1)                                                         
         BCT   R2,FR150                                                         
         LA    R2,SRTSTAH                                                       
         B     NODATA                                                           
*                                                                               
FR160    STC   R9,STANUM                                                        
*                                                                               
         BAS   RE,GETDEMS          GET THE RATING/SHARE VALUES                  
         BAS   RE,DEMUP            DO THE UPGRADES                              
*                                                                               
FRX      B     XIT                                                              
         EJECT                                                                  
* DEMAND HOOK FOR EXTRACTING MARKET STATIONS                                    
*                                                                               
STAHOOK  L     R4,DBAREC                                                        
         USING MLKEY,R4                                                         
         OC    MLKMKT,MLKMKT       TEST SPILL MARKET                            
         BNZ   SHX                 YES - IGNORE                                 
         TM    MLSTAT,X'F0'        TEST STATION NUMERIC                         
         BO    SHX                 YES - IGNORE                                 
         ZIC   RF,LOOPCT                                                        
         LA    RF,1(RF)                                                         
         STC   RF,LOOPCT                                                        
         CLI   LOOPCT,20                                                        
         BNH   *+6                                                              
         DC    H'0'                BLOW IF TOO MANY STATIONS                    
         L     RF,STAADDR                                                       
         MVC   0(5,RF),MLSTAT                                                   
         LA    RF,5(RF)                                                         
         ST    RF,STAADDR                                                       
SHX      BR    RE                                                               
         DROP  R3                                                               
         EJECT                                                                  
         DROP  R2                                                               
* DISPLAY RECORDS                                                               
*                                                                               
DISREC   NTR1                                                                   
         LA    R8,SAVED                                                         
         USING SAVED,R8                                                         
*                                                                               
         CLI   TWADEM,X'FF'                                                     
         BE    DR340                                                            
*                                                                               
         XC    STCHG,STCHG                                                      
*--READ COMPETITION RECORD FROM TEMP STORAGE                                    
         XC    DMCB,DMCB           SAVE TWA                                     
         MVC   DMCB+10(2),2(RA)    TERMINAL NUMBER                              
         MVI   DMCB+8,3            PAGE NUMBER                                  
         L     R9,AIO1                                                          
         MVC   DMCB+20(2),=C'L='                                                
         MVC   DMCB+22(2),=X'07D0'     LENGTH OF IO AREA                        
         GOTO1 DATAMGR,DMCB,(0,=C'DMREAD'),=C'TEMPSTR',,(R9)                    
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   0(13,R9),COMPKEY    SEE IF COMPETE RECORD LOADED                 
         BE    *+6                                                              
         DS    H'0'                                                             
*                                                                               
*--READ DETAIL RECORD                                                           
         CLI   NDETSW,X'FF'                                                     
         BE    DR050                                                            
         MVC   KEY(13),SAVEKEY                                                  
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   AIO,AIO2                                                         
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
*                                                                               
DR050    L     R3,AIO1                                                          
         USING COMPETE,R3                                                       
*                                                                               
         BAS   RE,COST             VALIDATE COST FIELD                          
*                                                                               
         TM    SRTBK1H+FVIIND-FVIHDR,FVIVAL    TEST ANY BOOK CHANGES            
         BZ    DR100                                                            
         TM    SRTBK2H+FVIIND-FVIHDR,FVIVAL                                     
         BZ    DR100                                                            
         TM    SRTBK3H+FVIIND-FVIHDR,FVIVAL                                     
         BZ    DR100                                                            
         TM    SRTBK4H+FVIIND-FVIHDR,FVIVAL                                     
         BO    DR120                                                            
*                                                                               
DR100    OI    SRTBK1H+FVIIND-FVIHDR,FVIVAL                                     
         OI    SRTBK2H+FVIIND-FVIHDR,FVIVAL                                     
         OI    SRTBK3H+FVIIND-FVIHDR,FVIVAL                                     
         OI    SRTBK4H+FVIIND-FVIHDR,FVIVAL                                     
         BAS   RE,BOOKS            INSPECT THE BOOK FIELDS                      
         BNE   DRX                                                              
         TM    LCHG,LBK                                                         
         BZ    DR120                                                            
         BAS   RE,GETDEMS                                                       
*                                                                               
DR120    TM    SRTUPGH+FVIIND-FVIHDR,FVIVAL TEST UPGRADE CHANGED                
         BO    DR140                                                            
         OI    SRTUPGH+FVIIND-FVIHDR,FVIVAL                                     
         BAS   RE,UPGRADE                                                       
         BL    DR140               EVERYTHINGS FINE                             
         BE    *+6                                                              
         DC    H'0'                                                             
         OC    LABUPEL,LABUPEL     UPGRADE REMOVED - TEST FOR UPGRD ELE         
         BZ    DR130                                                            
         OI    LCHG,LUPG           YES - INDICATE UPGRADE CHANGE                
         MVC   SVUPFILE,BCHUF            USE BATCH UPGRADE VALUES               
         MVC   SVUPGRD,BCHUP                                                    
         MVC   SVUPFRBK,BCHFB                                                   
         MVC   SVUPINP,BCHUPIN                                                  
         MVC   AIO,AIO2                                                         
         MVI   ELCODE,EUPCODEQ                                                  
         GOTO1 REMELEM                                                          
         MVI   ELCODE,EOVCODEQ                                                  
         GOTO1 REMELEM                                                          
         XC    LABUPEL,LABUPEL                                                  
*                                                                               
DR130    BAS   RE,DISUPGD                DISPLAY BATCH UPGRADE                  
*                                                                               
DR140    TM    LCHG,LUPG           TEST FOR UPGRADE CHANGE                      
         BZ    DR220                                                            
         BAS   RE,DEMUP            YES - DO THE UPGRADES                        
         B     DR340                                                            
*                                                                               
*                                                                               
*--LOOP FOR OUTPUT CHECK                                                        
*                                                                               
DR220    XC    LOOPCT,LOOPCT                                                    
         BAS   RE,GETELADD                                                      
*--SEE IF PROGRAM FIELD HAS CHANGED                                             
         LA    R2,SRTPRGH                                                       
*                                                                               
DR230    TM    4(R2),FVIVAL                  INSPECT PROGRAM NAME FLD           
         BO    DR250                                                            
         OI    4(R2),FVIVAL                                                     
*                                                                               
         ICM   R4,15,PRGADDR       ADDRESS OF PROGRAM ELEMENT                   
         USING CMPEL,R4                                                         
         BNZ   *+6                                                              
         DC    H'0'                                                             
         CLC   CMPPROG(17),8(R2)              TEST PROGRAM CHANGE               
         BE    DR250                                                            
         OI    CMPSEQ,X'80'                                                     
         MVC   CMPPROG(17),=17X'40'                                             
         MVC   CMPPROG(17),8(R2)                  YES -                         
         MVC   SAVPROG,8(R2)                                                    
         OI    LCHG,LPRG                                                        
         DROP  R4                                                               
*                                                                               
*--BEGINNING OF LOOP SET R2 TO RATING/SHARE FIELD                               
*                                                                               
DR250    LA    R2,SRTRTGH                                                       
*                                                                               
DR255    CLI   LOOPCT,NDEMSCR      TEST SCANNED ALL FIELDS YET                  
         BNL   DR340               YES - DONE                                   
         ZIC   R9,TWADEM           DEMO DISPLACEMENT FOR FIRST ON SCRN          
         ZIC   R1,LOOPCT           RELATIVE DEMO ON SCREEN                      
         AR    R9,R1                                                            
         CLM   R9,1,SVDEMNO        COMPARE TO NUMBER OF ESTIMATE DEMOS          
         BNL   DR340               HI - DONE                                    
         MH    R9,=H'12'                                                        
         LA    R9,SVPROJ(R9)                                                    
         ST    R9,LAPROJ           LAPROJ = A(PROJECTED DEMO VALUES)            
         ICM   R9,15,LDMADDR       R9 = A(CURRENT DEMO CODE)                    
*                                                                               
*--GET DEMO ELEMENT FROM COMPETITION RECORD                                     
*                                                                               
         L     R4,AIO1                                                          
         MVI   ELCODE,CMDCODEQ     LOOK FOR DEMO ELEMENT                        
         BAS   RE,GETEL                                                         
         BE    DR270                                                            
*                                                                               
DR265    BAS   RE,NEXTEL                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
DR270    CLC   1(2,R9),2(R4)                                                    
         BE    *+8                                                              
         B     DR265                                                            
*                                                                               
         LA    R4,4(R4)                                                         
         SR    R9,R9                                                            
         ICM   R9,1,STANUM                                                      
         CLI   STANUM,1                                                         
         BE    DR280                                                            
         BCTR  R9,0                                                             
*                                                                               
DR275    LA    R4,3(R4)                                                         
         BCT   R9,DR275                                                         
*                                                                               
DR280    ST    R9,DEMADDR                                                       
         TM    FVIIND-FVIHDR(R2),FVIVAL   TEST RATING FIELD CHANGED             
         BO    DR285                                                            
         OI    FVIIND-FVIHDR(R2),FVIVAL   YES -                                 
         CLI   5(R2),0             VALIDATE RATING FIELD                        
         BE    INVDMO                                                           
         ZIC   RF,5(R2)                                                         
         ST    RF,DMCB+4                                                        
         MVC   FVIFLD(10),8(R2)                                                 
         LA    R1,FVIFLD           REMOVE THE * IF ANY                          
         CLI   0(R1),C'*'                                                       
         BNE   *+8                                                              
         MVI   0(R1),C' '                                                       
         LA    R1,1(R1)                                                         
         BCT   RF,*-16                                                          
         GOTO1 CASHVAL,DMCB,(1,FVIFLD)                                          
         CLI   DMCB,FF                                                          
         BE    INVDMO                                                           
         OC    DMCB+4(4),DMCB+4                                                 
*        BZ    DR330               ALLLOW ZERORATINGS                           
         CLC   1(2,R4),DMCB+6      TEST FOR RATING CHANGE                       
         BE    DR285                                                            
         OI    0(R4),X'80'         SET OVERRIDE INDICATOR                       
         MVC   1(2,R4),DMCB+6      YES - MOVE IN NEW RATING                     
         OI    LCHG,LRTG                                                        
         L     RE,LAPROJ                 CHANGE THE SAVED RATING VALUE          
         MVC   0(4,RE),DMCB+4                                                   
         OI    0(RE),X'80'               INDICATE OVERRIDE                      
*                                                                               
DR285    SR    RE,RE                                                            
         ICM   RE,1,0(R2)                                                       
         AR    R2,RE                                                            
         SR    RE,RE                                                            
         ICM   RE,1,0(R2)                                                       
         AR    R2,RE                                                            
         SR    RE,RE                                                            
         ICM   RE,1,0(R2)                                                       
         AR    R2,RE                                                            
*                                                                               
         CLI   5(R2),0             VALIDATE RATING FIELD                        
         BE    INVDMO                                                           
         TM    4(R2),FVIVAL        TEST FOR CHANGE                              
         BO    DR315                                                            
         OI    FVIIND-FVIHDR(R2),FVIVAL   YES --                                
         XC    ELEM,ELEM                                                        
         MVC   FVIFLD(10),8(R2)                                                 
         LA    RF,C'/'                                                          
         STC   RF,ELEM(RF)                                                      
         LR    RE,R2                                                            
         TRT   FVIFLD(L'SRTSHP),ELEM      SEARCH FOR / CHARACTER                
         BZ    INVDMO                                                           
         LR    R2,RE                                                            
         LR    R9,R1               R1 = A(C'/')                                 
         SR    RE,RE               LOCATE SHARE FIELD                           
         BCTR  RE,0                                                             
         LA    RF,FVIFLD                                                        
         BCTR  RF,0                                                             
         XC    FULL,FULL                                                        
*                                                                               
DR290    BXH   R9,RE,*+18                                                       
         OC    FULL,FULL                                                        
         BZ    INVDMO                                                           
         B     DR300                                                            
         CLI   0(R9),C' '                                                       
         BNH   DR295                                                            
         OC    FULL,FULL                                                        
         BNZ   DR290                                                            
         ST    R9,FULL                                                          
         B     DR290                                                            
DR295    OC    FULL,FULL                                                        
         BZ    DR290                                                            
*                                                                               
DR300    L     RF,FULL                                                          
         SR    RF,R9                                                            
         ST    RF,DMCB+4                                                        
         LA    R9,1(R9)                                                         
         ST    R1,FULL           FULL = A(C'/')                                 
         GOTO1 CASHVAL,DMCB,(1,(R9))     VALIDATE SHARE FIELD                   
         CLI   DMCB,FF                                                          
         BE    INVDMO                                                           
         OC    DMCB+4(4),DMCB+4                                                 
         BZ    DR330                                                            
         L     RE,LAPROJ                                                        
         CLC   4(4,RE),DMCB+4    TEST FOR SHARE CHANGE                          
         BE    DR310                                                            
         MVC   4(4,RE),DMCB+4    YES                                            
         OI    LCHG,LSHP                                                        
*                                                                               
DR310    L     R9,FULL           VALIDATE PUT FIELD                             
         LA    R9,1(R9)                                                         
         ZIC   RE,5(R2)                                                         
         LA    RE,FVIFLD(RE)                                                    
         SR    RE,R9                                                            
         ST    RE,DMCB+4                                                        
         GOTO1 CASHVAL,DMCB,(1,(R9))                                            
         CLI   DMCB,FF                                                          
         BE    INVDMO                                                           
         OC    DMCB+4(4),DMCB+4                                                 
         BZ    DR330                                                            
         L     RE,LAPROJ                                                        
         CLC   8(4,RE),DMCB+4    TEST FOR PUT CHANGE                            
         BE    DR315                                                            
         MVC   8(4,RE),DMCB+4    YES                                            
         OI    LCHG,LSHP                                                        
*                                                                               
DR315    TM    LCHG,LSHP           TEST FOR SHARE/PUT CHANGE                    
         BZ    DR320                                                            
         L     R9,LAPROJ           YES - CALCULATE NEW RATING                   
         L     R0,8(R9)                                                         
         SRDA  R0,32                                                            
         M     R0,4(R9)                                                         
         LA    R1,500(R1)                                                       
         D     R0,=F'1000'                                                      
         ST    R1,0(R9)                                                         
         OI    0(R9),X'80'               RATING OVERRIDE BIT                    
         STCM  R1,3,1(R4)                STORE RATING IN BWS RECORD             
         OI    0(R4),X'80'               SET OVERRIDE                           
         B     DR325                                                            
*                                                                               
DR320    TM    LCHG,LRTG           NO SHARE/PUT CHANGE - TEST FOR               
         BZ    DR325                                     RATING CHANGE          
         L     R9,LAPROJ           YES -                                        
         L     R0,0(R9)            CALCULATE NEW SHARE                          
         SLL   R0,1                ELIMINATE OVERRIDE BIT                       
         SRDL  R0,33                                                            
         M     R0,=F'1000'                                                      
         D     R0,8(R9)                                                         
         ST    R1,4(R9)                                                         
*                                                                               
DR325    TM    LCHG,LSHP+LRTG      TEST FOR RATING CHANGE                       
         BZ    DR330                                                            
         NI    LCHG,FF-LSHP-LRTG                                                
         OI    LCHG,LPROJ                                                       
*                                                                               
DR330    L     RE,LDMADDR                                                       
         LA    RE,9(RE)                                                         
         CLC   0(3,RE),=3X'00'                                                  
         BE    DR337                                                            
         CLI   0(RE),X'FF'                                                      
         BNE   DR338                                                            
DR337    LA    RE,SVDEMS                                                        
         ST    RE,LDMADDR                                                       
         B     DR340                                                            
DR338    ST    RE,LDMADDR                                                       
*                                                                               
         IC    RF,LOOPCT                                                        
         LA    RF,1(RF)                                                         
         STC   RF,LOOPCT                                                        
         CLI   LOOPCT,NDEMSCR                                                   
         BNL   DR340                                                            
*                                                                               
         SR    R0,R0               BUMP R2 TO RATING FIELD                      
         ICM   R0,1,0(R2)                                                       
         AR    R2,R0                                                            
         ICM   R0,1,0(R2)                                                       
         AR    R2,R0                                                            
         ICM   R0,1,0(R2)                                                       
         AR    R2,R0                                                            
         ICM   R0,1,0(R2)                                                       
         AR    R2,R0                                                            
*                                                                               
         B     DR255                                                            
*                                                                               
         DROP  R8                                                               
         DROP  R3                                                               
*                                                                               
DR340    LA    R3,SAVED                                                         
         USING SAVED,R3                                                         
*                                                                               
         CLI   TWADEM,FF           SKIP TO DISPLAY FOR VERY FIRST SCRN          
         BE    DR350                                                            
*                                   NO TEST FOR CHANGES THAT WILL               
         TM    LCHG,LCOST+LUPG+LBK+LPROJ  AFFECT THE NUMBERS ON SCREEN          
         BZ    DR350                                                            
         BAS   RE,GETDEMS                GET RATINGS AND SHARES                 
*                                                                               
DR350    CLI   TWADEM,FF           TEST FOR VERY FIRST SCREEN                   
         BNE   *+12                                                             
         MVI   TWADEM,0                                                         
         B     DR360                                                            
         TM    LCHG,LUPG+LBK+LPROJ+LPRG+LCOST    TEST CHANGE IN STATUS          
         BNZ   DR360                     YES- RE-DISPLAY CURRENT SCREEN         
         ZIC   RE,TWADEM                 NO   - DISPLAY NEXT SCREEN             
         LA    RF,NDEMSCR                                                       
         AR    RE,RF                                                            
         STC   RE,TWADEM                                                        
         CLC   TWADEM,SVDEMNO      TEST ALL SCREENS DISPLAYED                   
         BL    DR360                                                            
         MVI   TWADEM,0            YES - DISPLAY FROM FIRST SCREEN              
*                                                                               
DR360    SR    RE,RE               CLEAR THE SCREEN                             
         LA    R4,SRTDM1H                                                       
         LA    R8,SRTCMTH                                                       
DR370    IC    RE,0(R4)                                                         
         SH    RE,=H'9'                                                         
         TM    1(R4),X'02'                                                      
         BZ    DR375                                                            
         SH    RE,=H'8'            EXTENDED HEADER                              
DR375    BNM   *+6                                                              
         DC    H'0'                                                             
         EX    RE,DRCLC                                                         
         BE    DR378                                                            
         EX    RE,DROC                                                          
         BZ    DR378                                                            
         EX    RE,DRXC                                                          
         OI    6(R4),FVOXMT                                                     
DR378    TM    1(R4),X'02'                                                      
         BZ    DR380                                                            
         LA    R4,17(RE,R4)                                                     
         B     DR383                                                            
DR380    LA    R4,9(RE,R4)                                                      
DR383    CR    R4,R8                                                            
         BL    DR370                                                            
*                                                                               
         LA    R0,6                FORMAT DEMO NAMES                            
         ZIC   RE,TWADEM                                                        
         ZIC   RF,SVDEMNO                                                       
         SR    RF,RE                                                            
         CR    R0,RF               TEST FOR A FULL SCREEN                       
         BNH   *+6                                                              
         LR    R0,RF               NO                                           
         STC   R0,LNDEMSCR         NUMBER OF DEMOS ON THIS SCREEN               
         MH    RE,=H'7'                                                         
         LA    RE,SVDEMNAM(RE)                                                  
         LA    R1,SRTDM1H                                                       
         LA    R4,SRTLN1H                                                       
         LA    R8,SRTLN2H                                                       
*                                                                               
DR390    OC    0(7,RE),0(RE)                                                    
         BZ    DR400                                                            
         MVC   8(L'SRTDM1,R1),0(RE)                                             
         OI    6(R1),FVOXMT                                                     
         OI    6(R4),FVOXMT        TRANSMIT BOTH DEMO LINES                     
         OI    6(R8),FVOXMT                                                     
         LA    R1,SRTDM2H-SRTDM1H(R1)                                           
         LA    R4,SRTDM2H-SRTDM1H(R4)                                           
         LA    R8,SRTDM2H-SRTDM1H(R8)                                           
         LA    RE,7(RE)                                                         
         BCT   R0,DR390                                                         
*                                                                               
DR400    LA    RF,SRTRTGH                                                       
         LA    R0,6                                                             
DR402    OI    1(RF),FVAHIGH    SHOW HIGH INTENSITY                             
         LA    RF,SRTLN3H-SRTLN1H(RF)                                           
         BCT   R0,DR402                                                         
*                                                                               
         LA    RF,SRTSHPH                                                       
         LA    R0,6                                                             
DR405    OI    1(RF),FVAHIGH    SHOW HIGH INTENSITY                             
         LA    RF,SRTLN3H-SRTLN1H(RF)                                           
         BCT   R0,DR405                                                         
*                                                                               
         XC    EBLOCK,EBLOCK       FORMAT THE BOOK VALUES                       
         MVI   EBTIN,C'B'                                                       
         MVI   EBLIN,4                                                          
         LA    R0,4                                                             
         ZIC   R4,TWADEM                                                        
         MH    R4,=H'12'                                                        
         LA    R4,SVDEMVAL(R4)     R4 = A(RTG/SHR/PUT)                          
*                                                                               
DR410    ZIC   R9,LNDEMSCR                                                      
         LA    R2,SRTLN1                                                        
         USING LINE1D,R2                                                        
*                                                                               
DR420    LNR   RE,R0               FORMAT THE RATING                            
         AH    RE,=H'3'                                                         
         LA    R8,L1RTG1                                                        
         MVI   EBLOUT,L'L1RTG1                                                  
         LTR   RE,RE                                                            
         BM    *+16                                                             
         MH    RE,=Y(L1RTG3-L1RTG2)                                             
         LA    R8,L1RTG2(RE)                                                    
         MVI   EBLOUT,L'L1RTG2                                                  
         ST    R8,EBAOUT                                                        
         ST    R4,EBAIN                                                         
         MVI   EBDECS,1                                                         
         MVI   EBSCIN,0                                                         
         ZIC   RE,EBLOUT                                                        
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         XC    0(0,R8),0(R8)                                                    
         OC    0(4,R4),0(R4)                                                    
         BZ    DR430                                                            
         GOTO1 VEDITOR,DMCB,EBLOCK                                              
*                                                                               
DR430    LA    R4,4(R4)            FORMAT THE SHARE/PUT                         
         LA    R2,SRTLN2-SRTLN1(R2)                                             
         USING LINE2D,R2                                                        
         LNR   R8,R0                                                            
         LA    R8,4(R8)                                                         
         MH    R8,=Y(L2SHP2-L2SHP1)                                             
         LA    R8,L2SHP1(R8)                                                    
         XC    0(L'L2SHP1,R8),0(R8)                                             
         LA    R8,2(R8)                                                         
         BAS   RE,FMTSHP           FORMAT SHARE/PUT ROUTINE                     
         LA    R4,8(R4)                                                         
         LA    R2,SRTLN3-SRTLN2(R2)                                             
         BCT   R9,DR420            DO FOR ALL DEMOS                             
*                                                                               
DR440    LNR   R4,R0                                                            
         LA    R4,5(R4)                                                         
         MH    R4,=Y(NDEMOS*12)                                                 
         LA    R4,SVDEMVAL(R4)                                                  
         ZIC   RE,TWADEM                                                        
         MH    RE,=H'12'                                                        
         AR    R4,RE                                                            
         BCT   R0,DR410            DO FOR ALL BOOKS                             
*                                                                               
         LA    R8,SRTRTGH          FORMAT PROJECTED VALUES                      
         ZIC   R4,TWADEM                                                        
         MH    R4,=H'12'                                                        
         LA    R4,SVPROJ(R4)                                                    
         ZIC   R9,LNDEMSCR                                                      
*                                                                               
DR450    MVI   EBLOUT,L'SRTRTG     FORMAT PROJECTED RATING                      
         LA    RE,L'FVIHDR(R8)                                                  
         ST    RE,EBAOUT                                                        
         LA    RE,FULL                                                          
         ST    RE,EBAIN                                                         
         XC    0(L'SRTRTG,RE),0(RE)                                             
         MVC   FULL,0(R4)                                                       
         NI    FULL,FF-X'80'                                                    
*        OC    FULL,FULL                                                        
*        BZ    DR460                                                            
         TM    0(R4),X'80'         OVERRIDE                                     
         BZ    *+8                                                              
         MVI   EBFLOAT,C'*'                                                     
         MVI   EBDECS,1                                                         
         MVI   EBSCIN,0                                                         
         OI    EBOPT,X'20'         ZERO=NOBLANK                                 
         GOTO1 VEDITOR,DMCB,EBLOCK                                              
         OC    FULL,FULL                                                        
         BNZ   DR460                                                            
         L     R1,EBAOUT                                                        
         MVI   6(R1),C'*'          EDITOR DOESN'T FLOAT IF =0                   
*                                                                               
DR460    OI    6(R8),FVOXMT                                                     
         MVI   EBLOUT,L'SRTCPP     FORMAT THE CPP/CPM                           
         LA    R8,SRTCPPH-SRTRTGH(R8)                                           
         LA    RE,L'FVIHDR(R8)                                                  
         ST    RE,EBAOUT                                                        
         XC    0(L'SRTCPP,RE),0(RE)                                             
         LA    R1,DETCOST                                                       
         ICM   RE,15,0(R1)                                                      
         BZ    DR470                                                            
         OC    FULL,FULL                                                        
         BZ    DR470                                                            
         SRDA  RE,32                                                            
         M     RE,=F'1000'                                                      
         D     RE,FULL                                                          
         ST    RF,FULL                                                          
         MVI   EBDECS,2                                                         
         MVI   EBFLOAT,0                                                        
         CLC   FULL,=F'100000'                                                  
         BL    *+12                                                             
         MVI   EBDECS,0                                                         
         MVI   EBSCIN,X'82'                                                     
         MVI   EBFLOAT,C'$'                                                     
         GOTO1 VEDITOR,DMCB,EBLOCK                                              
*                                                                               
DR470    OI    6(R8),FVOXMT                                                     
         LA    R8,SRTSHPH-SRTCPPH+8(R8)                                         
         LA    R4,4(R4)                                                         
         BAS   RE,FMTSHP           FORMAT SHR/PUT FOR PROJECTED                 
         SH    R8,=H'8'                                                         
         OI    6(R8),FVOXMT                                                     
         LA    R4,8(R4)                                                         
         LA    R8,SRTLN3H-SRTSHPH(R8)                                           
         LA    R8,SRTRTGH-SRTLN1H(R8)                                           
         BCT   R9,DR450            DO FOR ALL DEMOS                             
*                                                                               
*--WRITE COMPETITION RECORD TO TEMP STORAGE                                     
DR480    XC    DMCB,DMCB           SAVE TWA                                     
         MVC   DMCB+10(2),2(RA)    TERMINAL NUMBER                              
         MVI   DMCB+8,3            PAGE NUMBER                                  
         L     R9,AIO1                                                          
         GOTO1 DATAMGR,DMCB,(0,=C'DMWRT'),=C'TEMPSTR',,(R9)                     
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   NDETSW,X'FF'                                                     
         BE    DRX                                                              
*                                                                               
         CLI   RECSW,X'FF'         SEE IF COMPETITION REC EXISTS                
         BE    DR490                                                            
         MVC   AIO,AIO1            YES - SAVE BWS RECORD THAT'S BEEN            
         GOTO1 ADDREC                                                           
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   RECSW,X'FF'                                                      
         B     DR500                                                            
*                                                                               
DR490    MVC   KEY(13),COMPKEY                                                  
         GOTO1 HIGH                                                             
         CLC   KEY(12),KEYSAVE      TEST COMPETITION EXISTS                     
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   AIO,AIO3                                                         
         PRINT GEN                                                              
         GOTO1 GETREC                                                           
         PRINT NOGEN                                                            
*--MAKE SURE IOAREAS ARE IN SYNC                                                
         L     RE,AIO1                                                          
         L     RF,AIO3                                                          
         CLC   0(13,RE),0(RF)                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   AIO,AIO1                                                         
         GOTO1 PUTREC                       YES - PUT THE RECORD                
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
DR500    TM    LCHG,LCOST+LUPG                                                  
         BZ    DRX                                                              
         CLC   SVKEY+6(3),SAVEKEY+6   SEE IF STATION IS DIFFERENT               
         BNE   DR600                                                            
         TM    LCHG,LCOST                 AFFECT THE NUMBERS ON SCREEN          
         BZ    DR550                                                            
         MVC   SVCOST1L,SRTCSTH+5                                               
         MVC   SVCOST1,SRTCST                                                   
*                                                                               
DR550    TM    LCHG,LUPG                  AFFECT THE NUMBERS ON SCREEN          
         BZ    DR600                                                            
         MVC   SVUPGL,SRTUPGH+5                                                 
         MVC   SVUPG,SRTUPG                                                     
DR600    MVC   KEY(13),SAVEKEY     INFORMATION                                  
         GOTO1 HIGH                                                             
         CLC   KEY(12),KEYSAVE      TEST DETAIL EXISTS                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   AIO,AIO3                                                         
         GOTO1 GETREC                                                           
*--MAKE SURE IOAREAS ARE IN SYNC                                                
         L     RE,AIO2                                                          
         L     RF,AIO3                                                          
         CLC   0(13,RE),0(RF)                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   AIO,AIO2                                                         
         GOTO1 PUTREC                       YES - PUT THE RECORD                
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
DRX      XC    LCHG,LCHG                                                        
         B     XIT                                                              
         DROP  R3                                                               
         SPACE 2                                                                
DRCLC    CLC   8(0,R4),BLANKS      EXECUTED INSTRUCTIONS                        
DROC     OC    8(0,R4),8(R4)                                                    
DRXC     XC    8(0,R4),8(R4)                                                    
         EJECT                                                                  
* ROUTINE TO FORMAT SHARE/PUT                                                   
* INPUT  : R4 = A(SHR/PUT)                                                      
*          R8 = A(FORMAT AREA)                                                  
*                                                                               
FMTSHP   NTR1  ,                                                                
         MVI   BYTE,5                                                           
         CLC   4(4,R4),=F'100000'                                               
         BNL   FS4                                                              
         CLC   4(4,R4),=F'10000'                                                
         BNL   FS2                                                              
         CLC   4(4,R4),=F'1000'                                                 
         BNL   FS4                                                              
FS2      LA    R8,1(R8)                                                         
         MVI   BYTE,4                                                           
         CLC   4(4,R4),=F'100'                                                  
         BNL   FS4                                                              
         LA    R8,1(R8)                                                         
         MVI   BYTE,3                                                           
FS4      MVI   EBFLOAT,0           FORMAT THE SHARE                             
         MVI   EBDECS,1                                                         
         MVI   EBSCIN,0                                                         
         OC    0(4,R4),0(R4)                                                    
         BNZ   *+14                                                             
         MVC   1(3,R8),=C'0.0'                                                  
         B     FS6                                                              
         ST    R8,EBAOUT                                                        
         MVI   EBLOUT,4                                                         
         ST    R4,EBAIN                                                         
         GOTO1 VEDITOR,DMCB,EBLOCK                                              
FS6      MVI   4(R8),C'/'          FORMAT THE PUT                               
         LA    R4,4(R4)                                                         
         ST    R4,EBAIN                                                         
         LA    R8,5(R8)                                                         
         OC    0(4,R4),0(R4)                                                    
         BNZ   *+14                                                             
         MVC   0(3,R8),=C'0.0'                                                  
         B     FSX                                                              
         ST    R8,EBAOUT                                                        
         MVC   EBLOUT,BYTE                                                      
         CLC   0(4,R4),=F'10000'                                                
         BL    *+12                                                             
         MVI   EBDECS,0                                                         
         MVI   EBSCIN,X'81'                                                     
         GOTO1 VEDITOR,DMCB,EBLOCK                                              
         CLI   EBDECS,0                                                         
         BNE   FSX                                                              
         GOTO1 CASHVAL,DMCB,(1,(R8)),4                                          
         MVC   0(4,R4),DMCB+4                                                   
FSX      B     XIT                                                              
         EJECT                                                                  
* CREATE THE DEFAULT DEMO LIST                                                  
DEMO     NTR1                                                                   
         LA    R8,SAVED                                                         
         USING SAVED,R8                                                         
*--READ DEMO MENU RECORD                                                        
         XC    KEY,KEY                                                          
         MVC   KEY(2),=XL2'0D26'                                                
         MVC   KEY+2(1),SAVEKEY+1                                               
         MVC   KEY+3(4),SVDEMNM                                                 
*                                                                               
         LA    R2,SRTMNUH                                                       
         MVC   AIO,AIO3            RESTORE AIO                                  
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE     TEST DEMO MENU EXISTS                        
         BNE   INVDMN                                                           
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         LA    R1,14                                                            
         LA    R2,SVDEMS                                                        
         LA    R3,SVDEMNAM                                                      
         L     R4,AIO                                                           
         LA    R9,0                                                             
         MVI   ELCODE,X'05'        LOOK FOR DEMO ELEMENT                        
         BAS   RE,GETEL                                                         
         BE    GDEMO50                                                          
         DC    H'0'                                                             
*                                                                               
GDEMO40  BAS   RE,NEXTEL                                                        
         BNE   GDEMEX                                                           
*                                                                               
GDEMO50  LA    R9,1(R9)                                                         
*                                                                               
         MVC   0(3,R2),2(R4)                                                    
         MVC   3(3,R2),2(R4)                                                    
         MVC   6(3,R2),2(R4)                                                    
         CLI   1(R2),C'P'                                                       
         BE    *+12                                                             
         CLI   1(R2),C'H'                                                       
         BNE   *+20                                                             
         MVI   1(R2),C'R'                                                       
         MVI   4(R2),C'S'                                                       
         MVI   7(R2),C'P'                                                       
         B     GDEMO70                                                          
         CLI   1(R2),C'R'                                                       
         BNE   *+16                                                             
         MVI   4(R2),C'S'                                                       
         MVI   7(R2),C'P'                                                       
         B     GDEMO70                                                          
         CLI   1(R2),C'E'                                                       
         BNE   *+16                                                             
         MVI   4(R2),C'S'                                                       
         MVI   7(R2),C'P'                                                       
         B     GDEMO70                                                          
         CLI   1(R2),C'C'                                                       
         BE    GDEMO60                                                          
         CLI   1(R2),C'I'                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
GDEMO60  MVI   4(R2),C'X'                                                       
         MVI   7(R2),C'Q'                                                       
GDEMO70  MVC   0(7,R3),5(R4)                                                    
         LA    R2,9(R2)                                                         
         LA    R3,7(R3)                                                         
         BCT   R1,GDEMO40                                                       
*                                                                               
GDEMEX   MVI   0(R2),X'FF'                                                      
         MVI   0(R3),X'FF'                                                      
         STC   R9,SVDEMNO                                                       
         B     XIT                                                              
         DROP  R8                                                               
         EJECT                                                                  
* INSPECT BOOK FIELDS                                                           
* OUTPUT : LCHG = LBK IF BOOKS WERE CHANGED                                     
*          CC EQ  - OK                                                          
*             NE  - ERROR                                                       
*                                                                               
BOOKS    NTR1                                                                   
         LA    R3,SAVED                                                         
         USING SAVED,R3                                                         
*                                                                               
         LA    R4,SRTBK1H                                                       
         LA    R8,SVBKS                                                         
         LA    R0,4                                                             
*                                                                               
BK10     LR    R1,R4               VALIDATE BOOK FIELD                          
         CLI   5(R4),0                                                          
         BNE   BK15                                                             
         OC    0(2,R8),0(R8)       MISSING                                      
         BZ    BK20                                                             
         XC    0(2,R8),0(R8)                                                    
         OI    LCHG,LBK                                                         
         B     BK20                                                             
*                                                                               
BK15     LA    R9,8(R4)                                                         
         GOTO1 DATVAL,DMCB,(2,(R9)),WORK                                        
         OC    0(4,R1),0(R1)                                                    
         BZ    BK90                INVALID DATE                                 
         GOTO1 DATCON,DMCB,(0,WORK),(3,WORK+6)                                  
         CLC   0(2,R8),WORK+6      COMPARE TO OLD VALUE                         
         BE    BK20                                                             
         MVC   0(2,R8),WORK+6      SAVE BOOK YR/MN                              
         OI    LCHG,LBK                                                         
*                                                                               
BK20     LA    R4,SRTBK2H-SRTBK1H(R4)   NEXT BOOK                               
         LA    R8,2(R8)                                                         
         BCT   R0,BK10             DO FOR ALL BOOKS                             
         B     BKX                                                              
*                                                                               
BK90     LR    R2,R4                                                            
         B     INVBOOK             INVALID BOOK                                 
*                                                                               
BKX      CR    R1,R1               SET CC                                       
         B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
* GET DEMO VALUES ROUTINE                                                       
* ROUTINE ALSO EDITS THE PROGRAMS NAMES TO THE HEADLINES                        
*                                                                               
GETDEMS  NTR1                                                                   
         LA    R2,SAVED                                                         
         USING SAVED,R2                                                         
*                                                                               
         XC    SPDEMLK,SPDEMLK                                                  
         LA    RE,SVDEMLK                                                       
         ST    RE,SPLKXTND                                                      
         L     RE,AIO3                                                          
         ST    RE,SPLKAREC                                                      
         MVC   SPLKAFAC,ACOMFACS                                                
         MVC   SPLKAGY,AGENCY                                                   
*****    MVC   SPLKMED,QMED                                                     
*****    CLI   SVAPROF+7,C'C'      TEST CANADIAN AGENCY                         
*****    BNE   *+8                 NO                                           
*****    MVI   SPLKMED,C'C'        MOVE CANADIEN MEDIA                          
         MVC   SPLKMED,SVSELMED                                                 
         MVC   SPLKCLI,QCLT                                                     
*****    MVC   SPLKSRC,BKVALSRC                                                 
         MVC   SPLKSRC,SVSELSRC                                                 
         MVI   SPLKFIL,C'T'                                                     
         MVC   SPLKSTA,QSTA                                                     
         MVC   SPLKUMK,BMKT                                                     
         MVC   SPLKDAY(5),DAYTIME                                               
         MVI   SPLKSVI,X'FF'                                                    
         LA    RE,SVSTORGE                                                      
         ST    RE,SPLKAVAL                                                      
         LA    R0,4                                                             
         LA    R1,SVDEMS                                                        
         ST    R1,SPLKALST                                                      
         LA    R4,SVDEMVAL                                                      
         LA    R8,SVBKS                                                         
         XC    WORK,WORK                                                        
         LA    R9,WORK                                                          
         USING HEDLINED,R9                                                      
*                                                                               
GD40     XC    SVSTORGE(256),SVSTORGE                                           
         XC    SVSTORGE+256(80),SVSTORGE                                        
         OC    0(2,R8),0(R8)                                                    
         BZ    GD45                                                             
         MVC   SPLKDBK,0(R8)                                                    
         GOTO1 VSPDEMLK,DMCB,(X'FF',SPDEMLK)   CALL SPGETDEM                    
*                                                                               
         MVC   0(L'HEDPRG1,R9),SPLKPRG           PROGRAM NAME RETURNED          
GD45     LR    R1,R4                                                            
         LA    RE,SVSTORGE                                                      
         LA    RF,NDEMOS*3                                                      
*                                                                               
GD50     MVC   0(4,R1),0(RE)       SAVE THE DEMO VALUES                         
         LA    R1,4(R1)                                                         
         LA    RE,8(RE)            EXCLUDE SVI VALUES                           
         BCT   RF,GD50                                                          
*                                                                               
         LA    R4,NDEMOS*3*4(R4)                                                
         LA    R8,2(R8)            NEXT BOOK                                    
         LA    R9,HEDPRG2-HEDPRG1(R9)                                           
         BCT   R0,GD40             DO FOR ALL BOOKS                             
*                                                                               
         LA    R9,SRTHD1           MOVE PROGRAM NAMES TO HEADLINES              
         MVC   SRTHD1,WORK                                                      
         XC    HEDPRG1,HEDPRG1                                                  
         XC    HEDPRG3,HEDPRG3                                                  
         OI    SRTHD1H+6,FVOXMT                                                 
         LA    R9,SRTHD2                                                        
         MVC   SRTHD2,WORK                                                      
         XC    HEDPRG2,HEDPRG2                                                  
         XC    HEDPRG4,HEDPRG4                                                  
         OI    SRTHD2H+6,FVOXMT                                                 
         MVC   SRTPRG,SAVPROG                                                   
         OI    SRTPRGH+6,FVOXMT                                                 
*                                                                               
GDX      B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
* DEMO UPGRADE ROUTINE                                                          
*                                                                               
DEMUP    NTR1                                                                   
         L     R3,AIO1                                                          
         USING COMPETE,R3                                                       
         LA    R2,SAVED                                                         
         USING SAVED,R2                                                         
         OC    SVUPGRD,SVUPGRD     TEST UPGRADE EXISTS                          
         BZ    DMX                                                              
         LA    R4,LDEMOVR          BUILD DEMO OVERRIDE LIST                     
         XC    LDEMOVR(NDEMOS*6),LDEMOVR                                        
*                                                                               
         BAS   RE,GTOVRIDE         LOAD DEMO OVERIDES                           
*                                                                               
DM30     LA    R4,SVDEMS           REMOVE OVERRIDE INDICATORS                   
         CLI   0(R4),FF            FROM DEMO LIST                               
         BE    *+16                                                             
         MVI   0(R4),0                                                          
         LA    R4,3(R4)                                                         
         B     *-16                                                             
*                                                                               
         LA    R4,LDMUPBLK         BUILD SPDEMUP BLOCK                          
         USING SPDEMUPD,R4                                                      
         XC    SPDEMUPD(SPDEMUP2),SPDEMUPD                                      
         L     RE,AIO3                                                          
         ST    RE,SPUPAREC                                                      
         MVC   SPUPAFAC,ACOMFACS                                                
         MVC   SPUPAGY,AGENCY                                                   
*****    MVC   SPUPMED,QMED                                                     
*****    CLI   SVAPROF+7,C'C'      TEST CANADIAN AGENCY                         
*****    BNE   *+8                 NO                                           
*****    MVI   SPUPMED,C'C'        MOVE CANADIEN MEDIA                          
         MVC   SPUPMED,SVSELMED                                                 
         MVC   SPUPSTA,QSTA                                                     
         MVC   SPUPDAY(5),DAYTIME                                               
         MVC   SPUPFIL,SVUPFILE                                                 
*****    MVC   SPUPSRC,BKVALSRC                                                 
         MVC   SPUPSRC,SVSELSRC                                                 
         MVC   SPUPFBK,SVUPFRBK                                                 
         LA    R1,LDEMOVR                                                       
         ST    R1,SPUPAOVR                                                      
         MVC   SPUPUDAY,SVUPDAY                                                 
         MVC   SPUPUTIM,SVUPTIM                                                 
         MVC   SPUPTYPE(L'SVUPGRD),SVUPGRD                                      
         XC    SVPROJ(4*3*NDEMOS),SVPROJ                                        
         GOTO1 VSPDEMUP,DMCB,LDMUPBLK,SVDEMS,SVPROJ    CALL SPDEMUP             
*                                                                               
         CLI   SAVPROG,X'00'       YES - MOVE NEW PROGRAM NAME IF NOT           
         BE    DM33                      ALREADY OVERRIDDEN                     
         CLI   SAVPROG,X'FF'       YES - MOVE NEW PROGRAM NAME IF NOT           
         BNE   DM35                      ALREADY OVERRIDDEN                     
DM33     MVC   SAVPROG(L'SPUPPRG),SPUPPRG                                       
         MVC   SRTPRG,SAVPROG                                                   
         OI    SRTPRGH+6,FVOXMT                                                 
*                                                                               
DM35     LA    R4,SVDEMS           SHOW OVERRIDES IN SVPROJ                     
         LA    R8,SVPROJ                                                        
DM40     CLI   0(R4),FF                                                         
         BE    DM60                                                             
         CLI   0(R4),OVERELEM      TEST FOR RATING OVERRIDE                     
         BNE   DM50                                                             
*        CLC   0(4,R8),=XL4'00000000'  BYPASS ZERO RATINGS                      
*        BE    DM50                                                             
*        CLC   8(4,R8),=XL4'00000000'  BYPASS ZERO RATINGS                      
*        BE    DM50                                                             
         L     R0,0(R8)            YES - CALCULTE THE SHARE                     
         SRDA  R0,32                                                            
         M     R0,=F'1000'                                                      
         D     R0,8(R8)                                                         
         ST    R1,4(R8)                  STORE THE OVERRIDE SHARE               
         OI    0(R8),X'80'               RATING OVERRIDE BIT                    
DM50     LA    R4,9(R4)                                                         
         LA    R8,12(R8)                                                        
         B     DM40                                                             
*                                                                               
DM60     BAS   RE,BLDDEMS          BUILD COMPETITION DEMO ELEMENTS              
*                                                                               
DMX      B     XIT                                                              
         DROP  R2                                                               
         DROP  R3                                                               
         DROP  R4                                                               
         EJECT                                                                  
*--BLDDEMS BUILDS THE DEMO ELEMENTS IN THE COMPETITION RECORDS                  
BLDDEMS  NTR1                                                                   
         L     R1,AIO1                                                          
         USING COMPETE,R1                                                       
         LA    R9,SAVED                                                         
         USING SAVED,R9                                                         
*                                                                               
         XC    ELEM,ELEM                                                        
         MVI   ELEM,X'02'                                                       
         LA    R2,ELEM+4                                                        
         LA    R3,CMSSTA                                                        
         LA    R4,1                                                             
         LA    R8,4                                                             
         SR    RE,RE                                                            
         LA    RF,20                                                            
*                                                                               
BLDD050  CLI   0(R3),X'00'                                                      
         BE    BLDD090                                                          
         STC   R4,0(R2)                                                         
         MVC   1(2,R2),=XL2'FFFF'                                               
         CLC   0(1,R2),STANUM                                                   
         BNE   BLDD070                                                          
         LR    RE,R2                                                            
*                                                                               
BLDD070  LA    R2,3(R2)                                                         
         LA    R3,5(R3)                                                         
         LA    R4,1(R4)                                                         
         LA    R8,3(R8)                                                         
         BCT   RF,BLDD050                                                       
*                                                                               
BLDD090  OR    RE,RE                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         STC   R8,ELEM+1           STORE ELEMENT LENGTH                         
         LA    RF,ELEM                                                          
         SR    RE,RF               GET ABSOLUTE ADDRESS                         
         LR    R2,RE               R2 CONTAINS ABSOLUTE ADDRESS                 
*                                                                               
         LA    R3,SVPROJ                                                        
         LA    R8,SVDEMS                                                        
*                                                                               
BLDD120  CLI   0(R8),X'FF'                                                      
         BE    BLDD400                                                          
*                                                                               
         MVC   AIO,AIO1                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,CMDCODEQ     LOOK FOR DEMO ELEMENT                        
         BAS   RE,GETEL                                                         
         BE    BLDD160                                                          
         B     BLDD180                                                          
*                                                                               
BLDD140  BAS   RE,NEXTEL                                                        
         BNE   BLDD180                                                          
*                                                                               
BLDD160  CLC   2(2,R4),1(R8)                                                    
         BNE   BLDD140                                                          
         B     BLDD300                                                          
*                                                                               
BLDD180  MVC   ELEM+2(2),1(R8)                                                  
         LR    RE,R2                                                            
         LA    RE,ELEM(RE)                                                      
         MVC   1(2,RE),2(R3)                                                    
*--ADD THE ELEMENT                                                              
         MVC   AIO,AIO1                                                         
         LA    R4,ELEM                                                          
         GOTO1 ADDELEM                                                          
*--BUMP THE POINTERS                                                            
         LA    R3,12(R3)                                                        
         LA    R8,9(R8)                                                         
         B     BLDD120                                                          
*                                                                               
*--CHANGE ALREADY EXISTING ELEMENTS                                             
*                                                                               
BLDD300  LR    RE,R2                                                            
         AR    RE,R4                                                            
         MVC   1(2,RE),2(R3)                                                    
         TM    0(RE),X'80'                                                      
         BZ    BLDD320                                                          
*        OC    2(2,R3),2(R3)       IF RATING AMOUNT IS ZERO                     
*        BZ    BLDD320             REMOVE OVERRIDE                              
*        NI    0(RE),X'7F'                                                      
*--BUMP THE POINTERS                                                            
BLDD320  LA    R3,12(R3)                                                        
         LA    R8,9(R8)                                                         
         B     BLDD120                                                          
*                                                                               
*--CHANGE THE PROGRAM INFORMATION                                               
*                                                                               
BLDD400  MVC   AIO,AIO1                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,CMPCODEQ     LOOK FOR PROGRAM ELEMENT                     
         BAS   RE,GETEL                                                         
         BE    BLDD540                                                          
*--CREATE PROGRAM ELEMENTS                                                      
         XC    ELEM,ELEM                                                        
         MVC   ELEM(2),=XL2'0314'                                               
         LA    R2,CMSSTA                                                        
         LA    R3,1                                                             
         LA    R8,20                                                            
*                                                                               
BLDD420  CLI   0(R2),X'00'                                                      
         BE    BLDD600                                                          
         STC   R3,ELEM+2                                                        
         CLC   ELEM+2(1),STANUM                                                 
         BNE   BLDD440                                                          
         MVC   ELEM+3(17),SAVPROG                                               
         B     BLDD460                                                          
BLDD440  MVC   ELEM+3(17),=17X'FF'                                              
*--ADD THE ELEMENT                                                              
BLDD460  MVC   AIO,AIO1                                                         
         LA    R2,5(R2)                                                         
         LA    R3,1(R3)                                                         
         LA    R4,ELEM                                                          
         GOTO1 ADDELEM                                                          
         BCT   R8,BLDD420                                                       
         B     BLDD600                                                          
*--CHANGE EXISTING PROGRAM ELEMENTS                                             
BLDD520  BAS   RE,NEXTEL                                                        
         BE    BLDD540                                                          
         DC    H'0'                                                             
*                                                                               
BLDD540  MVC   CKBYTE,2(R4)                                                     
         NI    CKBYTE,X'7F'         TURN OFF OVERRIDE BIT                       
         CLC   CKBYTE,STANUM                                                    
         BNE   BLDD520                                                          
         MVC   3(17,R4),SAVPROG                                                 
*--UPDATE EXTRA INFORMATION ELEMENT                                             
BLDD600  L     R4,AIO1                                                          
         USING CMEEL,R4                                                         
         MVI   ELCODE,CMECODEQ     LOOK FOR DEMO ELEMENT                        
         BAS   RE,GETEL                                                         
         BNE   *+14                                                             
         MVC   CMESRCE,SRTSRC                                                   
         B     BLDDEX                                                           
*--BUILD EXTRA INFORMATION ELEMENT                                              
         LA    R4,ELEM                                                          
         XC    ELEM,ELEM                                                        
*                                                                               
         MVI   CMECODE,CMECODEQ                                                 
         MVI   CMELEN,X'14'                                                     
         MVC   CMESRCE,SRTSRC                                                   
         GOTO1 ADDELEM                                                          
         DROP  R4                                                               
*                                                                               
BLDDEX   LA    R2,SRTSTAH                                                       
         CLC   CMPKLEN,=H'1976'                                                 
         BH    TOOBIG              RECORD IS TOO BIG ERROR                      
         B     XIT                                                              
         DROP  R1                                                               
         DROP  R9                                                               
         EJECT                                                                  
*--BLDDEMDE GETS DEMO INFORMATION FROM COMPETITION                              
*--AND LOADS IT INTO THE DEMO OVERIDE TABLE.                                    
GTOVRIDE NTR1                                                                   
         LA    R8,SAVED                                                         
         USING SAVED,R8                                                         
         L     R9,AIO1                                                          
         USING COMPETE,R9                                                       
*                                                                               
         LA    R2,LDEMOVR                                                       
*                                                                               
         MVC   AIO,AIO1                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,CMDCODEQ     LOOK FOR DEMO ELEMENT                        
         BAS   RE,GETEL                                                         
         BNE   GTOVEX                                                           
         B     GTOV100                                                          
*                                                                               
GTOV050  BAS   RE,NEXTEL                                                        
         BNE   GTOVEX                                                           
*--SEE IF DEMO CODE MATCHES THE DEMOS TO BE LISTED                              
GTOV100  LA    R1,SVDEMS                                                        
         LA    R3,NDEMOS                                                        
*                                                                               
GTOV120  CLC   1(2,R1),2(R4)                                                    
         BE    GTOV200                                                          
         LA    R1,9(R1)                                                         
         BCT   R3,GTOV120                                                       
         B     GTOV050                                                          
*--SET OVERRIDE FIELDS IN DEMO STORAGE AREA                                     
GTOV200  MVI   0(R2),OVERELEM                                                   
         MVI   1(R2),6                                                          
         MVC   2(2,R2),2(R4)                                                    
*--FIND DEMO AMOUNT FOR THE REQUESTED STATION                                   
         SR    R3,R3                                                            
         ICM   R3,1,STANUM                                                      
         BCTR  R3,0                                                             
         LR    R1,R4               MAINTAIN ELEMENT LOCATION                    
         LA    R1,4(R1)                                                         
         CLI   STANUM,1                                                         
         BE    GTOV300                                                          
         BH    GTOV250                                                          
         DC    H'0'                                                             
*                                                                               
GTOV250  LA    R1,3(R1)                                                         
         BCT   R3,GTOV250                                                       
*--LOAD DEMO AMOUNT IN OVERRRIDE TABLE                                          
GTOV300  ZIC   RE,0(R1)                                                         
         NI    0(R1),X'7F'                                                      
         CLC   STANUM,0(R1)                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
         STC   RE,0(R1)                                                         
         TM    0(R1),X'80'                                                      
         BO    GTOV350                                                          
         XC    0(6,R2),0(R2)                                                    
         B     GTOV050                                                          
*                                                                               
GTOV350  MVC   4(2,R2),1(R1)                                                    
         LA    R2,6(R2)                                                         
         B     GTOV050                                                          
*                                                                               
GTOVEX   B     XIT                                                              
         DROP  R8                                                               
         DROP  R9                                                               
         EJECT                                                                  
*                                                                               
*--GETELADD GETS THE PROGRAM AND DEMO ELEMENT LOACATIONS                        
*                                                                               
GETELADD NTR1                                                                   
         LA    R8,SAVED                                                         
         USING SAVED,R8                                                         
         L     R9,AIO1                                                          
         USING COMPETE,R9                                                       
*                                                                               
         XC PRGADDR,PRGADDR                                                     
         XC DEMADDR,DEMADDR                                                     
*                                                                               
         L     R4,AIO1                                                          
         MVI   ELCODE,CMDCODEQ     LOOK FOR DEMO ELEMENT                        
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
GTEL050  CLC   2(2,R4),SVDEMS+1                                                 
         BE    GTEL100                                                          
         BAS   RE,NEXTEL                                                        
         BE    GTEL050                                                          
         DC    H'0'                                                             
GTEL100  LA    R4,4(R4)                                                         
         ST    R4,DEMADDR                                                       
         LA    R3,SVDEMS                                                        
         ST    R3,LDMADDR                                                       
*                                                                               
         CLI   TWADEM,X'FF'                                                     
         BE    GTEL250                                                          
*                                                                               
         CLI   TWADEM,0                                                         
         BE    GTEL250                                                          
*                                                                               
         SR    R1,R1                                                            
         ICM   R1,1,TWADEM                                                      
         L     R2,DEMADDR                                                       
         L     R3,LDMADDR                                                       
*                                                                               
GTEL200  LA    R2,3(R2)            GET NEXT DEMO RECORD                         
         LA    R3,9(R3)                                                         
         BCT   R1,GTEL200                                                       
*                                                                               
         ST    R2,DEMADDR                                                       
         ST    R3,LDMADDR                                                       
*                                                                               
GTEL250  L     R4,AIO1                                                          
         MVI   ELCODE,CMPCODEQ     LOOK FOR PROGRAM ELEMENT                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         ST    R4,PRGADDR                                                       
*                                                                               
         CLI   STANUM,1                                                         
         BE    GETELAEX                                                         
*                                                                               
         SR    R1,R1                                                            
         ICM   R1,1,STANUM                                                      
         BCTR  R1,0                                                             
*                                                                               
GTEL300  BAS   RE,NEXTEL           GET NEXT PROGRAM RECORD                      
         BE    *+6                                                              
         DC    H'0'                                                             
         BCT   R1,GTEL300                                                       
*                                                                               
         MVC   CKBYTE,2(R4)                                                     
         NI    CKBYTE,X'7F'         TURN OFF OVERRIDE BIT                       
         CLC   CKBYTE,STANUM                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         ST    R4,PRGADDR                                                       
*                                                                               
GETELAEX B     XIT                                                              
         DROP  R8                                                               
         DROP  R9                                                               
         EJECT                                                                  
*          DATA SET SPBWS09    AT LEVEL 002 AS OF 03/19/87                      
* INSPECT THE COST FIELD                                                        
* OUTPUT : LCHG = LCOST IF COST FIELD IN RECORD CHANGED                         
*          CC EQ  - COST FOUND AND RECORD CHANGED IF NECESSARY                  
*             LO  - BAD ERROR                                                   
*             HI  - COST NOT FOUND                                              
*                                                                               
COST     NTR1                                                                   
         LA    R2,SRTCSTH                                                       
         CLI   5(R2),0                                                          
         BE    CS90                                                             
         ZIC   RF,5(R2)                                                         
         ST    RF,DMCB+4                                                        
         LA    R9,8(R2)                                                         
         GOTO1 CASHVAL,DMCB,(C'N',(R9))        VALIDATE COST FIELD              
         CLI   DMCB,FF                                                          
         BE    INVCOST                                                          
         CLC   DMCB+4(4),DETCOST                                                
         BE    CSX                                                              
         OI    LCHG,LCOST                                                       
         MVC   DETCOST,DMCB+4                                                   
*                                                                               
         SR    R2,R2                                                            
         LA    R3,100                                                           
         MVC   FULL,DETCOST                                                     
         M     R2,FULL                                                          
         ST    R3,FULL                                                          
*--UPDATE COST ELEMENT IN DETAIL RECORD                                         
         MVC   AIO,AIO2                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,EECCODEQ     LOOK FOR COST ELEMENT                        
         BAS   RE,GETEL                                                         
         BE    CS50                                                             
*--ADD COST ELEMENT                                                             
         XC    ELEM,ELEM                                                        
         MVI   ELEM,X'08'                                                       
         MVI   ELEM+1,EECLENEQ                                                  
         MVC   ELEM+2(2),=X'011E'                                               
         MVC   ELEM+4(4),FULL                                                   
         LA    R4,ELEM                                                          
         MVC   AIO,AIO2                                                         
         GOTO1 ADDELEM                                                          
         B     VREXIT                                                           
*                                                                               
CS50     MVC   4(4,R4),FULL                                                     
         B     VREXIT                                                           
*                                                                               
         B     CSX                                                              
*                                                                               
CS90     CR    RA,RA               = CONDITION NO DATA IN FIELD                 
         B     XIT                                                              
*                                                                               
CSX      CR    R5,R6               NOT = CONDITION DATA IN FIELD                
         B     XIT                                                              
         EJECT                                                                  
* INSPECT UPGRADE FIELD                                                         
* OUTPUT : LCHG = LUPG IF UPGRADE IN RECORD CHANGED                             
*          CC EQ  - UPGRADE NOT FOUND                                           
*             LO  - UPGRADE OK                                                  
*                                                                               
UPGRADE  NTR1                                                                   
*                                                                               
         LA    R2,SRTUPGH                                                       
         CLI   5(R2),0                                                          
         BE    VR200                                                            
*                                                                               
         XC    CONHEAD,CONHEAD                                                  
         GOTO1 VALIUPG,DMCB,BLOCK+256                                           
         OC    CONHEAD,CONHEAD     TEST FOR ERROR MESSAGE                       
         BNZ   SOLONG              YES - LEAVE                                  
         LA    R3,BLOCK+256                                                     
         USING SPDEMUPD,R3                                                      
*                                                                               
         OI    LCHG,LUPG                                                        
         LA    R9,ELEM                                                          
         XC    ELEM,ELEM                                                        
         USING EUPELEM,R9                                                       
         MVI   EUPCODE,EUPCODEQ                                                 
         MVI   EUPLEN,EUPLENEQ                                                  
         MVC   EUPUPFIL,SPUPFIL                                                 
         MVC   EUPUPGRD,SPUPTYPE                                                
         MVC   EUPUPFBK,SPUPFBK                                                 
         MVC   EUPUPINP,SPUPPRG                                                 
*--SAVE UPGRADE VALUES                                                          
         MVC   SVUPFILE,SPUPFIL                                                 
         MVC   SVUPGRD,SPUPTYPE                                                 
         MVC   SVUPFRBK,SPUPFBK                                                 
         MVC   SVUPINP,SPUPPRG                                                  
*--SEE IF UPGRADE ELEMENT EXISTS                                                
         MVC   AIO,AIO2                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,EUPCODEQ     LOOK FOR UPGRADE ELEMENT                     
         BAS   RE,GETEL                                                         
         BNE   VR060                                                            
         MVC   0(29,R4),ELEM                                                    
         B     VR080                                                            
*                                                                               
VR060    MVC   AIO,AIO2                                                         
         LA    R4,ELEM                                                          
         GOTO1 ADDELEM                                                          
         DROP  R9                                                               
*                                                                               
VR080    OC    SPUPUDAY,SPUPUDAY                                                
         BNZ   VR100                                                            
         OC    SPUPUTIM,SPUPUTIM                                                
         BNZ   VR100                                                            
         OC    SPUPSTA,SPUPSTA                                                  
         BZ    VREXIT                                                           
*                                                                               
VR100    LA    R9,ELEM                                                          
         XC    ELEM,ELEM                                                        
         USING EOVELEM,R9                                                       
*                                                                               
         MVI   EOVCODE,EOVCODEQ                                                 
         MVI   EOVLEN,EOVLENEQ                                                  
         MVC   EOVUPDAY,SPUPUDAY                                                
         MVC   EOVUPTIM,SPUPUTIM                                                
         MVC   EOVUPSTA,SPUPSTA                                                 
*--SEE IF DAY TIME STATION ELEMENT EXISTS                                       
         MVC   AIO,AIO2                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,EOVCODEQ     LOOK FOR DAY/TIME ELEMENT                    
         BAS   RE,GETEL                                                         
         BNE   VR130                                                            
         MVC   0(12,R4),ELEM                                                    
         B     VREXIT                                                           
*                                                                               
VR130    MVC   AIO,AIO2                                                         
         LA    R4,ELEM                                                          
         GOTO1 ADDELEM                                                          
         B     VREXIT                                                           
*                                                                               
VR200    CR    RA,RA                                                            
         B     XIT                                                              
*                                                                               
VREXIT   CR    R5,R6                                                            
         B     XIT                                                              
         DROP  R3,R9                                                            
*                                                                               
         EJECT                                                                  
*                                                                               
*--DISUPGD FORMATS UPGRADE EXPRESSION ON SCREEN                                 
*                                                                               
DISUPGD  NTR1                                                                   
         MVI   SRTUPG,C' '           BUILD PERIOD UPGRADE EXPRESSION            
         MVC   SRTUPG+1(L'SRTUPG-1),SRTUPG                                      
         OC    SVUPFILE(27),SVUPFILE                                            
         BZ    DISUPEX                                                          
*                                                                               
         MVC   SRTUPG(4),=C'UPX='                                               
         MVC   SRTUPG+2(1),SVUPFILE                                             
         MVC   SRTUPG+4(16),SVUPINP                                             
*                                                                               
         OC    SVUPFRBK,SVUPFRBK   TEST ANY SHARE BOOK                          
         BZ    DUP050                                                           
         LA    R3,SRTUPG+21                                                     
         CLI   0(R3),C' '          BACK UP TO NON-BLANK                         
         BNE   *+8                                                              
         BCT   R3,*-8                                                           
         MVI   1(R3),C','          EDIT IN COMMA                                
         LA    R3,2(R3)                                                         
         MVC   0(3,R3),=C'BK='                                                  
         GOTO1 DATCON,DMCB,(3,SVUPFRBK),(6,3(R3))                               
*                                                                               
DUP050   OI    SRTUPGH+6,FVOXMT                                                 
*                                                                               
DISUPEX  B     XIT                                                              
         EJECT                                                                  
*                                                                               
*                                                                               
*--CHKSRC READS THE 04 ELEMENT ON THE COMPETITION                               
*--RECORDS AND MOVES IT IN THE HEADLINE.                                        
*                                                                               
CHKSRC   NTR1                                                                   
         L     R4,AIO                                                           
         MVI   ELCODE,CMECODEQ                                                  
         BAS   RE,GETEL                                                         
         BNE   CHKSRCEX                                                         
*                                                                               
         CLI   2(R4),X'40'                                                      
         BNH   CHKSRCEX                                                         
*                                                                               
CHKSRC30 XC    SRTSRC,SRTSRC                                                    
         MVC   SRTSRC(1),2(R4)     COMP SOURCE CODE                             
         MVI   SRTSRCH+5,1         SET LENGTH TO 1                              
         LA    R2,SRTSRCH                                                       
         GOTO1 VALISRC                                                          
*                                                                               
         MVC   DEMOSRC,SRTSRC                                                   
*****    CLI   SVAPROF+7,C'C'      TEST CANADIAN AGENCY                         
*****    BNE   CHKSRCEX            NO                                           
*****    CLI   SRTSRC,C'A'         TEST ARBITRON                                
*****    BNE   *+14                NO                                           
*****    MVC   SRTSRC,=C'BBM'      IT'S CANADIAN ARBITRON                       
*****    B     CHKSRCEX                                                         
*****    MVC   SRTSRC,=C'CSI'      IT'S CANADIAN NIELSON                        
*                                                                               
CHKSRCEX B     XIT                                                              
         EJECT                                                                  
*                                                                               
*--SETHEAD SETS THE HEADLINE UP WHEN PROGRAM IS                                 
*--ENTERED FROM ANOTHER OVERLAY AND NOT THE BASE                                
*                                                                               
SETHEAD  NTR1                                                                   
*                                                                               
         OI    CONSERVH+6,X'81'    SET ON MODIFIED BIT AND XMIT                 
         MVC   SRTCMT(70),=C'ENTER=NEXT SCREEN PF1=COMPETE PF2=DETAIL PX        
               F11=NEXT SELECTION PF12=QUIT'                                    
         OI    SRTCMTH+6,X'80'    XMIT                                          
*                                                                               
         CLI   SELSW,X'FF'                                                      
         BE    XIT                                                              
*                                                                               
         CLC   SVTRESTA,=3X'00'                                                 
         BE    SD05                                                             
         LA    R1,2000                                                          
         L     RF,AIO3                                                          
         L     RE,AIO2                                                          
         MVC   FAKSTA(3),6(RE)     SAVE OLD STATION                             
         MVC   6(3,RE),SVTRESTA    MOVE REAL STATION                            
         MOVE  ((RF),(R1)),(RE)                                                 
*                                                                               
SD05     LA    R2,CONRECH          PROTECT ALL KEY FIELDS                       
         LA    R3,SRTCSTH                                                       
SD10     OI    1(R2),X'20'         PROTECT                                      
         NI    1(R2),X'FE'         UNMODIFY                                     
         ZIC   R0,0(R2)            BUMP                                         
         AR    R2,R0                                                            
         CR    R2,R3                                                            
         BL    SD10                                                             
*--DONT PROTECT DEMO AND MENU FIELDS                                            
         LA    R2,SRTSRCH                                                       
         NI    1(R2),X'DF'                                                      
         LA    R2,SRTMNUH                                                       
         NI    1(R2),X'DF'                                                      
*--SET YEAR FOR VALID NUMERIC                                                   
         LA    R2,SRTYRH                                                        
         OI    4(R2),X'08'                                                      
*                                                                               
         MVC   AIO,AIO2            DETAIL RECORD IS IN IO2                      
         L     R3,AIO                                                           
         USING SIRREC,R3                                                        
*                                                                               
         MVC   PERNUM,SIRKMON                                                   
         NI    PERNUM,X'0F'                                                     
*                                                                               
         XC    WORK,WORK                                                        
         LA    R4,WORK                                                          
         USING STAPACKD,R4                                                      
         MVI   STAPACT,C'U'                                                     
         MVC   STAPAGY,AGENCY                                                   
         MVC   STAPCTRY,SVAPROF+7                                               
         MVC   STAPMED,QMED                                                     
         MVC   STAPMKST,SIRKMS                                                  
         MVC   STAPACOM,ACOMFACS                                                
         GOTO1 VSTAPACK,WORK                                                    
         MVC   SRTSTA,STAPQSTA                                                  
         OI    SRTSTAH+6,X'80'     XMIT                                         
         CLI   SRTSTA,X'F0'        CABLE HEADEND?                               
         BL    *+8                                                              
         MVI   SRTSTA+4,C'/'       YES                                          
*                                                                               
         MVC   SRTDPT,SIRKDPT      DISPLAY DAYPART                              
         OI    SRTDPTH+6,X'80'     XMIT                                         
*                                                                               
         L     R4,AIO                                                           
         MVI   ELCODE,EDPCODEQ     DAY/TIME ELEMENT                             
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING EDPELEM,R4          DISPLAY DAY/TIME                             
         GOTO1 UNDAY,DMCB,EDPDAY,SRTDAY                                         
         OI    SRTDAYH+6,X'80'     XMIT                                         
*                                                                               
         GOTO1 UNTIME,DMCB,EDPTIME,SRTTIME                                      
         OI    SRTTIMEH+6,X'80'    XMIT                                         
         DROP  R4                                                               
*                                                                               
         XC    SRTPER,SRTPER                                                    
         OI    SRTPERH+6,X'80'     XMIT PERIOD NAME                             
*                                                                               
         XC    KEY,KEY             BUILD SCHEME KEY                             
         MVC   KEY(13),0(R3)                                                    
         LA    R3,KEY                                                           
         XC    SIRKMS(9),SIRKMS                                                 
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                LOOK FOR SCHEME KEY                          
         CLC   KEY(13),KEYSAVE                                                  
         BNE   NOSCHEM                                                          
*                                                                               
         MVC   AIO,AIO3            USE IO3 FOR SCHEME RECORD                    
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R4,AIO                                                           
         MVI   ELCODE,EPNCODEQ     LOOK AT PERIOD NAMES ELEMENT                 
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R4,2(R4)            GO PAST OVERHEAD                             
SD60     CLC   PERNUM,0(R4)        TEST MATCH ON PERIOD                         
         BE    *+12                                                             
         LA    R4,5(R4)            NEXT PERIOD                                  
         B     SD60                                                             
         MVC   SRTPER,1(R4)        PERIOD NAME                                  
*                                                                               
         MVC   AIO,AIO2            RESTORE AIO                                  
         L     R3,AIO                                                           
         OI    SRTYRH+6,X'80'      XMIT                                         
         MVC   BYTE,SIRKYEAR       YEAR IN 1'S COMPLEMENT                       
         XI    BYTE,X'FF'                                                       
         ZIC   R1,BYTE                                                          
         CVD   R1,DUB                                                           
         UNPK  SRTYR,DUB                                                        
         OI    SRTYR+1,X'F0'       EBCDIC YEAR                                  
*                                                                               
         OI    SRTSCHH+6,X'80'     XMIT                                         
         OC    SIRKCODE,SIRKCODE   TEST SCHEME 'ALL'                            
         BNZ   *+14                                                             
         MVC   SRTSCH,=C'ALL'                                                   
         B     SD80                                                             
         GOTO1 CLUNPK,DMCB,SIRKCODE,SRTSCH                                      
         DROP  R3                                                               
*                                                                               
SD80     MVI   SRTMNUH+5,0         DISPLAY MENU                                 
*                                                                               
SDEXIT   MVI   SELSW,X'FF'                                                      
         BAS   RE,SETLEN                                                        
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
*--SET THE SCREEN LENGTHS                                                       
*                                                                               
SETLEN   NTR1                                                                   
         LA    R9,SRTBK1H                                                       
         LA    R2,SRTSTAH                                                       
SETL050  LA    RE,8                                                             
         TM    1(R2),X'02'         DOES FIELD HAVE EXTENDED HEADER              
         BZ    SETL070                                                          
         LA    RE,16                                                            
SETL070  SR    R8,R8               FIND LENGTH OF FIELD                         
         ICM   R8,1,0(R2)                                                       
         SR    R8,RE               TOTAL LENGTH-HEADER LENGTH                   
         LA    R1,8(R2)                                                         
         SR    RE,RE                                                            
SETL090  CLI   0(R1),X'00'                                                      
         BE    SETL130                                                          
         CLI   0(R1),X'40'                                                      
         BE    SETL130                                                          
         LA    R1,1(R1)                                                         
         LA    RE,1(RE)                                                         
         BCT   R8,SETL090                                                       
SETL130  STC   RE,5(R2)            STORE LENGTH IN HEADLINE                     
         SR    RE,RE               BUMP TO NEXT FIELD                           
         ICM   RE,1,0(R2)                                                       
         AR    R2,RE                                                            
         CR    R2,R9                                                            
         BL    SETL050                                                          
         B     XIT                                                              
         EJECT                                                                  
         GETEL R4,DATADISP,ELCODE                                               
         SPACE 3                                                                
TRAPERR  GOTO1 ERREX                                                            
         OI    6(R2),X'40'         PUT CURSOR HERE                              
         OI    CONHEADH+6,X'80'    XMIT HEADER                                  
         GOTO1 SAVEUWKA            SAVE SYSD                                    
         L     RD,SAVERD           BACK ALL THE WAY OUT                         
         B     XIT                                                              
         EJECT                                                                  
INVPER   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'INVPERM),INVPERM                                       
         B     SOLONG                                                           
INVPERM  DC    C'* ERROR * INVALID PERIOD NAME *'                               
*                                                                               
NODPT    XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'NODPTM),NODPTM                                         
         LA    R2,SRTDPTH                                                       
         B     SOLONG                                                           
NODPTM   DC    C'* ERROR * DAYPART NOT FOUND *'                                 
*                                                                               
INVSCHE  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'INVSCHEM),INVSCHEM                                     
         B     SOLONG                                                           
INVSCHEM DC    C'* ERROR * INVALID SCHEME NAME *'                               
*                                                                               
NOSCHEM  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'NOSCHEMM),NOSCHEMM                                     
         B     SOLONG                                                           
NOSCHEMM DC    C'* ERROR * SCHEME NOT FOUND *'                                  
*                                                                               
INVYER   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'INVYERM),INVYERM                                       
         LA    R2,SRTYRH                                                        
         B     SOLONG                                                           
INVYERM  DC    C'* ERROR * INVALID YEAR *'                                      
*                                                                               
INVDMO   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'INVDMOM),INVDMOM                                       
         B     SOLONG                                                           
INVDMOM  DC    C'* ERROR * INVALID DEMO *'                                      
*                                                                               
INVBOOK  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'INVBOOKM),INVBOOKM                                     
         B     SOLONG                                                           
INVBOOKM DC    C'* ERROR * INVALID BOOK *'                                      
*                                                                               
INVINPT  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'INVINPTM),INVINPTM                                     
         B     SOLONG                                                           
INVINPTM DC    C'* ERROR * INVALID INPUT *'                                     
*                                                                               
INVDET   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'INVDETM),INVDETM                                       
         B     SOLONG                                                           
INVDETM  DC    C'* ERROR * NO DETAIL RECORD FOR THIS REQUEST'                   
*                                                                               
NORECS   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'NORECSM),NORECSM                                       
         B     SOLONG                                                           
NORECSM  DC    C'NO RECORDS EXIST FOR THIS REQUEST'                             
*                                                                               
INVDMN   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'INVDMNM),INVDMNM                                       
         B     SOLONG                                                           
INVDMNM  DC    C'INVALID DEMO MENU CODE'                                        
*                                                                               
NOMENU   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'NOMENUM),NOMENUM                                       
         B     SOLONG                                                           
NOMENUM  DC    C'NO DEFAULT DEMO MENU INPUT REQUIRED'                           
*                                                                               
INVCOST  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'INVCOSTM),INVCOSTM                                     
         B     SOLONG                                                           
INVCOSTM DC    C'INVALID COST MUST BE NUMERIC NON ZERO NUMBER'                  
*                                                                               
NODATA   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'NODATAM),NODATAM                                       
         B     SOLONG                                                           
NODATAM  DC    C'NO DATA TO DISPLAY CHECK DEFAULT BOOKS'                        
*                                                                               
TOOBIG   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'TOOBIGM),TOOBIGM                                       
         B     SOLONG                                                           
TOOBIGM  DC    C'* ERROR * NO NEW STATIONS OR DEMOS CAN BE ADDED'               
*                                                                               
NOSTAT   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'NOSTATM),NOSTATM                                       
         B     SOLONG                                                           
NOSTATM  DC    C'* ERROR * REQUESTED STATION DOES NOT EXIST IN RECORD'          
*                                                                               
SOLONG   GOTO1 ERREX2                                                           
         OI    6(R2),X'40'         PUT CURSOR HERE                              
         OI    CONHEADH+6,X'80'    XMIT HEADER                                  
         GOTO1 SAVEUWKA            SAVE SYSD                                    
         L     RD,SAVERD           BACK ALL THE WAY OUT                         
         B     XIT                                                              
         EJECT                                                                  
         LTORG                                                                  
* CONSTANTS                                                                     
*                                                                               
TEMPSTR  DC    CL7'TEMPSTR'                                                     
DMREAD   DC    CL7'DMREAD '                                                     
DMWRITE  DC    CL7'DMWRT  '                                                     
MAJBKS   DC    XL4'0205070B'                                                    
BLANKS   DC    CL80' '                                                          
CMT1     DC    CL19'ENTER=NEXT PF1=COMP'                                        
CMT2     DC    CL8'PF2=TRAN'                                                    
CMT3     DC    CL36'PF4=FRST PF5=PREV PF6=NEXT PF12=QUIT'                       
*                                                                               
NDEMOS   EQU   14                  MAX STATIONS                                 
NSTASCR  EQU   7                   STATIONS/SCREEN                              
FF       EQU   X'FF'                                                            
OVERELEM EQU   X'DE'                                                            
         EJECT                                                                  
*        PRINT OFF                                                              
         PRINT NOGEN                                                            
       ++INCLUDE DDSPOOLD                                                       
       PRINT GEN                                                                
       ++INCLUDE DDSPLWORKD                                                     
       PRINT NOGEN                                                              
       ++INCLUDE SPSFMFFD                                                       
         PRINT ON                                                               
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SPSFMB7D                                                       
********************************************************************            
** CAREFUL NOT TO GO OVER X'E00' FOR STORAGE IN SCREEN - IT'S GENCON'S          
********************************************************************            
         ORG   SRTWORK                                                          
SAVERD   DS    F                                                                
STAADDR  DS    F                                                                
LDMADDR  DS    F                                                                
DEMADDR  DS    F                                                                
PRGADDR  DS    F                                                                
PJRADDR  DS    F                                                                
SAVEKEY  DS    XL13                THE DETAIL KEY                               
COMPKEY  DS    XL13                                                             
SAVPROG  DS    XL17                                                             
DETCOST  DS    XL4                 DETAIL RECORD COST VALUE                     
PERBOOKS DS    XL8                                                              
FAKSTA   DS    XL3                 PHONEY STATION TO GET THROUGH VK             
LOOPCT   DS    X                                                                
PERNUM   DS    X                   PERIOD                                       
CURYEAR  DS    X                   CURRENT YEAR                                 
SCRYEAR  DS    X                   SCREEN YEAR                                  
COMPER   DS    X                   Y/M COMPETION RECORD                         
PERSTART DS    XL3                 PERIOD START DATE (YMD)                      
PEREND   DS    XL3                 PERIOD END DATE (YMD)                        
DAYPART  DS    C                   DAYPART                                      
DAYTIME  DS    0XL5                                                             
DAY      DS    X                   DAY(S) CODE                                  
TIME     DS    XL4                 START/END TIMES                              
SCHCLI   DS    CL2                 SCHEME CLIENT CODE                           
PROGTYPE DS    C                   PROGRAM TYPE                                 
LNDEMSCR DS    X                                                                
STANUM   DS    X                   DISPLACEMNT OF STATION IN COMP REC           
CKBYTE   DS    X                                                                
YEAR     DS    X                   YEAR                                         
INDEX    DS    X                   SEQUENCE NO. FOR SORTING                     
SEQNUM   DS    X                   SEQUENCE NO. FOR DAY/TIME                    
PERUPG   DS    CL34                PERIOD UPGRADE EXPRESSION                    
SVDEFUPG DS    CL34                SAVE DEFAULT UPGRADE VALUES                  
DEMWRK   DS    CL40                                                             
COSTWRK  DS    CL4                                                              
LASTDATE DS    XL3                                                              
PRGTYPSV DS    CL170               PROGRAM TYPE SAVE (CODE+NAME)                
DEMOTYP  DS    XL2                                                              
HLDBOOK  DS    XL2                 STATION LOOKUP BOOK                          
DEMOSRC  DS    XL1                 DEMO SOURCE                                  
ADDFLAG  DS    C                   'Y' IF ADDREC, 'N' IF PUTREC                 
TWADEM   DS    X                                                                
NDETSW   DS    X                                                                
MORESW   DS    X                                                                
RECSW    DS    X                                                                
FSTSW    DS    X                                                                
SELSW    DS    X                                                                
NODEMSW  DS    X                   COMPETITION EXISTS WITHOUT DEMO EL           
STCHG    DS    X                                                                
LABUPEL  DS    X                   UPGRADE ELEMENT SWITCH                       
*                                                                               
BCHUF    DS    C                                                                
BCHUP    DS    CL8                                                              
BCHFB    DS    CL2                                                              
BCHUPIN  DS    CL16                                                             
*                                                                               
LAPROJ   DS    A                                                                
LANDX    DS    A                                                                
LABOVEL  DS    A                                                                
LABDMEL  DS    A                                                                
LATWA    DS    A                                                                
LASAVE   DS    A                                                                
LOLDSHR  DS    F                                                                
LDNAME   DS    CL7                                                              
LDEM     DS    XL4                                                              
LHDRDA   DS    XL4                                                              
LDEMOVR  DS    (NDEMOS)XL6                                                      
LNSTASCR DS    XL1                                                              
LBATCH   DS    XL3                                                              
LDMUPBLK DS    (SPDEMUP2)X                                                      
*                                                                               
LCHG     DS    X                                                                
LCOST    EQU   X'80'                                                            
LUPG     EQU   X'40'                                                            
LBK      EQU   X'20'                                                            
LPRG     EQU   X'10'                                                            
LRTG     EQU   X'08'                                                            
LSHP     EQU   X'04'                                                            
LHDR     EQU   X'02'                                                            
LPROJ    EQU   X'01'                                                            
*                                                                               
*--SCREEN HEADER DEFAULTS                                                       
FVIHDR   DS    0XL8                ** EXTRACTED INPUT FIELD HEADER **           
FVTLEN   DS    XL1                 L'FIELD HEADER+L'FIELD                       
FVATRB   DS    XL1                 FIELD ATTRIBUTE                              
FVAPROT  EQU   X'20'               PROTECTED FIELD                              
FVAHIGH  EQU   X'08'               HIGH INTENSITY                               
FVAXTND  EQU   X'02'               EXTENDED HEADER                              
FVAMODF  EQU   X'01'               MODIFIED INPUT FIELD                         
FVABSA   DS    XL2                 SCREEN ABSOLUTE ADDRESS                      
FVIIND   DS    XL1                 INPUT VALIDATION INDICATORS                  
FVITHIS  EQU   X'80'               FIELD INPUT THIS TIME                        
FVILAST  EQU   X'40'               FIELD HAS BEEN INPUT PREVIOUSLY              
FVIVAL   EQU   X'20'               FIELD HAS BEEN VALIDATED PREVIOUSLY          
FVIINV   EQU   X'10'               FIELD IS INVALID                             
FVINUM   EQU   X'08'               FIELD IS VALID NUMERIC                       
FVIALF   EQU   X'04'               FIELD IS VALID ALPHA                         
FVIHEX   EQU   X'02'               FIELD IS VALID HEXADECIMAL                   
FVILEN   DS    XL1                 ACTUAL INPUT DATA LENGTH                     
FVOIND   DS    XL1                 OUTPUT INDICATORS                            
FVOXMT   EQU   X'80'               TRANSMIT FIELD                               
FVOCUR   EQU   X'40'               INSERT CURSOR TO FIELD                       
FVXLEN   DS    XL1                 ACTUAL INPUT DATA LENGTH-1                   
FVIFLD   DS    CL20                                                             
*--OTHER EQUATES                                                                
NDEMSCR  EQU   6                                                                
*                                                                               
DETADDR  DS    20CL5                                                            
         EJECT                                                                  
********************************************************************            
** CAREFUL NOT TO GO OVER X'E00' FOR STORAGE IN SCREEN - IT'S GENCON'S          
********************************************************************            
*        PRINT OFF                                                              
       ++INCLUDE SPSFMWORKD                                                     
         PRINT ON                                                               
         SPACE 5                                                                
* WORK AREA                                                                     
*                                                                               
         ORG   SYSSPARE+1024       DON'T CREAM T2170E STORAGE                   
       ++INCLUDE DEDBLOCK                                                       
         EJECT                                                                  
VDEMAND  DS    F                                                                
VDEMOVAL DS    F                                                                
VEDITOR  DS    F                                                                
VSPDEMUP DS    F                                                                
VSPDEMLK DS    F                                                                
*                                                                               
       ++INCLUDE SPDEMLK                                                        
         EJECT                                                                  
       ++INCLUDE DDEBLOCK                                                       
         EJECT                                                                  
*-- SAVE AREA                                                                   
SAVED    DS    0H                                                               
SVSELMED DS    C                                                                
SVSELSRC DS    C                                                                
SVBKS    DS    XL8                                                              
SVDEMNM  DS    XL4                                                              
SVDEMNO  DS    X                                                                
SVDEMS   DS    (NDEMOS*3)XL3                                                    
         DS    X                                                                
SVDEMVAL DS    (NDEMOS*4*3)XL4                                                  
SVPROJ   DS    (NDEMOS*3)XL4                                                    
SVDEMNAM DS    (NDEMOS)XL7                                                      
         DS    X                                                                
SVSTORGE DS    (NDEMOS*3)XL8                                                    
SVDEMLK  DS    XL32                                                             
         ORG                                                                    
*--ADDRESS CANNOT EXCEED X'1950'                                                
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE SPDEMUPD                                                       
         EJECT                                                                  
       ++INCLUDE DDCOMFACS                                                      
         EJECT                                                                  
         PRINT ON                                                               
         EJECT                                                                  
HEDLINED DSECT                                                                  
HEDPRG1  DS    CL12                PROGRAM NAME 1                               
         DS    CL1                                                              
HEDPRG2  DS    CL12                PROGRAM NAME 2                               
         DS    CL1                                                              
HEDPRG3  DS    CL12                PROGRAM NAME 3                               
         DS    CL1                                                              
HEDPRG4  DS    CL12                PROGRAM NAME 4                               
         SPACE 2                                                                
LINE1D   DSECT                                                                  
L1RTG1   DS    CL10                                                             
         DS    X                                                                
L1RTG2   DS    CL12                                                             
         DS    X                                                                
L1RTG3   DS    CL12                                                             
         DS    X                                                                
L1RTG4   DS    CL12                                                             
         SPACE  2                                                               
LINE2D   DSECT                                                                  
         DS    XL5                                                              
L2SHP1   DS    CL12                                                             
         DS    X                                                                
L2SHP2   DS    CL12                                                             
         DS    X                                                                
L2SHP3   DS    CL12                                                             
         DS    X                                                                
L2SHP4   DS    CL12                                                             
         EJECT                                                                  
*                                                                               
SIRRECD  DSECT                                                                  
       ++INCLUDE SPGENSIR                                                       
         SPACE 4                                                                
COMPETE  DSECT                                                                  
       ++INCLUDE SPGENCOMP                                                      
         SPACE 4                                                                
COMSTAT  DSECT                                                                  
       ++INCLUDE SPGENCLST                                                      
         SPACE 4                                                                
       ++INCLUDE SPGENDMN                                                       
         SPACE 4                                                                
* DEDEMFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE DEDEMFILE                                                      
         PRINT ON                                                               
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'005SPSFM17   05/26/04'                                      
         END                                                                    
