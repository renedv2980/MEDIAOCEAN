*          DATA SET SPTRA00    AT LEVEL 214 AS OF 04/17/19                      
*PHASE T21600C    A FOR NET NAV/C FOR SPOT/LINK                                 
*INCLUDE KHDUMMY                                                                
         TITLE 'T21600 - SPOT TRAFFIC SPOOL CONTROLLER'                         
***********************************************************************         
* LEV 149 BGRI APR06/04 ADD LOCK BY NET FOR PAT GEN                   *         
* LEV 150 SMUR FEB09/04 BUILD TRUE PRD LIST FOR BRAND LEVEL SEC CLT   *         
*         BGRI APR16/04 ACTIVATE SPOT PRODUCT FOR TALENT              *         
* LEV 150 SMUR FEB09/04 BUILD TRUE PRD LIST FOR BRAND LEVEL SEC CLT   *         
* LEV 153 BGRI JUL16/04 ADD CHECK READ ONLY FILE CODE                 *         
* LEV 154 BGRI AUG04/04 BYPASS SOX OFFLINE                            *         
* LEV 156 BGRI SEP03/04 CLEAR SVBCLTH FIELD IN VMEDIA RTN             *         
* LEV 158 BGRI JAN13/05 ADD LEN 25 TO NETPAK SPOT LEN TABLE           *         
* LEV 159 SMUR FEB22/05 ADD LEN 80 TO NETPAK SPOT LEN TABLE           *         
* LEV 160 BGRI APR14/05 ADD LEN 70 TO NETPAK SPOT LEN TABLE           *         
* LEV 161 BGRI JUN15/05 ADD MORE PRODUCTS FOR NETPAK (MORE THAN 253)  *         
* LEV 162 SMUR OCT10/05 OFFICER TO VALIDATE CLIENT OFFICE             *         
* LEV 163 SMUR JAN22/06 USE SPSLENTAB (IN CORE) TO VALIDATE SPOT LEN  *         
* LEV 164 MBLU SEP21/06 ADD STRANS REPORT                             *         
* LEV 163 BGRI NOV01/05 CHA COMMENT PRG FOR NET TO 52, SAME SCREEN    *         
*                       AND GET PROF TO INCLUDED PROGRAM              *         
* LEV 164 KWAN JAN24/06 IO ROUTINE                                    *         
*         SMUR          SAVE CPROF+6 FOR ALHANUMERIC PRINT OF CLT     *         
*         SMUR          LOCKING AND 3 CHAR PRODUCT                    *         
* LEV 164 MNAS FEB06/07 COMBINE MORE BRANDS AND 6K WITH A LITTLE      *         
*                       AUTOGEN WITH AMS OPTION/SPOT TRAFFIC FIX      *         
*                       ON THE SIDE                                   *         
* LEV 167 MNAS FEB27/07 ADD IN RECLAIMING SOME COMMON STORAGE         *         
* LEV 172 MNAS OCT23/07 FIX IN VMEDIA FOR AUTOGE/ASMGEN FEATURE       *         
* LEV 176 MNAS FEB25/08 CODE TO PROCESS GRIDS                         *         
* LEV 177 SMUR APR10/08 BPAT, BINST RECORDS                           *         
* LEV 178 SMUR JUN16/08 SPOT LIST WITH GRIDS                          *         
* LEV 179 MNAS JUL01/08 FORCE PU(5)=C (FULLY CONVERTED MORE BRANDS)   *         
*                       UNLESS SJ WHERE WE READ DOWN TO CLI LEV PROF  *         
* LEV 180 SMUR JUL08/08 COMTEXT FOR NET                               *         
* LEV 185 SMUR SEP17/09 FIX BAD ERROR EXIT                            *         
* LEV 186 SMUR OCT26/09 ADD PGRLIST NEW RECORD DEFINITION             *         
* LEV 189 MNAS APR16/10 1) ADD PRODDEL NEW RECORD DEFINITION AND CODE *         
*                       FOR DDS ONLY OVERLAYS                         *         
*                       2) DISABLE 'D' SELECT CODE FROM GENCON LIST   *         
*                       FOR ALL OVERLAYS                              *         
* LEV 197 MNAS NOV15/11 CHANGES TO ACCOMODATE NEW VENDOR RECORD       *         
*         (REPLACING DAYPART) - BUG FIX FOR FEEDS ALSO INCLUDED       *         
* LEV 199 MNAS MAR21/12 FIX PFKEY BUGS IN SPOT COMMERCIAL             *         
*         CONTINUE WHEN NO FAX FOUND FOR PAT/GEN AND CAB/GEN          *         
* LEV 200 MNAS MAR21/12 BUG FIX TO PID DISPLAY PROTECT/INVISIBLE      *         
* LEV 201 SMUR FEB06/13 SET READ FOR UPDATE TO N BEFORE DMGR CALLS    *         
* LEV 204 MNAS FEB20/13 NEW ENTRY FOR NEW SPOT FILE Z                 *         
* LEV 206 SMUR NOV26/13 REPLACE SYSTBL TABLE WITH DDNAME CALL WHEN    *         
*                       SWITCHING BETWEEN SPOT AND TRAFFIC            *         
* LEV 207 KWAN NOV03/16 ENABLE DDLINK FOR TRAFFIC                     *         
* LEV 208 JBAS JAN03/16 ENABLE GLOBBER CALL FOR COMTEXT               *         
* SPEC-34900  SMUR FIX PROTECTION DUMP ON ERROR EXIT                  *         
*                                                                     *         
***********************************************************************         
         SPACE                                                                  
T21600   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 5500,**T21600,R7,RR=R2,CLEAR=Y                                   
         SPACE                                                                  
         LR    R9,R1                                                            
         LR    R8,RC                                                            
         USING SPOOLD,R8                                                        
*                                                                               
         LA    RC,SPOOLEND                                                      
         USING GEND,RC                                                          
         ST    R9,SYSPARMS                                                      
         ST    RB,SYSRB            SYSTEM BASE REGS                             
         ST    RD,SYSRD                                                         
*                                                                               
         L     RA,4(R9)            A(TWA)                                       
         ST    RA,ATWA                                                          
         USING CONHEADH-64,RA                                                   
         LA    R9,IO                                                            
         AHI   R9,18024            GRABBING 3 6000 BYTE I/O AREAS               
*                                  NEED SPACE FOR 3 8 BYTE LABELS               
         USING SYSD,R9                                                          
         ST    RC,SVADGEND                                                      
         ST    R2,SPTR00RR                                                      
*                                  NEED SPACE FOR 3 8 BYTE LABELS               
*GRID                                                                           
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -         
         BRAS  RE,CHKGRID          CHECK FOR GRIDS TURNED ON                    
*        BNE   XIT                                                              
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -         
*GRID                                                                           
         BRAS  RE,SYSINIT          INITIALIZE SYSTEM DEPENDENT                  
*                                                                               
GOON     L     RE,SYSPARMS                                                      
         L     RF,0(,RE)           A(TIOB)                                      
         ST    RF,ATIOB                                                         
         USING TIOBD,RF                                                         
         ZIC   R0,TIOBAID          NUMBER OF PFKEY PRESSED                      
         CHI   R0,12                                                            
         BNH   *+8                                                              
         AHI   R0,-12                                                           
         STC   R0,PFKEY            SAVE ADJUSTED PFKEY VALUE                    
         DROP  RF                                                               
         SPACE                                                                  
*                                                                               
         OC    TWAVPRNT,TWAVPRNT   IF OFFLINE                                   
         JNZ   GOON4               THEN SKIP DDLINK STUFF                       
         L     R2,SYSPARMS         TEST IF CALLED FROM DDLINK UPLOAD            
         L     R2,16(R2)           R2=A(COMFACS)                                
         L     RF,CGLOBBER-COMFACSD(R2)                                         
         GOTO1 (RF),DMCB,=C'GETD',BLOCK,10,GLVDLUWF                             
         CLI   8(R1),0             WORKER FILE GLOBAL NOT PRESENT               
         JNE   GOON4                                                            
*                                                                               
         L     R1,SYSPARMS                                                      
         L     RE,8(R1)            A(SYSLIST)                                   
         L     RF,4(RE)            A(CALLOV)                                    
         ST    RF,CALLOV                                                        
*                                                                               
         MVI   DMCB+7,QCLPACK      GET A(CLPACK) AND A(CLUNPK)                  
         BRAS  RE,SPCALLOV                                                      
         MVC   CLPACK,DMCB                                                      
         MVI   DMCB+7,QCLUNPK                                                   
         BRAS  RE,SPCALLOV                                                      
         MVC   CLUNPK,DMCB                                                      
*                                                                               
         MVI   DMCB+7,QSTAVAL                                                   
         BRAS  RE,SPCALLOV                                                      
         MVC   STAVAL,DMCB                                                      
         MVI   DMCB+7,QSTAPACK                                                  
         BRAS  RE,SPCALLOV                                                      
         MVC   STAPACK,DMCB                                                     
*                                                                               
         LA    R1,FCMSPACK         INTERCEPT FOR MSPACK                         
         ST    R1,MSPACK                                                        
         LA    R1,FCMSUNPK                                                      
         ST    R1,MSUNPK                                                        
*                                                                               
         L     R1,SYSPARMS                                                      
         L     R1,16(,R1)          COMFACS                                      
         USING COMFACSD,R1                                                      
         L     RF,CGETFACT                                                      
         DROP  R1                                                               
         GOTO1 (RF),DMCB,0                                                      
         L     RE,DMCB             FAFACTS                                      
         USING FACTSD,RE                                                        
         L     R1,FASYSLST                                                      
         LARL  RF,RECACTS          SPOT RECORD/ACTION DIRECTORY                 
         MVI   SPOTNETF,C'S'       SET FLAG TO SPOT                             
         CLI   FAOVSYS,02                                                       
         BE    GOON2                                                            
         CLI   FAOVSYS,13                                                       
         BE    GOON2                                                            
         LA    RF,RECACTN          NET RECORD/ACTION DIRECTORY                  
         MVI   SPOTNETF,C'N'       SET FLAG TO NETWORK                          
         CLI   FAOVSYS,03                                                       
         BE    GOON2                                                            
         DC    H'00'                                                            
GOON2    ST    RF,ARECACT                                                       
         L     RF,CGENNEW-COMFACSD(R2)                                          
         MVI   GENSTAT6,GES$UPLD   ENABLE DDLINK AUTO UPLOAD                    
         MVI   MODE,TESTGLOB                                                    
         GOTO1 (RF),DMCB,SPOOLD                                                 
         DC    H'0'                GENNEW SHOULD RETURN TO MONITOR              
         DROP  RE                                                               
*                                                                               
GOON4    DS    0H                                                               
         CLC   =C'PFM',CONREC                                                   
         BNE   GOON6                                                            
         CLI   T216FFD+1,C'*'        TEST DDS TERMINAL                          
         BNE   GOON8                 NO                                         
         BRAS  RE,PFMXFR                                                        
         B     XIT                                                              
*                                                                               
GOON6    CLC   =C'AUTOPDF',CONREC                                               
         BE    GOON6X                                                           
         CLC   =C'AMSPDF',CONREC                                                
         BE    GOON6X                                                           
         CLC   =C'NINSPDF',CONREC                                               
         BNE   GOON8                                                            
*                                                                               
GOON6X   L     RE,TWAMASTC                                                      
         MVI   MCREQREP-MASTD(RE),C'N'  SUPPRESS REQUEST DETAILS                
*                                                                               
GOON8    CLI   TWASCR,X'82'        IS TEXT MAINTENANCE SCREEN LOADED?           
         BE    *+12                                                             
         CLI   TWASCR,X'FE'        IS TEXT MAINTENANCE SCREEN LOADED?           
         BNE   GOON10                                                           
*                                                                               
         CLC   =C'TEX',CONREC      RECORD TEXT                                  
         BNE   GOON40                                                           
         B     CHACHA                                                           
*                                                                               
GOON10   CLI   TWASCR,X'B3'        IS DTEXT MAINTENANCE SCREEN LOADED?          
         BNE   GOON14                                                           
         CLC   =C'DTE',CONREC      RECORD DTEXT                                 
         BE    CHACHA                                                           
         CLC   =C'STE',CONREC      RECORD STEXT                                 
         BNE   GOON40                                                           
         B     CHACHA                                                           
*                                                                               
GOON14   CLI   TWASCR,X'E1'        COMTEXT MAINTENANCE SCREEN LOADED?           
         BE    *+12                                                             
         CLI   TWASCR,X'D1'        OR COMTEXT LIST                              
         BNE   GOON20                                                           
*                                                                               
         CLI   PFKEY,6                                                          
         BNE   GOON16              CHANGE RECORD TO COMML                       
         XC    CONREC,CONREC                                                    
         NI    CONRECH+4,X'DF'                                                  
         MVI   CONRECH+5,5                                                      
         MVC   CONREC(5),=C'COMML'                                              
         XC    CONACT,CONACT                                                    
         MVC   CONACT(4),=C'DISP'                                               
         NI    CONACTH+4,X'DF'                                                  
         MVI   CONACTH+5,4                                                      
         B     GOON40                                                           
*                                                                               
GOON16   CLC   =C'COM',CONREC                                                   
         BNE   GOON40                                                           
         B     CHACHA                                                           
*                                                                               
GOON20   CLI   TWASCR,X'84'        IS TEXT MAINTENANCE SCREEN LOADED?           
         BNE   GOON30                                                           
         CLC   =C'TEX',CONREC      RECORD ESCAPE?                               
         BNE   GOON40                                                           
         B     CHACHA                                                           
*                                                                               
GOON30   CLI   TWASCR,X'83'        IS COMMENT MAINT SCREEN LOADED?              
         BNE   GOON32                                                           
         CLC   =C'COMMENT',CONREC  RECORD COMMENT?                              
         BNE   GOON40                                                           
         B     CHACHA                                                           
*                                                                               
GOON32   DS    0H                                                               
         CLI   TWASCR,X'DF'                                                     
         BE    *+20                                                             
         CLI   TWASCR,X'F4'        TEST NET COMML SCREEN LOADED                 
         BE    *+12                                                             
         CLI   TWASCR,X'F2'        TEST STR COMML SCREEN LOADED                 
         BNE   GOON40                                                           
         CLI   PFKEY,6             TEST SWITCH TO COMTEXT                       
         BNE   GOON40                                                           
         BRAS  RE,SETCOMT                                                       
         B     GOON40                                                           
*                                                                               
CHACHA   CLC   =C'SEL',CONACT      ACTION SELECT?                               
         BNE   GOON40                                                           
         CLI   THISLSEL,C'C'       'C' ENTERED AS LIST SELECTION?               
         BNE   GOON40                                                           
         CLI   PFKEY,3             ERASE A LINE?                                
         BE    CHACHA10                                                         
         CLI   PFKEY,4             INSERT A LINE?                               
         BNE   GOON40                                                           
CHACHA10 MVC   CONACT(3),=C'CHA'   FORCE ACTION TO CHANGE                       
*                                                                               
GOON40   BRAS  RE,INIT                                                          
         LA    R1,FCMSPACK         INTERCEPT FOR MSPACK                         
         ST    R1,MSPACK                                                        
         LA    R1,FCMSUNPK                                                      
         ST    R1,MSUNPK                                                        
*                                                                               
GOON52   MVI   SVAMSAUT,C'N'                                                    
         MVI   DMCB+7,QGENCON                                                   
         BAS   RE,SPCALLOV                                                      
*                                                                               
         L     RF,DMCB             PICK UP A(GENERAL CONTROLLER)                
         GOTO1 (RF),DMCB,(R8)      GO THERE - PASS A(WORKING STORAGE)           
*                                                                               
         CLI   SVAMSAUT,C'Y'       TEST TO RUN AMS/GEN                          
         BE    *+12                                                             
         BRAS  RE,CHKSCRN          CHECK SCREEN FOR TALCOM LIST                 
         B     XIT                 THEN WE'RE THROUGH                           
*                                                                               
         XC    CONREC,CONREC                                                    
         MVC   CONREC(3),=C'AMS'                                                
         MVI   CONRECH+5,3                                                      
         NI    CONRECH+4,X'DF'                                                  
         MVI   PQSW,0              SET TO OPEN PRTQUE AGAIN                     
         GOTO1 (RF),DMCB,(R8)      CALL GENCON AGAIN!                           
         B     XIT                 THEN WE'RE THROUGH                           
*                                                                               
SPCALLOV NTR1                                                                   
         XC    DMCB(7),DMCB                                                     
         MVI   DMCB+4,X'D9'        BUILD X'D9000A..' IN DMCB+4                  
         MVI   DMCB+6,X'0A'                                                     
         L     RF,CALLOV                                                        
         GOTO1 (RF),DMCB                                                        
         CLI   4(R1),X'FF'                                                      
         JNE   XIT                                                              
         DC    H'0'                                                             
         EJECT                                                                  
* INTERCEPTED MSPACK ROUTINE - USES STAPACK *                                   
         SPACE                                                                  
         DS    0H                                                               
FCMSPACK NTR1  BASE=SYSRB                                                       
         LA    R7,4095(,RB)                                                     
         LA    R7,1(,R7)                                                        
         ST    RD,COMMRD                                                        
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         SPACE                                                                  
         LM    R4,R6,0(R1)         GET A(MKT)/A(STA)/A(MKTSTA)                  
         SPACE                                                                  
         LA    R1,BIGKEY                                                        
         XC    BIGKEY,BIGKEY                                                    
         USING STAPACKD,R1                                                      
         MVI   STAPACT,C'P'                                                     
         MVC   STAPAGY,AGENCY                                                   
         MVC   STAPCTRY,SPOTCAN                                                 
         MVC   STAPMED,QMED                                                     
         MVC   STAPACOM,ACOMFACS                                                
         MVC   STAPQMKT,0(R4)      MARKET                                       
         MVC   STAPQSTA(8),0(R5)   STATION                                      
*                                                                               
         CLI   SPOTCAN,C'C'        IF CANADIAN                                  
         BNE   *+10                                                             
         OC    STAPQNET,SPACES     THEN SPACES                                  
*                                                                               
         GOTO1 STAPACK,(R1)                                                     
         CLI   STAPERR,0                                                        
         BNE   STAERR                                                           
         MVC   0(5,R6),STAPMKST    RETURN RESULT                                
         B     XIT                                                              
         DROP  R1                                                               
         SPACE 3                                                                
* INTERCEPTED MSPACK ROUTINE - USES STAPACK *                                   
         SPACE                                                                  
         DS    0H                                                               
FCMSUNPK NTR1  BASE=SYSRB                                                       
         LA    R7,4095(,RB)                                                     
         LA    R7,1(,R7)                                                        
         ST    RD,COMMRD                                                        
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         SPACE                                                                  
         LM    R4,R6,0(R1)         GET A(MKT)/A(STA)/A(MKTSTA)                  
         SPACE                                                                  
         LA    R1,BIGKEY                                                        
         XC    BIGKEY,BIGKEY                                                    
         USING STAPACKD,R1                                                      
         MVI   STAPACT,C'U'                                                     
         MVC   STAPAGY,AGENCY                                                   
         MVC   STAPCTRY,SPOTCAN                                                 
         MVC   STAPMED,QMED                                                     
         MVC   STAPACOM,ACOMFACS                                                
         MVC   STAPMKST,0(R4)      MARKET/STATION                               
*                                                                               
         MVC   STAPQNET,SPACES                                                  
*                                                                               
         GOTO1 STAPACK,(R1)                                                     
         CLI   STAPERR,0                                                        
         BNE   STAERRA                                                          
*        BE    *+6                                                              
*        DC    H'0'                                                             
         MVC   0(4,R5),STAPQMKT    RETURN RESULT                                
         MVC   0(8,R6),STAPQSTA    ALL CONVERTED FOR 8 BYTE STATION             
         OC    5(3,R6),SPACES                                                   
         SPACE                                                                  
         CLI   4(R6),C'/'          GET RID OF /                                 
         BNE   *+8                                                              
         MVI   4(R6),C' '                                                       
         SPACE                                                                  
*NOP     BNZ   *+10                                                             
*        MVC   5(3,R6),SPACES                                                   
         B     XIT                                                              
         DROP  R1                                                               
RECLENER DS   0H                                                                
         LA    RE,L'RECLENMS-1                                                  
         LARL  RF,RECLENMS                                                      
         B     ERRXIT2                                                          
         SPACE                                                                  
STAERRA  DS   0H                                                                
         LA    RE,L'STAERRMS-1                                                  
         LARL  RF,STAERRMS                                                      
         B     ERRXIT2                                                          
         EJECT                                                                  
* SYSTEM ROUTINES ENTERABLE FROM BASE OR OVERLAY *                              
         SPACE                                                                  
         DS    0H                                                               
VCOMMON  NTR1  BASE=SYSRB                                                       
         LA    R7,4095(,RB)                                                     
         LA    R7,1(,R7)                                                        
         ST    RD,COMMRD                                                        
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         SRL   RF,24                                                            
         B     VBRANCH(RF)                                                      
         SPACE                                                                  
VBRANCH  B     VUSER                                                            
         B     VMED                                                             
         B     VCLI                                                             
         B     VPRD                                                             
         B     VMKT                                                             
         B     VSTAT                                                            
         B     VSLN                                                             
         B     VLOC                                                             
         B     VEST                                                             
         B     VERR                                                             
         B     VSWIT                                                            
         B     VPER                                                             
         B     VDPT                                                             
         B     VSOX                                                             
         B     VPGR                                                             
         SPACE                                                                  
VCOUNT   EQU   (*-VBRANCH)/4                                                    
         LTORG                                                                  
         EJECT                                                                  
         USING T216FFD,RA                                                       
VUSER    XC    GBLOCK,GBLOCK       CLEAR ERROR AREA                             
         BRAS  RE,CHKSCRN2         CHECK IF NEED TO DO ANYTHING SPECIAL         
         BE    XIT                 IF TALCOM LIST AND NEWSCREEN, EXIT           
*                                                                               
         BRAS  RE,CHKCGPG          CHECK IF NEED TO DO ANYTHING SPECIAL         
         BE    XIT                 IF CG/PG LIST AND NEWSCREEN, EXIT            
*                                                                               
         L     R1,SYSPARMS                                                      
         L     R1,16(,R1)          COMFACS                                      
         USING COMFACSD,R1                                                      
         L     RF,CGETFACT                                                      
         DROP  R1                                                               
         GOTO1 (RF),DMCB,0                                                      
         L     RE,DMCB             FAFACTS                                      
         USING FACTSD,RE                                                        
         L     R1,FASYSLST                                                      
         CLI   FAOVSYS,02          SPOT - NOT ANY MORE                          
         BE    SIGNONE1                                                         
         CLI   FAOVSYS,13          STRAFFIC                                     
         BNE   VUSER10                                                          
         SPACE                                                                  
         LARL  R1,RECACTS          SPOT RECORD/ACTION DIRECTORY                 
         MVI   SPOTNETF,C'S'       SET FLAG TO SPOT                             
                                                                                
*GRID                                                                           
         CLC   =C'SPO',CONREC                                                   
         BNE   VUSER05                                                          
         CLC   =C'LIS',CONACT                                                   
         BNE   VUSER05                                                          
         LHI   R5,PCDRIVE-T216FFD                                               
         A     R5,ATWA                                                          
         USING PCDRIVEN,R5                                                      
         TM    PCGRIDS,PCGRPCQ                                                  
         BZ    VUSER05                                                          
         DROP  R5                                                               
         LA    R1,RECACTG           NET RECORD/ACTION DIRECTORY                 
*GRID                                                                           
* CHECK SECURITY FOR TBUY HERE *                                                
         SPACE                                                                  
VUSER05  CLI   OFFLINE,C'Y'        ONLY CHECK ONLINE                            
         BE    VUSER14                                                          
         SPACE                                                                  
         CLC   CONREC(3),=C'TBU'                                                
         BNE   VUSER14                                                          
         TM    TWAAUTH,X'80'       TBUY AUTHORIZED                              
*NOP     BO    SECERR               NO, TELL 'EM GOODBYE                        
         SPACE                                                                  
         B     VUSER14                                                          
         SPACE                                                                  
VUSER10  CLI   FAOVSYS,03          NET                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  RE                                                               
         MVC   RCPROG(2),=C'NE'    PREFIX FOR REPORT NO.                        
         LA    R1,RECACTN          NET RECORD/ACTION DIRECTORY                  
         MVI   SPOTNETF,C'N'       SET FLAG TO NETWORK                          
*                                                                               
         CLI   MODE,NEWSCR                                                      
         BNE   VUSER12                                                          
         CLC   =C'COMT',CONREC     COMTEXT FOR NET                              
         BNE   XIT                                                              
*                                                                               
         MVC   CX$TRAHMS,=C'NETWORK'                                            
         XC    CX$TRAHMS+1(6),SPACES  LOWERCASE                                 
         OI    CX$TRAHMSH+6,X'80' TRANSMIT                                      
*                                                                               
         CLI   TWASCR,X'D1'        COMTEXT LIST SCREEN LOADED                   
         BNE   XIT                                                              
         MVC   CL$TRAMMS+10(7),=C'NETWORK'                                      
         XC    CL$TRAMMS+11(6),SPACES  LOWERCASE                                
         XC    CL$TRAMMS+17(2),CL$TRAMMS+17                                     
         OI    CL$TRAMMSH+6,X'80' TRANSMIT                                      
         B     XIT                                                              
*                                                                               
VUSER12  DS    0H                                                               
*GRID                                                                           
         CLC   =C'NET',CONREC                                                   
         BNE   VUSER14                                                          
         CLC   =C'LIS',CONACT                                                   
         BNE   VUSER14                                                          
         LHI   R5,PCDRIVE-T216FFD                                               
         A     R5,ATWA                                                          
         USING PCDRIVEN,R5                                                      
         TM    PCGRIDS,PCGRPCQ                                                  
         BZ    VUSER14                                                          
         DROP  R5                                                               
         LA    R1,RECACTG           NET RECORD/ACTION DIRECTORY                 
*GRID                                                                           
         SPACE                                                                  
* READ AGENCY HEADER FOR USER NAME AND ADDRESS *                                
         SPACE                                                                  
VUSER14  CLI   TWAFIRST,0           TEST FIRST TIME                             
         BE    VUSER16              YES - READ DATA                             
         CLI   OFFLINE,C'Y'         TEST OFF-LINE                               
         BE    VUSER16              YES - ALWAYS READ                           
                                                                                
*        WE ARE LOSING SVUSER IN THE LATEST VERSION OF THE                      
*        CONTROLLER SO AM GOING TO ALWAYS READ IT                               
*        MVC   USERNAME(66),SVUSER  ELSE MOVE SAVED NAME                        
*        B     VUSER60                                                          
                                                                                
*        INITIALIZE SECURITY BLOCK                                              
                                                                                
VUSER16  DS    0H                                                               
         MVC   FILENAME,=CL8'CTFILE'                                            
         MVI   USEIO,C'Y'                                                       
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING CTIKEY,R4                                                        
         MVI   CTIKTYP,C'I'                                                     
         MVC   CTIKID+8(2),TWAORIG                                              
         GOTO1 READ                                                             
         L     R4,AIO                                                           
         LA    R6,CTIDATA                                                       
         MVI   ELCODE,X'36'                                                     
         BAS   RE,FIRSTEL                                                       
         BNE   VUSER30                                                          
         USING CTORGD,R6                                                        
         MVC   USERNAME,CTORGNAM                                                
         MVC   USERADDR,CTORGADD                                                
         MVC   SVUSER(66),USERNAME  SAVE FOR FUTURE REF                         
         SPACE                                                                  
VUSER30  XC    FILENAME,FILENAME                                                
         MVI   USEIO,0                                                          
         SPACE                                                                  
* TEMP CODE FOR CANADIAN TESTING * (SEEMS SORT OF PERMANENT!)                   
         SPACE                                                                  
         L     R4,AIO                                                           
         LA    R6,CTIDATA                                                       
         MVI   ELCODE,X'02'                                                     
         BAS   RE,FIRSTEL                                                       
         BNE   VUSER60                                                          
         USING CTDSCD,R6                                                        
         MVC   AGYORIG,CTDSC                                                    
         SPACE                                                                  
* IF OFFLINE, AND NOT NET, SW TO SPT TO OPEN SPOT FILES *                       
         SPACE                                                                  
         CLI   OFFLINE,C'Y'                                                     
         BNE   VUSER40                                                          
         CLI   SPOTNETF,C'N'                                                    
         BE    VUSER40                                                          
         SPACE                                                                  
         GOTO1 VSWITCH,SPTDIR                                                   
         SPACE                                                                  
         GOTO1 VSWITCH,=C'STR'                                                  
         SPACE                                                                  
VUSER40  CLC   =C'SJHD',CTDSC                                                   
         BE    VUSER50                                                          
         DROP  R4,R6                                                            
         SPACE                                                                  
* SEE IF CANADIAN AGENCY *                                                      
         SPACE                                                                  
         BRAS  RE,INITSPT         CHANGE TO SPOT FILE                           
         SPACE                                                                  
         XC    KEY,KEY             GET AGENCY RECORD                            
         LA    R4,KEY                                                           
         USING AGYKEY,R4                                                        
         MVI   AGYKTYPE,X'06'                                                   
         MVC   AGYKAGY,AGENCY                                                   
         DROP  R4                                                               
         SPACE                                                                  
         MVI   RDUPDATE,C'N'       NOT READ FOR UPDATE                          
         GOTO1 HIGH                                                             
         SPACE                                                                  
         CLC   KEY(13),KEYSAVE     FIND THE AGENCY RECORD                       
         BNE   AGYERR               WHAT, NO AGENCY HEADER?                     
         SPACE                                                                  
         L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         SPACE                                                                  
         MVI   RDUPDATE,C'N'       NOT READ FOR UPDATE                          
         GOTO1 GETREC                                                           
*                                                                               
         MVI   ELCODE,X'01'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING AGYEL,R6                                                         
         SPACE                                                                  
         CLI   AGYPROF+7,C'C'                                                   
         BNE   VUSER60                                                          
VUSER50  MVI   SPOTCAN,C'C'                                                     
         DROP  R6                                                               
         SPACE                                                                  
* READ CLIENT HEADER & SAVE CLIST *                                             
         SPACE                                                                  
VUSER60  DS    0H                                                               
         OC    SVBCLTH,SVBCLTH     IS THERE A SAVED CLT                         
         BZ    VUSER66              NO                                          
         SPACE                                                                  
         CLI   OFFLINE,C'Y'        CHECK ONLINE                                 
         BE    VUSER62                                                          
                                                                                
* RESTORE TWA2 WITH PRODUCT LIST                                                
                                                                                
         LA    R1,=C'DMREAD'                                                    
         XC    DMCB(24),DMCB                                                    
         ST    R1,DMCB                                                          
         L     RE,ATWA                                                          
         MVC   DMCB+10(2),2(RE)    MOVE TERMINAL NUMBER                         
         MVI   DMCB+8,3            SET PAGE 3                                   
         ICM   RF,12,=C'L='                                                     
         ICM   RF,3,=Y(LENSVCL)                                                 
         GOTO1 DATAMGR,DMCB,,=C'TEMPSTR',,ASVNCLST,,(RF)                        
         CLI   8(R1),0                                                          
         BE    VUSER66                                                          
         DC    H'0'                                                             
*                                                                               
VUSER62  BRAS  RE,RDCLST                                                        
         EJECT                                                                  
         SPACE                                                                  
* FIND DCB ADDRESS AND SEE IF READ ONLY                                         
         SPACE                                                                  
VUSER66  DS   0H                                                                
         CLI   TWAOFFC,C'*'        THIS A DDS TERMIANL                          
*        BE    VUSER68              YES, THEY MAY BE OKAY                       
         SPACE                                                                  
         CLI   OFFLINE,C'Y'        IF OFFLINE DO NOT SET UP SOX                 
         BE    VUSER68                                                          
         SPACE                                                                  
         L     RE,ACOMFACS                                                      
         L     RF,CXTRAINF-COMFACSD(,RE)                                        
         MVC   SOXSW,XIFLAG1-XTRAINFO(RF)                                       
         SPACE                                                                  
*        GOTO1 DATAMGR,DMCB,=C'DTFAD',SYSDIR                                    
         SPACE                                                                  
*        L     RE,12(,R1)          GET ADDR OF DCB                              
         SPACE                                                                  
*        TM    ISFOPEN-ISDTF(RE),ISFORO+ISFONOP  READ ONLY OR NOP               
*        BNZ   WRITERR                                                          
         SPACE                                                                  
* FOR NETPAK READ PU PROFILE TO SEE IF MORE PRODUCTS SET ON *                   
         SPACE                                                                  
VUSER68  DS    0H                                                               
         CLI   SPOTNETF,C'N'       IS THIS NETWORK                              
         BNE   VUSER69                                                          
                                                                                
         BRAS  RE,RDPUPROF                                                      
                                                                                
* SET SYSFIL/DIR TO UNIT FILE FOR REVISIONS, SKED, SPOT ALL OTHER *             
VUSER69  DS   0H                                                                
         TM    GENSTAT1,USKYMRG+EFHKYMRG                                        
         BZ    *+8                  NO                                          
         NI    GENSTAT1,X'FF'-(USKYMRG+EFHKYMRG)                                
         SPACE                                                                  
         BRAS  RE,INITSPT         CHANGE TO SPOT FILE                           
         SPACE                                                                  
         MVI   LKEY+1,13          DETAILS OF DIRECTORY AND KEY                  
         MVI   DATADISP+1,24                                                    
         SPACE                                                                  
         CLI   CONRECH+5,0                                                      
         BE    XIT                                                              
         SPACE                                                                  
         ZIC   RF,CONRECH+5                                                     
         BCTR  RF,0                                                             
         SPACE                                                                  
         CLI   SPOTNETF,C'S'       IF SPOT, CK FOR PRODUCT (SPOT FILES)         
         BE    VUSER80                                                          
         CLI   SPOTNETF,C'N'       IF NETWORK, CK FOR UNIT FILES                
         BNE   XIT                                                              
         EX    RF,VUSERCLA         THIS REVISION                                
         BE    VUSER70A                                                         
         EX    RF,VUSERCLB         THIS SKED                                    
         BE    VUSER70                                                          
         EX    RF,VUSERCLC         THIS PATTERN                                 
         BE    VUSER70A                                                         
         EX    RF,VUSERCLD         THIS EQUIV PROG                              
         BE    VUSER70                                                          
         EX    RF,VUSERCLE         THIS DAYPART PROG                            
         BE    VUSER70                                                          
         EX    RF,VUSERCLU         THIS VENDOR PROG                             
         BE    VUSER70                                                          
         EX    RF,VUSERCLT         THIS STATION RECORD                          
         BE    VUSER70                                                          
         EX    RF,VUSERCLF         THIS TSUPP PROG                              
         BNE   VUSER71                                                          
         SPACE                                                                  
VUSER70  DS   0H                                                                
         BRAS  RE,INITNET         CHANGE TO UNT FILE                            
         SPACE                                                                  
         B     XIT                                                              
         SPACE                                                                  
VUSER70A DS   0H                                                                
*                                 NET FULLY CONVERTED - OR USING BOTH           
         TM    SECFLAG,NECONPRD+NEMORPRD                                        
         BZ    VUSER70                                                          
         SPACE                                                                  
         BRAS  RE,INITXSP         CHANGE TO XSP FILE                            
         B     XIT                                                              
         SPACE                                                                  
VUSER71  LARL  RE,RECACTS         THIS COMML RECORD                             
         AHI   RE,SPTCOMML-RECACTS                                              
         EX    RF,VUSERCLM         THIS COMML RECORD                            
         BE    VUSER94              YES, GO FOR MORE CHECKING                   
         SPACE                                                                  
VUSER73  EX    RF,VUSERCLG         NINS GEN                                     
         BE    VUSER78                                                          
         EX    RF,VUSERCLN         NET LIST/NET ASSIGN/NET SEED                 
         BE    VUSER74                                                          
         CHI   RF,1                                                             
         BNH   XIT                                                              
         LA    RF,1                MAYBE 1ST 2 CHARS MATCH                      
         B     VUSER73                                                          
         SPACE                                                                  
VUSER74  ZIC   RF,CONACTH+5                                                     
         BCTR  RF,0                                                             
VUSER75  DS   0H                                                                
                                                                                
         LARL  RE,RECACTS          POINT TO ACTION TABLE                        
         AHI   RE,ACTASSGN-RECACTS POINT TO 'ASSIGN'                            
         EX    RF,VUSERCLH         IS IT NET/ASSIGN?                            
         BE    VUSER76                                                          
         CHI   RF,1                                                             
         BNH   VUSER78                                                          
         LA    RF,1                MAYBE 1ST 2 CHARS MATCH                      
         B     VUSER75                                                          
         SPACE                                                                  
VUSER76  OC    CONWHEN,CONWHEN     PRINT OPTION                                 
         BZ    VUSER78                                                          
         XC    CONWHEN,CONWHEN     NO, CLEAR PRINT FIELD                        
         OI    CONWHENH+6,X'80'    TRANSMIT                                     
         L     RF,EFHWHEN          USE FOR EXTENDED HEADERS                     
         MVI   5(RF),0                                                          
         SPACE                                                                  
VUSER78  OI    GENSTAT1,USKYMRG    TURN ON MERGE KEYS ON LIST                   
         OI    GENSTAT1,EFHKYMRG   & ON EXTENDED FIELD HEADER                   
         B     XIT                                                              
         SPACE                                                                  
VUSER80  DS   0H                                                                
         BRAS  RE,INITTRF          POINT TO TRAFFIC FILE                        
         LARL  RE,RECACTS                                                       
         AHI   RE,SPTPROD-RECACTS  POINT RE TO 'SPTPROD'                        
         EX    RF,VUSERCLP         THIS PRODUCT REC                             
         BNE   VUSER84                                                          
         BRAS  RE,INITSPT          POINT TO SPOT FILE                           
         B     XIT                                                              
         SPACE                                                                  
VUSER84  LARL  RE,RECACTS                                                       
         AHI   RE,GNSTUDIO-RECACTS POINT TO GNSTUDIO                            
         EX    RF,VUSERCLS         THIS STUDIO REC                              
         BNE   VUSER90                                                          
         SPACE                                                                  
         CLI   CONRECH+5,3                                                      
         BL    RECLENER                                                         
         SPACE                                                                  
         L     RF,ACOMFACS          USE FASWITCH TO SWITCH                      
         L     R6,CSWITCH-COMFACSD(RF)                                          
         LTR   R6,R6                OFFLINE ?                                   
         BNZ   *+6                  NO                                          
         DC    H'0'                                                             
         SPACE                                                                  
         GOTO1 (R6),DMCB,=C'CON',0                                              
         CLI   4(R1),0             ALL SWITCHES COME HERE                       
         BE    *+6                                                              
         DC    H'0'                                                             
         SPACE                                                                  
         BRAS  RE,INITGEN          POINT TO CONTROL SYSTEM GENDIR/FIL           
         B     XIT                                                              
         SPACE                                                                  
VUSER90  EX    RF,VUSERCLM         THIS COMML RECORD                            
         BNE   XIT                  NO, DONE                                    
         SPACE                                                                  
* BE CAREFUL! THIS CODE IS FOR BOTH STR/NET                                     
         SPACE                                                                  
VUSER94  DS    0H                                                               
         B     XIT                                                              
         SPACE                                                                  
VUSERCLA CLC   CONREC(0),NETREV                                                 
VUSERCLB CLC   CONREC(0),NETSKED                                                
VUSERCLC CLC   CONREC(0),NETPAT                                                 
VUSERCLD CLC   CONREC(0),NETEPRG   PROG EQUIV                                   
VUSERCLE CLC   CONREC(0),NETDYPT   DAYPART FAX (NETDYPT)                        
VUSERCLF CLC   CONREC(0),NETTSUP   TRAFFIC SUPPLIER (NETTSUP)                   
VUSERCLG CLC   CONREC(0),NINSGEN   NINS GEN                                     
VUSERCLH CLC   CONACT(0),0(RE)     ACTION ASSIGN                                
VUSERCLM CLC   CONREC(0),0(RE)     COMMERCIAL RECORD                            
VUSERCLN CLC   CONREC(0),NETLIST   NET LIST/ NET ASSIGN                         
VUSERCLP CLC   CONREC(0),0(RE)     SPOT PRODUCT                                 
VUSERCLS CLC   CONREC(0),0(RE)     STUDIO LIST                                  
VUSERCLT CLC   CONREC(0),STATION   STATION RECORD (STATION)                     
VUSERCLU CLC   CONREC(0),NETVEND   DAYPART FAX (NETDYPT)                        
         DROP  RA                                                               
         EJECT                                                                  
* VALIDATE MEDIA CODE *                                                         
         SPACE                                                                  
VMED     DS   0H                   CLEAR SAVED KEY AREA                         
         CLI   RECNUM,X'15'                                                     
         BNE   *+8                                                              
         BRAS  RE,SVFILE                                                        
                                                                                
         BRAS  RE,VMEDIA                                                        
                                                                                
         CLI   RECNUM,X'15'                                                     
         BNE   *+8                                                              
         BRAS  RE,REFILE                                                        
         B     XIT                                                              
         EJECT                                                                  
                                                                                
* VALIDATE CLIENT - ON EXIT QCLT AND BCLT CONTAIN VALUES *                      
         SPACE                                                                  
VCLI     DS   0H                                                                
                                                                                
         CLI   RECNUM,X'15'                                                     
         BNE   *+8                                                              
         BRAS  RE,SVFILE                                                        
                                                                                
         XC    SVBCLTH,SVBCLTH     CLEAR IN CASE OF ERROR                       
         GOTO1 ANY                 CLIENT                                       
*                                                                               
         NI    SECFLAG,X'FF'-BLSSW INIT BRAND LEVEL SECURITY                    
         NI    SECFLAG,X'FF'-NOCLOFF  AND NO MATCH ON CLIENT OFFICE             
*                                                                               
         SPACE                                                                  
         MVI   ERROR,INVCLI                                                     
         MVC   QCLT(3),WORK                                                     
         CLI   5(R2),3                                                          
         BH    VCLIERR                                                          
         CLI   5(R2),2                                                          
         BL    VCLIERR                                                          
*                                                                               
         GOTO1 CLPACK,DMCB,QCLT,BCLT                                            
         CLI   0(R1),0                                                          
         BNE   VCLIERR                                                          
         SPACE                                                                  
* READ CLIENT HEADER *                                                          
         SPACE                                                                  
         BRAS  RE,INITSPT          POINT TO SPOT FILE                           
         SPACE                                                                  
         XC    KEY,KEY                                                          
         MVC   KEY+1(1),BAGYMD                                                  
         MVC   KEY+2(2),BCLT                                                    
*                                                                               
         MVI   RDUPDATE,C'N'       NOT READ FOR UPDATE                          
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   VCLIERR                                                          
         SPACE                                                                  
         L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         SPACE                                                                  
         USING CLTHDRD,R6                                                       
         MVI   RDUPDATE,C'N'       NOT READ FOR UPDATE                          
         GOTO1 GETREC                                                           
         SPACE                                                                  
                                                                                
         CLI   SPOTNETF,C'S'       IF SPOT STILL NEED TO FILL IN TABLE          
         BE    VCLI10                                                           
         BRAS  RE,RDPUPROF                                                      
                                                                                
*        SAVE CLIENT PRODUCT LIST *                                             
VCLI10   DS   0H                   SVCLIST/SVNCLIST NOW THE SAME                
         L     R0,ASVNCLST                                                      
         LA    RE,CLIST                                                         
         LA    RF,880                                                           
         LR    R1,RF                                                            
         MVCL  R0,RE                                                            
         SPACE                                                                  
         LR    R2,R0               SAVE POINTER FOR POSSIBLE 2ND MOVE           
         SPACE                                                                  
         LA    R1,140              FILL REMAINDER WITH NULLS                    
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         SPACE                                                                  
         CLC   CLEN,=H'1500'       IS THIS A NEW CLIST CLT                      
         BL    VCLI12               NO                                          
         SPACE                                                                  
         LR    R0,R2               RESTORE                                      
         LA    RE,CLIST2                                                        
         LA    RF,140                                                           
         LR    R1,RF                                                            
         MVCL  R0,RE                                                            
         SPACE                                                                  
VCLI12   DS    0H                                                               
         MVC   CLTNM,CNAME         AND CLIENT NAME                              
         CLI   RECNUM,X'36'                                                     
         BE    VCLI12C                                                          
         SPACE                                                                  
VCLI12A  DS    0H                                                               
         CLI   SPOTNETF,C'N'       IS THIS NETWORK                              
         BE    *+8                                                              
         BRAS  RE,INITTRF         CHANGE TO TRAFFIC FILE                        
         SPACE                                                                  
VCLI12C  CLI   OFFLINE,C'Y' BYPASS IF OFFLINE                                   
         BE    VCLI13                                                           
         SPACE                                                                  
         XC    DMCB(24),DMCB                                                    
         LA    R1,=C'DMWRT '                                                    
         ST    R1,DMCB                                                          
*        MVI   DMCB+8,0            SET PAGE 0  (TWA0)                           
         L     RE,ATWA                                                          
         MVC   DMCB+10(2),2(RE)    MOVE TERMINAL NUMBER                         
         MVC   DMCB+20(2),=C'L='                                                
         MVC   DMCB+22(2),=Y(TWA018K)                                           
         GOTO1 DATAMGR,DMCB,,=C'TEMPSTR',,ATWA                                  
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
VCLI13   DS    0H                                                               
         TM    COPT3,COP3PSEC      BRAND LEVEL SECURITY                         
         BZ    *+8                                                              
         OI    SECFLAG,BLSSW       TURN ON BRAND LEVEL SECURITY                 
         SPACE                                                                  
         MVC   SVCLTOFF,CTRAFOFC   USE TRAFFIC OFFICE                           
         CLI   SVCLTOFF,C' '       IF THERE IS ONE                              
         BNL   VCLI14                                                           
         SPACE                                                                  
         MVC   SVCLTOFF,COFFICE    USE MEDIA OFFICE                             
         SPACE                                                                  
VCLI14   DS   0H                                                                
         GOTO1 CLUNPK,DMCB,(CPROF+6,BCLT),QCLT                                  
         SPACE                                                                  
         MVC   SVCPROF6,CPROF+6    SAVE FOR AAN PRINT                           
*                                                                               
         CLI   OFFLINE,C'Y'        IF OFFLINE                                   
         BNE   *+14                                                             
         CLC   =C'OMDDTQ',AGYORIG  AND ITS THIS AGENCY                          
         BE    VCLIEX              THEN BYPASS LIMIT ACCESS                     
*                                                                               
         L     RA,ATWA                                                          
         USING T216FFD,RA                                                       
         MVI   ERROR,SECLOCK                                                    
         OC    T216FFD+4(2),T216FFD+4  NEW SECURITY                             
         BNZ   VCLI30                                                           
*                                                                               
         OC    T216FFD+6(2),T216FFD+6  TEST ANY SECURITY LIMIT                  
         BNZ   *+12                                                             
         NI    SECFLAG,X'FF'-BLSSW RESET BRAND LEVEL SECURITY                   
         B     VCLIEX                                                           
*                                                                               
         CLI   T216FFD+6,C'*'          TEST OFFICE LOCKOUT                      
         BE    VCLI30                    (OLD BRANCH TO VCLI20)                 
         CLI   T216FFD+6,C'$'          TEST OFFICE LIST LOCKOUT                 
         BE    VCLI30                                                           
*                                                                               
         NI    SECFLAG,X'FF'-BLSSW     RESET BRAND LEVEL SECURITY               
*                                                                               
         CLC   T216FFD+6(2),BCLT       ELSE SINGLE CLIENT ACCESS                
         BE    VCLIEX                                                           
         BNE   VCLI30                                                           
*                                                                               
VCLI20   CLI   T216FFD+7,C'A'         CHECK IF ALPHA                            
         BL    VCLI25                                                           
         CLI   T216FFD+7,C'Z'                                                   
         BH    VCLI25                                                           
         CLI   T216FFD+8,C'0'         CHECK IF NUMERIC                          
         BL    VCLI25                                                           
         CLI   T216FFD+8,C'9'                                                   
         BH    VCLI25                                                           
         SPACE                                                                  
         MVI   ERROR,0                                                          
         SPACE                                                                  
         BAS   RE,VALGROUP            VALIDATE GROUP NUMBER                     
         SPACE                                                                  
         CLI   ERROR,0                                                          
         BNE   VCLIERR                                                          
         B     VCLIEX                                                           
*                                                                               
VCLI25   DS   0H                                                                
         LA    R1,CACCESS                                                       
         LA    R0,3                                                             
         CLI   0(R1),C' '                                                       
         BH    *+12                                                             
         LA    R1,SVCLTOFF                                                      
         LA    R0,1                                                             
*                                                                               
VCLI27   CLC   T216FFD+7(1),0(R1)   TEST RIGHT OFFICE                           
         BE    VCLIEX                                                           
         LA    R1,1(R1)                                                         
         BCT   R0,VCLI27                                                        
         B     VCLIERR                                                          
*                                                                               
VCLI30   DS    0H                                                               
         CLI   OFFLINE,C'Y'        OFFLINE                                      
         BNE   VCLI35                                                           
         CLI   T216FFD+6,C'$'      TEST OFFICE LIST LOCKOUT                     
         BE    VCLI35                                                           
         CLI   T216FFD+6,C'*'      TEST OFFICE LOCKOUT                          
         BE    VCLI35                                                           
         MVI   ERROR,0             BYPASS SECURITY                              
         B     VCLIEX              IT IS NOT WORKING YET                        
*                                                                               
VCLI35   DS    0H                                                               
         XC    DMCB(8),DMCB                                                     
         MVC   DMCB+4(4),=X'D9000A38'  GET OFFICER ADDRESS                      
         GOTO1 CALLOV,DMCB                                                      
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         XC    WORKSEC,WORKSEC                                                  
         LA    R1,WORKSEC                                                       
         USING OFFICED,R1                                                       
         MVI   OFCSYS,C'S'         SYSTEM ID                                    
         MVC   OFCAUTH,T216FFD+6   ID AUTH VALUE                                
         MVC   OFCAGY,AGENCY                                                    
         MVC   OFCOFC,SVCLTOFF                                                  
         MVC   OFCCLT,QCLT                                                      
         OC    OFCCLT,SPACES                                                    
         MVC   OFCSAGMD,BAGYMD                                                  
         MVC   OFCLMT(4),T216FFD+6                                              
         MVC   OFCACCSC(3),CACCESS    ACCESS LIST FROM CLTHDR                   
         MVC   OFCSECD,ASECBLK                                                  
         DROP  R1                                                               
*                                                                               
         L     RF,DMCB                                                          
         GOTO1 (RF),DMCB,(C'N',WORKSEC),ACOMFACS                                
         CLI   0(R1),0                                                          
         BE    VCLIEX                                                           
         SPACE                                                                  
VCLIERR  DS    0H                                                               
         TM    SECFLAG,BLSSW       BRAND LEVEL SECURITY                         
         BZ    VCLIER20                                                         
         SPACE                                                                  
         OI    SECFLAG,NOCLOFF     NO MATCH ON CLIENT OFFICE                    
         SPACE                                                                  
         CLI   T216FFD+6,C'*'      TEST OFFICE LOCKOUT                          
         BE    VCLIER10                                                         
         CLI   T216FFD+6,C'$'      TEST OFFICE LIST LOCKOUT                     
         BNE   VCLIER20                                                         
         SPACE                                                                  
VCLIER10 BRAS  RE,BCPRLST          RE-BUILD CLIENT PRODUCT LIST                 
         CLI   ERROR,0                                                          
         BE    VCLIEX10                                                         
         SPACE                                                                  
VCLIER20 XC    FILENAME,FILENAME                                                
         CLI   ERROPT,C'Y'         RETURN ON ERROR?                             
         BE    XIT                                                              
         B     TRAPERR                                                          
         SPACE                                                                  
VCLIEX   DS    0H                                                               
         TM    SECFLAG,BLSSW       BRAND LEVEL SECURITY                         
         BZ    VCLIEX05                                                         
         BRAS  RE,BCPRLST          RE-BUILD CLIENT PRODUCT LIST                 
         CLI   ERROR,0                                                          
         BNE   VCLIER20                                                         
*                                                                               
VCLIEX05 MVI   ERROR,0             RESET ERROR                                  
*                                                                               
VCLIEX10 MVC   SVBCLTH,BCLT        SAVE FOR NEW SAVE CLIST                      
         XC    FILENAME,FILENAME                                                
                                                                                
         CLI   RECNUM,X'15'                                                     
         BNE   *+8                                                              
         BRAS  RE,REFILE                                                        
                                                                                
         CLI   OFFLINE,C'Y'        CHECK ONLINE                                 
         BE    VCLIXX                                                           
         LA    R1,=C'DMWRT'                                                     
         XC    DMCB(24),DMCB                                                    
         ST    R1,DMCB                                                          
         L     RE,ATWA                                                          
         MVC   DMCB+10(2),2(RE)    MOVE TERMINAL NUMBER                         
         MVI   DMCB+8,3            SET PAGE 3                                   
         SR    RF,RF                                                            
         ICM   RF,12,=C'L='                                                     
         ICM   RF,3,=Y(LENSVCL)                                                 
         GOTO1 DATAMGR,DMCB,,=C'TEMPSTR',,ASVNCLST,,(RF)                        
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
VCLIXX   DS    0H                                                               
                                                                                
         B     XIT                                                              
         DROP  R6,RA                                                            
                                                                                
RDPUPROF NTR1                                                                   
         NI    SECFLAG,X'FF'-NEMORPRD-NECONPRD                                  
         OI    SECFLAG,NECONPRD                                                 
         CLC   AGENCY(2),=C'SJ'                                                 
         BNE   RDPUXIT                                                          
                                                                                
         NI    SECFLAG,X'FF'-NECONPRD                                           
         XC    WORK,WORK                                                        
         MVC   WORK(4),=C'S0PU'                                                 
         MVC   WORK+4(2),AGENCY                                                 
         MVC   WORK+6(L'QMED+L'QCLT),QMED       MEDIA/CLIENT                    
         GOTO1 GETPROF,DMCB,(X'90',WORK),WORK+16,DATAMGR                        
                                                                                
         CLI   WORK+16+4,C'Y'      RUNNING IN PARALLEL                          
         BNE   RDPU025                                                          
         OI    SECFLAG,NEMORPRD                                                 
         B     RDPUXIT                                                          
                                                                                
RDPU025  CLI   WORK+16+4,C'C'      RUNNING IN PARALLEL                          
         BNE   RDPUXIT                                                          
         OI    SECFLAG,NECONPRD                                                 
RDPUXIT  XIT1                                                                   
                                                                                
                                                                                
SVFILE   NTR1                                                                   
         MVC   SAVFILD,SYSDIR                                                   
         MVC   SAVFILF,SYSFIL                                                   
         MVC   SAVLKEY,LKEY                                                     
         MVC   SAVDDISP,DATADISP                                                
         MVC   SAVLSTAT,LSTATUS                                                 
         B     XIT                                                              
                                                                                
REFILE   NTR1                                                                   
         MVC   SYSDIR,SAVFILD                                                   
         MVC   SYSFIL,SAVFILF                                                   
         MVC   LKEY,SAVLKEY                                                     
         MVC   DATADISP,SAVDDISP                                                
         MVC   LSTATUS,SAVLSTAT                                                 
         B     XIT                                                              
         EJECT                                                                  
VALGROUP NTR1                                                                   
         L     RA,ATWA                                                          
         USING T216FFD,RA                                                       
*                                                                               
         MVC   ELEM(24),KEY                                                     
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING GRPRECD,R4          CLIENT STATION PASSIVE POINTER               
*                                                                               
         MVI   GRPPTYP,GRPPTYPQ    RECORD TYPE                                  
         MVI   GRPPSTYP,GRPPCTYQ   CLIENT GROUP                                 
         MVC   GRPPAGMD(1),BAGYMD     AGENCY/MEDIA                              
         MVC   GRPPVAL(3),QCLT     CLIENT                                       
         OC    GRPPVAL,SPACES      BLANK PADDED                                 
         MVC   GRPPID(1),T216FFD+7   GROUP ID                                   
*                                                                               
         MVC   FILENAME,SPTDIR                                                  
         MVI   RDUPDATE,C'N'       NOT READ FOR UPDATE                          
         GOTO1 HIGH                                                             
         CLC   KEY(10),KEYSAVE                                                  
         BNE   VALGR10                                                          
*                                                                               
         MVC   FULL,SPACES                                                      
         MVC   FULL(2),T216FFD+8   GROUP CODE                                   
         OC    FULL,=C'0000'       REPLACE BLANKS WITH X'F0'                    
         PACK  DUB,FULL                                                         
         L     R0,DUB+4                                                         
         SRL   R0,4                GET RID OF SIGN NIBLE                        
         STCM  R0,3,HALF           LEFT-JUSTIFIED, PWOS                         
*                                                                               
         CLC   HALF,GRPPCODE       GROUP CODE MATCH?                            
         BE    VALGR20             YES                                          
*                                                                               
VALGR10  DS   0H                                                                
         CLI   ERROPT,C'Y'         RETURN ON ERROR?                             
         BNE   VALGR14              NO                                          
         MVI   ERROR,SECLOCK                                                    
         B     XIT                                                              
VALGR14  DS   0H                                                                
         LA    RE,L'NOCLACC-1                                                   
         LA    RF,NOCLACC                                                       
         B     ERRXIT2                                                          
*                                                                               
VALGR20  XC    KEY,KEY                                                          
         MVC   KEY(13),ELEM                                                     
         MVI   RDUPDATE,C'N'       NOT READ FOR UPDATE                          
         GOTO1 HIGH                                                             
         XC    FILENAME,FILENAME                                                
         B     XIT                                                              
         DROP  R4,RA                                                            
         EJECT                                                                  
**********************************************************************          
* VALIDATE DAYPART                                                              
**********************************************************************          
         SPACE                                                                  
VDPT     BRAS  RE,VALDPT                                                        
         B     XIT                                                              
         SPACE 3                                                                
**********************************************************************          
* GIVE SOX ERROR MESSAGE                                                        
**********************************************************************          
         SPACE                                                                  
VSOX     BRAS  RE,SOXERR                                                        
         B     XIT                                                              
         EJECT                                                                  
**********************************************************************          
* VALIDATE PRODUCT AND SLN                                                      
**********************************************************************          
         SPACE                                                                  
VPRD     BRAS  RE,VALPRD                                                        
         B     XIT                                                              
*                                                                               
VPGR     BRAS  RE,VALPGR                                                        
         B     XIT                                                              
         EJECT                                                                  
* VALIDATE STATION CALL LETTERS - ON EXIT QSTA QMKT BMKTSTA                     
*                                         AND STAPRINT ARE SET                  
         SPACE                                                                  
VSTAT    XC    BLOCK(80),BLOCK                                                  
         LA    R4,BLOCK                                                         
         USING STABLKD,R4                                                       
         ZIC   R1,5(R2)                                                         
         LA    R1,7(,R1)           ADD FOR HEADER -1                            
         EX    R1,STAMVC                                                        
STAMVC   MVC   BLOCK+64(0),0(R2)                                                
         LA    R1,BLOCK+64                                                      
         ST    R1,STBADDR                                                       
         MVC   STBMED,QMED                                                      
         MVI   STBCTRY,C'U'                                                     
         MVC   STBACOM,ACOMFACS                                                 
         SPACE                                                                  
         GOTO1 STAVAL,DMCB,(R4)                                                 
         MVI   ERROR,INVSTA                                                     
         CLI   STBERR,0                                                         
         BNE   STAERR                                                           
         MVC   QSTA(5),STBSTA      SAVE CALL LETTERS                            
         SPACE                                                                  
         CLI   QSTA+4,C' '         IF THIS IS BLANK, FORCE TO TV                
         BH    *+8                                                              
         MVI   QSTA+4,C'T'                                                      
         SPACE                                                                  
         XC    STANET,STANET                                                    
         CLC   STBNET,SPACES       IS THIS A CABLE HEAD                         
         BE    VSTAT06              NO                                          
         MVC   STANET(4),STBSTA                                                 
         MVI   STANET+4,C'/'                                                    
         MVC   STANET+5(3),STBNET                                               
         DROP  R4                                                               
         SPACE                                                                  
* FORMAT STATION FOR PRINTING (EG WABC-FM) *                                    
         SPACE                                                                  
VSTAT06  MVC   STAPRNT,SPACES                                                   
         MVC   STAPRNT(4),QSTA                                                  
         LA    RE,STAPRNT+3                                                     
         CLI   0(RE),C' '                                                       
         BNE   *+6                                                              
         BCTR  RE,0                                                             
         MVI   1(RE),C'-'                                                       
         MVC   2(1,RE),QSTA+4      MOVE SUB-MEDIA                               
         CLI   QSTA+4,C'L'         LOW POWER TV STA                             
         BE    VSTA12                                                           
         SPACE                                                                  
         CLI   QMED,C'T'                                                        
         BNE   VSTA10                                                           
         MVI   3(RE),C'V'          ASSUME TV                                    
         B     VSTA12                                                           
VSTA10   CLI   QMED,C'R'                                                        
         BNE   VSTA12                                                           
         MVI   3(RE),C'M'          ASSUME RADIO                                 
         SPACE                                                                  
* READ STATION MASTER RECORD *                                                  
         SPACE                                                                  
VSTA12   DS    0H                                                               
         MVI   KEY,C'0'                                                         
         MVC   KEY+1(16),KEY                                                    
         MVI   KEY,C'S'                                                         
         MVC   KEY+1(1),QMED                                                    
         MVC   KEY+2(5),QSTA                                                    
         SPACE                                                                  
         CLI   QSTA+4,C' '                                                      
         BNE   *+10                                                             
         MVC   KEY+6(1),QMED                                                    
         SPACE                                                                  
         MVC   KEY+7(2),AGENCY                                                  
         MVC   KEY+9(3),QCLT                                                    
         MVC   AIO,AIO1                                                         
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'STATION',KEY,AIO                      
         CLI   8(R1),0                                                          
         BNE   STAERR                                                           
*                                                                               
         L     R6,AIO                                                           
         USING STARECD,R6                                                       
         MVC   QMKT,SMKT                                                        
         SPACE                                                                  
         XC    WORK,WORK                                                        
         MVC   WORK(5),QSTA                                                     
         OC    STANET,STANET                                                    
         BZ    VSTA20                                                           
         MVC   WORK(L'STANET),STANET                                            
         MVI   ERROR,0                                                          
         SPACE                                                                  
VSTA20   GOTO1 MSPACK,DMCB,QMKT,WORK,BMKTSTA                                    
         SPACE                                                                  
         CLI   DMCB,X'FF'                                                       
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   DMCB,X'00'                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         SPACE                                                                  
* READ MARKET RECORD TO IO1+400                                                 
         SPACE                                                                  
         LA    R6,400(R6)                                                       
*                                   N O T E  - USE STA LEN SOMEDAY              
*        SR    R0,R0                                                            
*        ICM   R0,3,15,(R6)                                                     
*        AR    R6,R0                                                            
*                                                                               
VSTA30   ST    R6,AIO                                                           
         USING MKTRECD,R6                                                       
         MVI   KEY,C'0'                                                         
         MVC   KEY+1(16),KEY                                                    
         MVI   KEY,C'M'                                                         
         MVC   KEY+1(1),QMED                                                    
         MVC   KEY+2(4),QMKT                                                    
         MVC   KEY+6(2),AGENCY                                                  
*                                                                               
         MVI   ERROR,INVMKT                                                     
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'STATION',KEY,AIO                      
*                                                                               
         MVC   AIO,AIO1            RESTORE AIO                                  
*                                                                               
         CLC   KEY(15),0(R6)                                                    
         BNE   STAERR                                                           
*                                                                               
         L     RA,ATWA                                                          
         USING T216FFD,RA                                                       
*                                                                               
         CLI   T216FFD+6,C'+'      TEST MARKET LOCKOUT                          
         BNE   VSTA40                                                           
         LA    R0,3                                                             
         LA    R1,MKTLTACC                                                      
         CLC   T216FFD+7(1),0(R1)                                               
         BE    VSTA40                                                           
         LA    R1,1(R1)                                                         
         BCT   R0,*-14                                                          
         MVI   ERROR,NOMKTACC                                                   
         B     STAERR                                                           
         DROP  RA                                                               
*                                                                               
VSTA40   MVC   MKTNM,MKTNAME       RETURN MARKET NAME TO USER                   
         MVI   ERROR,0             CLEAR ERROR CODE FOR USER                    
         B     XIT                                                              
         SPACE                                                                  
* ON ERROR TEST IF USER WANTS CONTROL BACK *                                    
         SPACE                                                                  
STAERR   CLI   ERROPT,C'Y'                                                      
         BNE   TRAPERR                                                          
         B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
* VALIDATE MARKET CODE *                                                        
         SPACE                                                                  
VMKT     DS    0H                                                               
         TM    4(R2),X'08'         TEST NUMERIC                                 
         BZ    VMKT2                                                            
         CLI   5(R2),0             BAD MARKET IF NO DIGITS                      
         BE    VMKT2                                                            
         CLI   5(R2),4             BAD MARKET IF MORE THAN 4 DIGITS             
         BH    VMKT2                                                            
         ZIC   RE,5(R2)            GET LENGTH                                   
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R2) *EXECUTED*                                           
*                                                                               
         CVB   R0,DUB                                                           
         STCM  R0,3,BMKT                                                        
*                                                                               
         OI    DUB+7,X'0F'                                                      
         UNPK  QMKT,DUB            SET FOR USER TOO                             
*                                                                               
         L     R6,AIO1                                                          
         B     VSTA30                                                           
VMKT2    MVI   ERROR,INVMKT                                                     
         B     STAERR                                                           
         SPACE 2                                                                
* VALIDATE SPOT LENGTH *                                                        
         SPACE                                                                  
VSLN     BRAS  RE,VALSLN                                                        
         B     XIT                                                              
         EJECT                                                                  
* COMMON ROUTINE TO LOCK RECS BY TYPE, OR CHECK TO SEE IF LOCKED *              
         SPACE                                                                  
* FOR SPOT                                                                      
*     DUB HAS 1 BYTE L,T,U, 1 BYTE COUNT 1-3 6 BYTES OF KEY TYPES *             
         SPACE                                                                  
*     LOCKTYP HAS RECORD TYPE 22 - PATTERN                                      
*                             24 - INST RECAP                                   
*                             25 - SHIP RECAP                                   
*     LOCKKEY 1 BYTE MEDIA                                                      
*             3 BYTES CLIENT                                                    
*             3 BYTES PRODUCT IF RUNNING BY PRODUCT                             
         SPACE                                                                  
* FOR NET MEDIA                                                                 
* DUB HAS 1 BYTE L,T,U, 3 BYTES OF "NET"                                        
*                                                                               
*     IF NET TRAFFIC                                                            
*     FIELD LABELED BYTE WILL HAVE AN ASTERISK FOR BY PRODUCT                   
*           OR PRD GRP LETTER FOR BY PRD GRP                                    
*           IF NOT BY PRD OR PRD GRP, BYTE = 00                                 
*        1 BYTE FF,  4 BYTES NETWORK, OR 1 BYTE FF AND MEDIA                    
*      OR                                                                       
*         1 BYTE OF PRODUCT - FF IS ALL PRODUCTS                                
*                 OR PRODUCT GROUP HAS 2 BYTES PGRP NUMBER                      
*     LOCKTYP IS UN FOR ALL BUT PAT GEN, WHICH IS 21                            
****************************************************************                
*     3 CHAR PRODUCT IS PASSED IN THE FIRST PARAMETER                           
****************************************************************                
*                                                                               
*     LOCKKEY 3 BYTES CLIENT                                                    
*             4 BYTES NETWORK OR X'FF' AND MEDIA                                
*             3 BYTES PRODUCT IF RUNNING BY PRODUCT                             
*                     OR 1 BYTE PRD GRP LETTER, 2 BYTES BINARY NUMBER           
         SPACE                                                                  
VLOC     DS    0H                                                               
         LR    R0,R1               SAVE PARAM 1 IF ANY                          
                                                                                
         CLI   DUB,C'L'            THIS LOCK                                    
         BE    VLOC10                                                           
         CLI   DUB,C'T'            THIS TEST                                    
         BE    VLOC10                                                           
         CLI   DUB,C'U'            THIS UNLOCK                                  
         BE    VLOC10                                                           
         DC    H'0'                                                             
VLOC10   CLC   =C'NET',DUB+1      IS THIS NETWORK                               
         BE    VLOC14               YES                                         
         CLI   DUB+1,X'FF'        IS THIS NETWORK BY MEDIA                      
         BE    VLOC14               YES                                         
         SPACE                                                                  
         CLI   DUB+1,1             ONE                                          
         BE    VLOC14                                                           
         CLI   DUB+1,2                TWO                                       
         BE    VLOC14                                                           
         CLI   DUB+1,3                 OR THREE ENTRIES OK                      
         BE    VLOC14                                                           
         SPACE                                                                  
         CLI   SPOTNETF,C'N'       IS THIS NET                                  
         BE    VLOC14                                                           
         SPACE                                                                  
         DC    H'0'                                                             
VLOC14   LA    R3,ELEM                                                          
         USING LKKEYD,R3                                                        
         XC    ELEM,ELEM                                                        
         L     R6,ACOMFACS                                                      
         CLI   OFFLINE,C'Y'                                                     
         BNE   VLOC20                                                           
         SPACE                                                                  
         L     RA,ATWA                                                          
         USING T216FFD,RA                                                       
         L     RE,TWAMASTC                                                      
         L     RF,MCUTL-MASTD(,RE)                                              
         MVC   LOCKSE,4(RF)        SE NUMBER FROM UTL                           
         B     VLOC30                                                           
         DROP  RA                                                               
         SPACE                                                                  
VLOC20   L     RF,CGETFACT-COMFACSD(,R6)                                        
         GOTO1 (RF),DMCB,0                                                      
         L     RE,DMCB             FAFACTS                                      
         MVC   LOCKSE,FASYS-FACTSD(RE)                                          
         SPACE                                                                  
VLOC30   MVC   LOCKAGY,AGENCY                                                   
         CLC   =C'NET',DUB+1       IS THIS NETWORK                              
         BE    VLOC33                                                           
         SPACE                                                                  
         CLI   DUB+1,X'FF'         IS THIS NETWORK (REALLY BY MEDIA)            
         BE    VLOC33                                                           
         SPACE                                                                  
         CLC   DUB+1(3),=X'010A25' IS THIS NETWORK SHIP GEN                     
         BE    VLOC31                                                           
         SPACE                                                                  
         CLI   SPOTNETF,C'N'       IS THIS NET                                  
         BE    VLOC32                                                           
         SPACE                                                                  
VLOC31   DS    0H                                                               
         MVC   LOCKKEY+2(4),QMED & QCLT                                         
         SPACE                                                                  
         CLI   SVT1PR16,C'Y'       RUN BY PRODUCT                               
         BNE   VLOC40                                                           
         MVC   LOCKKEY+6(3),QPRD                                                
         B     VLOC40                                                           
         SPACE                                                                  
VLOC32   CLC   DUB+1(2),=X'0A25'   MUST BE NEW NET SHIP                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   LOCKRTY(4),=C'0A25'                                              
         MVC   LOCKKEY+2(3),QCLT                                                
         MVC   LOCKKEY+5(4),DUB+3  NETWORK                                      
         LA    R5,1                SET FOR ONLY 1 ITERATION                     
         B     VLOC40                                                           
         SPACE                                                                  
VLOC33   MVC   LOCKRTY,=C'UN'                                                   
         MVC   LOCKKEY(3),QCLT                                                  
         SPACE                                                                  
         LA    R5,1                SET FOR ONLY 1 ITERATION                     
         SPACE                                                                  
         CLI   DUB+4,X'21'         THIS PATTERN GEN                             
         BNE   VLOC34                                                           
         MVC   LOCKRTY,=C'21'                                                   
         SPACE                                                                  
         CLI   DUB+5,X'FF'         THIS PATTERN GEN BY NET                      
         BNE   VLOC40               NO                                          
         SPACE                                                                  
         MVC   LOCKKEY+3(4),FULL   LOCK BY NETWORK AS WELL                      
         B     VLOC40                                                           
         SPACE                                                                  
* THE NEXT MOVE WILL MOVE IN EITHER 4 BYTE NETWORK, OR X'FF' AND MEDIA          
         SPACE                                                                  
VLOC34   DS    0H                                                               
         MVC   LOCKKEY+3(4),DUB+1                                               
         SPACE                                                                  
         CLI   DUB+1,X'FF'         THIS NET TRAFFIC?                            
         BNE   VLOC40               NO, MUST BE MEDIA                           
         SPACE                                                                  
         MVC   LOCKKEY+3(4),DUB+2                                               
         SPACE                                                                  
         CLI   BYTE,0              THIS NOT BY PRD OR PRD GRP                   
         BE    VLOC40               YES                                         
         SPACE                                                                  
         CLI   BYTE,C'*'           THIS A BY PRODUCT                            
         BNE   VLOC38               NOPE                                        
         SPACE                                                                  
         CLI   DUB+6,X'FF'         CAN'T BE ALL PRODS                           
         BNE   *+6                                                              
         DC    H'0'                                                             
         SPACE                                                                  
         CLI   SPOTNETF,C'S'       IS THIS NET?                                 
         BE    VLOC34A              NO, OKAY AS IS                              
         LTR   R0,R0                                                            
         BNZ   VLOC36                                                           
         SPACE                                                                  
VLOC34A  LA    R0,220                                                           
         L     R1,ASVNCLST                                                      
         SPACE                                                                  
         CLI   SPOTNETF,C'N'       IS THIS NET?                                 
         BNE   VLOC35               NO, OKAY AS IS                              
         SPACE                                                                  
         LA    R0,NCLSTSIZ         NOW 253 PRODS                                
         L     R1,ASVNCLST         ADDR OF NEW LIST                             
         SPACE                                                                  
VLOC35   DS    0H                                                               
         CLC   3(1,R1),DUB+6                                                    
         BE    VLOC37                                                           
         LA    R1,4(,R1)                                                        
         CLI   0(R1),C' '                                                       
         BNH   *+8                                                              
         BCT   R0,VLOC35                                                        
         DC    H'0'                WHAT, NOT A PRODUCT?                         
         SPACE                                                                  
******** MVC   LOCKKEY+7(3),0(R1)                                               
         SPACE                                                                  
VLOC36   LR    R1,R0               3 CHAR PROD                                  
         OC    0(3,R1),0(R1)                                                    
         BNZ   *+6                                                              
         DC    H'0'                NO PROD ?                                    
         SPACE                                                                  
VLOC37   MVC   LOCKKEY+7(3),0(R1)                                               
         CLI   DUB+4,X'21'         THIS BETTER NOT BE PATTERN GEN               
         BNE   VLOC40                                                           
         DC    H'0'                CAN'T BE PROD SPECIFIC AND PAT GEN           
         SPACE                                                                  
* MOVE IN PRODUCT GROUP HERE                                                    
         SPACE                                                                  
VLOC38   DS    0H                                                               
         MVC   LOCKKEY+7(1),BYTE                                                
         MVC   LOCKKEY+8(2),DUB+6                                               
         SPACE                                                                  
VLOC40   DS    0H                                                               
         XC    DMCB(4),DMCB                                                     
         MVC   DMCB+4(3),=X'D9000A'                                             
         MVI   DMCB+7,QLOCKET                                                   
         GOTO1 CALLOV,DMCB                                                      
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     R2,DMCB                                                          
         XC    DMCB(24),DMCB                                                    
         IC    RA,DUB                                                           
         SPACE                                                                  
         CLC   =C'NET',DUB+1       IS THIS NETWORK                              
         BE    VLOC60               YES                                         
         SPACE                                                                  
         CLI   DUB+1,X'FF'         IS THIS NETWORK                              
         BE    VLOC60               YES                                         
         SPACE                                                                  
         CLC   DUB+1(3),=X'010A25' IS THIS NETWORK SHIP GEN                     
         BE    VLOC44                                                           
         SPACE                                                                  
         CLI   SPOTNETF,C'N'       IS THIS NET                                  
         BE    VLOC60                                                           
         SPACE                                                                  
VLOC44   DS    0H                                                               
         LA    R4,DUB+2                                                         
         ZIC   R5,DUB+1                                                         
VLOC50   GOTO1 HEXOUT,(R1),(R4),LOCKRTY,2,0,0                                   
         SPACE                                                                  
VLOC60   DS    0H                                                               
         CLI   DUB,C'L'            THIS LOCK                                    
         BNE   *+6                                                              
         DC    X'0700'                                                          
         GOTO1 (R2),(R1),((RA),ELEM),(R6)                                       
         SPACE                                                                  
         CLI   DMCB+4,0            ANY ERRORS                                   
         BNE   LOCKER                                                           
         LA    R4,2(,R4)                                                        
         BCT   R5,VLOC50                                                        
         B     XIT                                                              
         DROP  R3                                                               
                                                                                
LOCKER   L     RA,ATWA             LOCKET ERROR                                 
         USING T216FFD,RA                                                       
         LA    R2,CONRECH                                                       
         MVC   GERROR,=Y(CLILOCK)                                               
         GOTO1 VTRAERR                                                          
*                                                                               
         USING T216FFD,RA                                                       
SECERR   LA    R2,CONRECH                                                       
         MVI   ERROR,SECLOCK                                                    
         B     TRAPERR                                                          
*                                                                               
ERRXIT2  DS   0H                                                                
         L     RA,ATWA                                                          
         USING T216FFD,RA                                                       
         XC    CONHEAD,CONHEAD                                                  
         EX    RE,MVCERRMS                                                      
         LA    R2,CONRECH                                                       
ERREXIT2 GOTO1 ERREX2                                                           
MVCERRMS MVC   CONHEAD(0),0(RF)                                                 
         DROP  RA                                                               
         EJECT                                                                  
* VALIDATE ESTIMATE                                                             
         SPACE                                                                  
VEST     DS    0H                                                               
         ICM   RE,15,0(R1)                                                      
         LTR   RE,RE               VALIDATING ESTIMATE?                         
         BZ    VEST00                                                           
         BRAS  RE,PRCIOREQ         PROCESS I/O REQUEST                          
         J     XIT                                                              
*                                                                               
VEST00   OC    BCLT,BCLT           WAS CLIENT ENTERED                           
         BNZ   *+14                                                             
         MVC   GERROR,=AL2(MISCLTE)                                             
         B     VERR                                                             
*                                                                               
         MVC   GERROR,=AL2(BADESTSZ)                                            
         CLI   5(R2),1                                                          
         BL    VERR                                                             
         CLI   5(R2),3                                                          
         BH    VERR                                                             
         XC    GERROR,GERROR                                                    
         SPACE                                                                  
         GOTO1 VALINUM                                                          
         SPACE                                                                  
         MVC   BEST,ACTUAL                                                      
         ZIC   R0,BEST                                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  QEST,DUB                                                         
         SPACE                                                                  
VEST10   XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING ESTHDRD,R4                                                       
         MVC   EKEYAM(3),BAGYMD                                                 
         MVC   EKEYPRD,QPRD                                                     
         SPACE                                                                  
         CLI   BPRD,0              WAS PRODUCT ENTERED                          
         BNE   *+10                 YES                                         
         MVC   EKEYPRD,=C'POL'     USE POL                                      
         MVC   EKEYEST,BEST                                                     
         MVI   RDUPDATE,C'N'       NOT READ FOR UPDATE                          
         MVC   FILENAME,SPTDIR                                                  
         SPACE                                                                  
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+14                                                             
         MVC   GERROR,=AL2(ESTNF)                                               
         B     VERR                                                             
*                                                                               
         MVI   RDUPDATE,C'N'       NOT READ FOR UPDATE                          
         MVC   FILENAME,SPTFIL                                                  
         SPACE                                                                  
         GOTO1 GETREC                                                           
         L     R4,AIO                                                           
         SPACE                                                                  
         XC    FILENAME,FILENAME                                                
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*                                                                               
* SWITCH TO SPOT OR TRAFFIC SYSTEM                                              
*                                                                               
VSWIT    CLI   SPOTNETF,C'N'       IF NETWORK, NO SWITCH                        
         BE    XIT                                                              
         SPACE                                                                  
         LR    RF,R1                                                            
         LR    R2,R1                                                            
         CLC   SPTDIR(3),0(R1)                                                  
         BE    VSWIT20                                                          
         CLC   =C'STR',0(R1)                                                    
         BE    VSWIT10                                                          
         DC    H'0'                                                             
         SPACE                                                                  
VSWIT10  LA    RF,=C'TRF'                                                       
         SPACE                                                                  
VSWIT20  MVC   SYSDIR(3),0(RF)                                                  
         MVC   SYSFIL(3),0(RF)                                                  
         MVI   LSTATUS+1,1                                                      
         MVI   LKEY+1,13                                                        
         MVI   DATADISP+1,24                                                    
         SPACE                                                                  
         ICM   RF,15,SWITCH        IF ONLINE, GET TO IT                         
         BNZ   VSWIT30                                                          
         CLI   OFFLINE,C'Y'        IF OFFLINE, SEE IF SPOT FILE OPEN            
         BE    VSWIT40                                                          
         DC    H'0'                                                             
*                                                                               
VSWIT30  GOTO1 (RF),DMCB,(R2),0                                                 
         CLI   4(R1),0             SWITCH OK?                                   
         BE    XIT                                                              
         SPACE                                                                  
         L     RA,ATWA                                                          
         USING T216FFD,RA                                                       
         CLI   4(R1),1             TEST USER NOT AUTHORIZED FOR SYSTEM          
         BE    SWERR10                                                          
         CLI   4(R1),2             TEST SYSTEM NOT OP                           
         BE    SWERR20                                                          
         DC    H'0'                                                             
SWERR10  DS   0H                                                                
         LA    RE,34                                                            
         LARL  RF,NOTAUTMS                                                      
         MVC   WORK(L'NOTAUTMS),0(RF)                                           
         LA    RF,WORK                                                          
         MVC   31(3,RF),0(R2)                                                   
         MVI   GENSTAT2,USMYOK                                                  
         B     ERRXIT2                                                          
         SPACE                                                                  
SWERR20  MVC   GERROR,=Y(SPTRDONL) SPOT FILE - READ ONLY                        
         GOTO1 VTRAERR                                                          
         DROP  RA                                                               
         SPACE                                                                  
* ONLY IF OFFLINE, OPEN SPOT FILE IF NOT OPEN ALREADY *                         
         SPACE                                                                  
VSWIT40  ICM   R3,15,UTL           MUST BE A UTL ADDRESS                        
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                  OPEN SPTDIR/SPTFIL IF NEEDED                 
         CLI   SPTOPEN,C'Y'                                                     
         BE    VSWIT50                                                          
*                                                                               
         MVC   TRFSE#,4(R3)        SAVE TRAFFIC SENUM                           
         GOTO1 DATAMGR,DMCB,(0,DDNAME),TRFSE,0    SE=0# (PASS TRFSE#)           
         TM    8(R1),X'10'         SYSTEM NOT FOUND                             
         JO    *+2                                                              
         LT    RE,8(,R1)           GET A(DDNADATA)                              
         JZ    *+2                                                              
*                                                                               
         USING DDNAMED,RE                                                       
         MVC   TRFSYSC,DDNASEN3+1  SENAME                                       
*                                                                               
         GOTO1 DATAMGR,DMCB,(0,DDNAME),TRFSYS,0 S=SENAME (S=D1 >> STR1)         
         TM    8(R1),X'10'         SYSTEM NOT FOUND                             
         JO    *+2                                                              
         LT    RE,8(,R1)           GET A(DDNADATA)                              
         JZ    *+2                                                              
         MVC   SPTSE#,DDNASENO     SPOT SENUM                                   
         DROP  RE                                                               
*                                                                               
         MVC   4(1,R3),SPTSE#                                                   
*                                                                               
         L     RA,ATWA                                                          
         USING T216FFD,RA                                                       
*                                                                               
         CLC   =C'SPOT',CONREC                                                  
         BNE   VSWIT44C                                                         
*                                                                               
         CLC   =C'GEN',CONACT                                                   
         BE    VSWIT45                                                          
         SPACE                                                                  
         CLC   =C'SEE',CONACT                                                   
         BE    VSWIT45                                                          
         B     VSWIT46                                                          
         SPACE                                                                  
VSWIT44C CLC   =C'DINS',CONREC                                                  
         BNE   VSWIT46                                                          
         SPACE                                                                  
         CLC   =C'GEN',CONACT                                                   
         BNE   VSWIT46                                                          
         SPACE                                                                  
VSWIT45  L     RE,TWAMASTC                                                      
         CLI   MCRECOVR-MASTD(RE),C'Y'                                          
         BE    VSWIT48                                                          
         SPACE                                                                  
VSWIT46  MVI   SPRECV,C'X'         DO NOT OPEN RECOVERY FILE                    
         SPACE                                                                  
VSWIT48  L     RE,TWAMASTC                                                      
         TM    MCOPT1-MASTD(RE),MCQ1XSP  TEST NEED EXTENDED SPOT FILES          
         BZ    VSWIT49                                                          
         SPACE                                                                  
         MVI   XSPDIR,C' '                                                      
         MVI   XSPFIL,C' '                                                      
         SPACE                                                                  
VSWIT49  L     R4,AIO1             USE AS WORK AREA FOR OPEN                    
         SPACE                                                                  
         GOTO1 DATAMGR,DMCB,=C'OPEN',=CL7'SPO',FILELIST,(R4)                    
         MVI   SPTOPEN,C'Y'                                                     
         MVC   SPTSYS,4(R3)        SAVE SPOT SENUM                              
         SPACE                                                                  
VSWIT50  CLI   SYSDIR,C'S'                                                      
         BNE   VSWIT54                                                          
         MVC   4(1,R3),SPTSYS                                                   
         B     XIT                                                              
VSWIT54  CLI   SYSDIR,C'T'                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   4(1,R3),TRFSE#                                                   
         B     XIT                                                              
         EJECT                                                                  
*          DATA SET GEGENCON   AT LEVEL 061 AS OF 06/11/98                      
VPER     L     R3,0(R1)            PERIOD                                       
         ZIC   R5,0(R1)            POSSIBLE MONTH YEAR OPTION                   
         LA    R4,8(R2)                                                         
         GOTO1 DATVAL,DMCB,((R5),(R4)),(R3)                                     
         ZIC   R0,DMCB+3                                                        
         LTR   R0,R0                                                            
         BZ    VVALDERR                                                         
         AR    R4,R0                                                            
         MVC   6(6,R3),0(R3)       ASSUME END = START                           
         CLI   0(R4),C' '                                                       
         BE    XIT                                                              
         CLI   0(R4),0                                                          
         BE    XIT                                                              
         CLI   0(R4),C'-'          - OR , ACCEPTABLE DELIMITERS                 
         BE    *+12                                                             
         CLI   0(R4),C','                                                       
         BNE   VVALDERR                                                         
         LA    R4,1(R4)                                                         
         LA    R3,6(R3)                                                         
         GOTO1 (RF),(R1),((R5),(R4)),(R3)                                       
         CLI   DMCB+3,0                                                         
         BE    VVALDERR                                                         
         AHI   R3,-6                                                            
         CLC   0(6,R3),6(R3)                                                    
         BNH   XIT                                                              
         MVI   ERROR,INVEBFRS                                                   
         B     TRAPERR                                                          
VVALDERR MVI   ERROR,INVDATE                                                    
         B     TRAPERR                                                          
         DROP  RA                                                               
         EJECT                                                                  
         DS    0H                                                               
SIGNONE1 DS   0H                                                                
         LA    RE,L'SIGNONM1-1                                                  
         LARL  RF,SIGNONM1                                                      
         B     ERRXIT2                                                          
         SPACE                                                                  
WRITERR  DS   0H                                                                
         TM    ISFOPEN-ISDTF(RE),ISFONOP   NOP?                                 
         BO    WRITERRA                                                         
         LA    RE,L'WRITERMS-1                                                  
         LARL  RF,WRITERMS                                                      
         B     ERRXIT2                                                          
         SPACE                                                                  
WRITERRA DS   0H                                                                
         LA    RE,L'WRITERMA-1                                                  
         LARL  RF,WRITERMA                                                      
         B     ERRXIT2                                                          
         SPACE                                                                  
AGYERR   DS   0H                                                                
         LA    RE,L'AGYMS-1                                                     
         LARL  RF,AGYMS                                                         
         B     ERRXIT2                                                          
         DS    0H                                                               
         EJECT                                                                  
*                                                                               
* THIS ROUTINE CALLS GENCON'S ERREX ROUTINE AND ASKS FOR GETTXT CALLS           
         SPACE                                                                  
VERR     OI    GENSTAT2,USGETTXT   FLAGS ERREX TO CALL GETTXT                   
         LA    RF,GETTXTCB                                                      
         USING GETTXTD,RF                                                       
         MVC   GTINDX,GINDEX       MESSAGE INDEX                                
         MVC   GTMSGNO,GERROR      MESSAGE NUMBER                               
         MVC   GTMTYP,GMSGTYPE     MESSAGE TYPE                                 
         MVC   GTASUBST,GASUBST    A(TABLE OF SUBST TEXT(S))                    
         MVI   GTMSYS,X'0D'        ALL MESSGES FROM STRAFFIC (X'0D')            
*                                                                               
         CLC   GERROR,=H'255'                                                   
         BH    *+8                                                              
         MVI   GTMSYS,X'05'        MESSAGES 1-255 LIVE HERE                     
*                                                                               
         CLC   GERROR,=H'60'       MESSAGE NUMBER <= 60 ?                       
         BH    *+8                 NO                                           
         MVI   GTMSYS,X'FF'        USE GENERAL SYSTEM                           
*                                                                               
         LA    RE,ERRTEXT          AREA FOR APPENDED ERR TEXT                   
         CLI   0(RE),C' '          TEST IF THERE IS ANY TEXT                    
         BL    *+12                                                             
         ST    RE,GTATXT-1                                                      
         MVI   GTLTXT,L'ERRTEXT                                                 
         DROP  RF                                                               
*                                                                               
TRAPERR  GOTO1 ERREX                                                            
         SPACE                                                                  
XIT      XIT1                                                                   
         SPACE                                                                  
         GETEL (R6),DATADISP,ELCODE                                             
         EJECT                                                                  
* CONSTANTS TABLES, ETC *                                                       
         EJECT                                                                  
*        DIRECTORY OF PROGRAMS                                                  
*                                                                               
* SYST                     ACTION           PROG SCRN                           
* SPOT/NET/BOTH                                                                 
*  S     COMMERCIAL        ADD/CHA/DIS       02*  F2                            
*  S                       LIST              02   E2                            
*  S                       USAGE             48   F0                            
*  S     COMMERCIAL TEXT   ADD/CHA/DIS       31   E1                            
*  S                       LIST              31   D1                            
*  S     COMMERCIAL CLASS  ADD/CHA/DIS       69   7A                            
*  S                       LIST              69   79                            
*  N     COMMERCIAL        ADD/CHA/DIS       22*  E4                            
*  N                       LIST              22   E2                            
*  S     COMMERCIAL(CANADA)ADD/CHA/DIS       32*  E5                            
*  S                  "    LIST              32   E4                            
*  S                       RECAP (NOTE 1)    42+  D2                            
*  S     PATTERN           ADD/CHA/DIS       03*  F3                            
*  S                       LIST              23   E3                            
*  S                       RECAP (NOTE 2)    43+  D3                            
*  S     BOOKLET           LIST              60   70                            
*           BOOKLET IS FOR JWT WARNER LAMBERT                                   
*  S     MANINST           GENERATE          04   F6 BASE 04 CALLS 06           
*  S     INST              RECAP (NOTE 3)    44   D4 SEL=PAT F3 DET             
*  S     AUTOINST          GENERATE (NOTE 7) 04   F7 BASE 04 CALLS 07           
*  S     TWXAUTO           GENERATE          24   F7 BASE 24 CALLS 07           
*  S                                         92   AF THIS WAS TWX LIST          
*  S     SHIPPING          CHA/DIS           05   F5                            
*  S                       GENERATE          46   D6                            
*  S                       RECAP (NOTE 4)    45+  D5                            
*  S     CONTACT           ADD/CHA/DIS/DEL   08   F8                            
*  S                       LIST              08   E8                            
*  S     FLIGHT            ADD/CHA/DIS       0A*  FA                            
*  S                       LIST              0A   EA                            
*  B     STATION           ADD/CHA/DIS/DEL   0B   FB                            
*  B                       LIST              0B   EB                            
*  N     STATION           ADD/CHA/DIS       56   5F                            
*  N                       LIST              56   5E                            
*  S                       LABEL             4B   DB                            
*  S     LABEL LIST        ADD/CHA/DIS/DEL   10   B0                            
*  S                       LIST              10   A0                            
*  S     HOUSE             ADD/CHA/DIS/DEL   0C   FC                            
*  S                       LIST              0C   EC                            
*  S     SATEL             ADD/CHA/DIS/DEL   0D   FD                            
*  S                       LIST              0D   ED                            
*  S     TEXT              ADD/CHA/DIS/DEL   0E   FE                            
*  S                       LIST              0E   EE                            
*  N     TEXT              ADD/CHA/DIS/DEL   0E   82                            
*  N                       LIST              0E   80                            
*  S     BUYACT            REPORT            25   B1                            
*  S     BUYACT (TAM/TCM)  DISPLAY           11   B1                            
*  S                       LIST              11   A1                            
*  S     STEXT             ADD/CHA/DIS/DEL   13   B3 (SAME AS DLR TEXT)         
*                          LIST              13   A3                            
*  S     DPEQV             ADD/CHA/DIS/DEL   33   D8 DAYPART EQUIV              
*                          LIST              33   D7                            
*  S     CACT              LIST              47   DD                            
*  S     TALCOM            LIST              49   8F                            
*  S     COVER LETTER INS  GENERATE          34   D9                            
*  S     MARKET LETTER INS GENERATE          35   7F                            
*  S     AMS LETTER INS    GENERATE          36   7E                            
*  S     PURGE             PURGE             94   E7                            
*  S     SPOT              ASSIGN            40   D0                            
*  S     SPOT              GEN               4A   DA                            
*  S     SPOT              LIST              65   75                            
*  S     SPOT              SEED              2A   78                            
*  S     PRDEQV            ADD/CHA/DIS/DEL   64   84                            
*  S     PRDEQV            LIST              64   74                            
*  S     PRODUCT LIST      ADD/CHA/DIS       3A   8B                            
*  S                       LIST              3A   7B                            
*                                                                               
*  *-DELETE NOT ALLOWED  +-NO SELECT FUNCTION                                   
         EJECT                                                                  
* DEALER TAGS                                                                   
*  S     DLR               ADD/CHA/DIS/DEL   12   B2                            
*  S                       LIST              12   A2                            
*  S     DTEXT             ADD/CHA/DIS/DEL   13   B3                            
*  S                       LIST              13   A3                            
*  S     SPOT              TAG               14   B4                            
*  S                       RECAP (NOTE 5)    54+  A4                            
*  S     DINST             GEN   (NOTE 8)    15   B5                            
*                          RECAP (NOTE 6)    55+  A5                            
*                                                                               
         SPACE                                                                  
* SYST                     ACTION           PROG SCRN                           
* SPOT/NET/BOTH                                                                 
* NEW TRAFFIC BUY                                                               
*  S     TBUY              ASSIGN            16   B6                            
*  S     TBUY              BUY               26   E6                            
*  S                       LIST              16   A6                            
*  S     MKTLIST           ADD/CHA/DIS       17   B7                            
*  S                       LIST              17   A7                            
*  S                       GENERATE          93   A7                            
*  S     MLIST             ADD/CHA/DIS       51   4C                            
*  S                       LIST              51   4D                            
         SPACE                                                                  
*  *-DELETE NOT ALLOWED  +-NO SELECT FUNCTION                                   
         EJECT                                                                  
* NETWORK TRAFFIC                                                               
*  N     PROGRAM LIST      ADD/CHA/DIS       18   B8                            
*  N                       LIST              18   A8                            
*  N     CLIENT LIST       ADD/CHA/DIS       19   B9                            
*  N                       LIST              19   A9                            
*  N     PRODUCT LIST      ADD/CHA/DIS       1A   BA                            
*  N                       LIST              1A   AA                            
*  N     FEED              ADD/CHA/DIS       1B   BB                            
*  N                       LIST              1B   AB                            
*  N     NETWORK           ASSIGN            1C   BC                            
*  N                       LIST              2C   AC                            
*  N     REVISION COMMENTS ADD/CHA/DIS       1D   BD                            
*  N                       LIST              1D   AD                            
*  N     NETWORK SCHEDULE  ADD/DIS/DEL       1E   BE                            
*  N                       LIST              1E   AE                            
*  N     NETWORK INSTR     GENERATE          1F   BF                            
*  N     PROGRAM DAY SPREAD CHA/DIS          20   DE                            
*  N                       LIST              20   DC                            
*  N     PATTERN           ADD/CHA/DIS       61   71                            
*  N                       LIST              61   81                            
*  N                       GEN               50   85                            
*  N     EQVPGM            ADD/CHA/DIS       66   76                            
*  N                       LIST              66   86                            
*  N     BILLING (MARGA)   LIST              91   AC                            
*  N     COMMENT           ADD/CHA/DIS/DEL   53   83                            
*  N                       LIST              53   88                            
*  N     COMMENT           DIS/CHA           2F   4F                            
*  N                       LIST              2F   3F                            
*                                                                               
*  *-DELETE NOT ALLOWED  +-NO SELECT FUNCTION                                   
*                                                                               
*   1. COMMERCIAL RECAP SHOWS WHAT STATIONS HAVE THIS COMMERCIAL                
*   2. PATTERN RECAP SHOWS WHAT STATIONS HAVE THIS PATTERN                      
*   3. INSTRUCTION RECAP SHOWS WHAT INSTRUCTIONS ARE AT THIS STATION            
*   4. SHIPPING RECAP SHOWS WHAT COMMERCIALS ARE AT THIS STATION                
*   5. SPOT RECAP SHOWS SPOTS BY DEALER (TAG)                                   
*   7. AUTO INSTR (04) CALLS 98 TO GENERATE OFFLINE AUTO REQUESTS               
*      AND ALSO CALLS 97 FOR ERROR LIST.                                        
*                                                                               
*    FREE SCREENS  B2 F6 F7 (MHER 9/16/08)                                      
*    FREE PROGRAMS 01 09 28-29 2B 2D 2E 30 38-39 3B-3E 4E                       
*                  52 58-5D 6C-6E                                               
*                  06 0F 12 15 24 37 6A 6B (MHER 9/16/08)                       
*    8C USED FOR STORAGE AND CAN BE RE-USED                                     
*    59 USED FOR STORAGE AND CAN BE RE-USED                                     
*    5A USED FOR NETWORK PATTERN COMMENT SCREEN                                 
*                                                                               
         EJECT                                                                  
* PROFILES USED BY TRAFFIC                                                      
*                                                                               
* SPOT T0 - MISC                                                                
*      T1 - MORE MISC                                                           
*      T2 - MORE MISC                                                           
*      T9 - TRAFFIC PLAN REPORT - BOB Z.                                        
*      TA - ACTIVITY                                                            
*      TB - TBUY                                                                
*      TC - COVER LETTER                                                        
*      TI - INVOICE MATCHING                                                    
*      TL - STATION LABELS                                                      
*      TS - SHIPPING ORDERS                                                     
*      TW - ELECTRONIC TRANSMISSION OF SPOT INSTR, COVER LETTERS, AND           
*           SHIPPING ORDERS                                                     
*      TZ - SPOT ASSIGN/GEN                                                     
*                                                                               
* NET  TN - MISC                                                                
* NET  TN1- MORE MISC                                                           
* NET  TN2- MORE MISC                                                           
         EJECT                                                                  
* HELP SCREEN LAYOUTS                                                           
*                                                                               
* PHASE      MESSAGES                                                           
*                                                                               
*   CF                        RECORD    HELP SCREEN FOR SPOT                    
*                                                                               
*   C0                        RECORD    HELP SCREEN FOR NET                     
*                                                                               
*   C2                        A(DD)     ADD A NEW RECORD                        
*                             C(HANGE)  DISPLAY/CHANGE EXISTING RECORD          
*                             D(ISPLAY) DISPLAY AN EXISTING RECORD              
*                             L(IST)    SHOW LIST OF RECORDS FILTERED           
*                             REC(AP)   RECAP STATIONS WITH THIS CML            
*                                                                               
*   C3                        A(DD)     ADD A NEW RECORD                        
*                             C(HANGE)  DISPLAY/CHANGE EXISTING RECORD          
*                             D(ISPLAY) DISPLAY AN EXISTING RECORD              
*                             D(ELETE)  DELETE EXISTING RECORD                  
*                             R(ESTORE) RESTORE DELETED RECORD                  
*                             L(IST)    SHOW LIST OF RECORDS FILTERED           
*                                                                               
*  C4                         GEN(ERATE) GENERATE MANUAL INSTRUCTIONS           
*                             REC(AP)    STATIONS WITH THIS PATTERN             
*                                        SEL SHOWS EACH PATTERN                 
*                                                                               
*  C5                         GEN(ERATE) GENERATE SHIPPING INSTRUCTIONS         
*                             REC(AP)    RECAP STATIONS WITH THIS INSTR         
*                                                                               
*  C6                         GEN(ERATE) INSTRUCTIONS                           
*                                                                               
*  C7                         TAG   - ASSIGN DEALER TAGS TO SPOTS               
*                             RECAP - LIST TAGGED SPOTS BY DEALER               
*                                                                               
*  C8                         A(DD)     ADD A NEW RECORD                        
*                             D(ISPLAY) DISPLAY AN EXISTING RECORD              
*                             C(HANGE)  DISPLAY/CHANGE EXISTING RECORD          
*                             DELETE    DELETE AN EXISTING RECORD               
*                             R(ESTORE) RESTORE A DELETED RECORD                
*                             L(IST)    SHOW LIST OF RECORDS FILTERED           
*                             L(ABEL)   PRINT STATION ADDRESS LABELS            
*                                                                               
*  C9                         A(DD)     ADD A NEW RECORD                        
*                             C(HANGE)  DISPLAY/CHANGE EXISTING RECORD          
*                             D(ISPLAY) DISPLAY AN EXISTING RECORD              
*                             L(IST)    SHOW LIST OF RECORDS FILTERED           
*                                                                               
*  CA                         L(IST)    SHOW LIST OF RECORDS FILTERED           
*                                                                               
*  CB                         G(ENERATE) GENERATE INSTRUCTIONS                  
*                             L(IST)    SHOW LIST OF RECORDS FILTERED           
*                                                                               
*  CC                                                                           
*                             L(IST)    SHOW LIST OF RECORDS FILTERED           
*                             A(SSIGN)  ASSIGN BUYS                             
*                                                                               
*  CD                         C(HANGE)  DISPLAY/CHANGE EXISTING RECORD          
*                             D(ISPLAY) DISPLAY AN EXISTING RECORD              
*                             L(IST)    SHOW LIST OF RECORDS FILTERED           
*                                                                               
*  CE                         A(DD)     ADD A NEW RECORD                        
*                             D(ISPLAY) DISPLAY AN EXISTING RECORD              
*                             D(ELETE)  DELETE EXISTING RECORD                  
*                             R(ESTORE) RESTORE DELETED RECORD                  
*                             L(IST)    SHOW LIST OF RECORDS FILTERED           
*                                                                               
*  DF                         PURGE     PURGE TRAFFIC RECORDS                   
*                                                                               
*                                                                               
         TITLE 'T21600 - TRAFFIC EOD REPORT CODES AND MEANINGS'                 
* REPORT - PROGRAM(S) USING           15 USED  21 FREE 24 DUPES                 
* CODE                                                                          
*                                                                               
*  TA    SPOT AUTO GEN                                                          
*  TB    NET CABLE GEN                                                          
*  TC    SPOT RECAPS - COMML, PATTERN, INSTR, SHIP,             (5)             
*                      SPOT, DINS (DEALER TAGS)                                 
*  TD                                                                           
*  TE                                                                           
*  TF    FAX LETTER                                                             
*  TG    DEALER INST                                                            
*  TH                                                                           
*  TI                                                                           
*  TJ    AMS LETTERS                                                            
*  TK    MARKET LETTERS                                                         
*  TL    SPOT LISTS - COMML, PATTERN, FLIGHT, STATION, LABEL,     (14)          
*                     HOUSE, TEXT, COMML ACTIVITY, DEALER TEXT,                 
*                     BUYACT, TBUY, GOAL, MKTLIST, DPEQV(DAY PART EQV)          
*                     PRODUCT EQUIV                                             
*        NET LISTS  - COMML, PATTERN, TEXT, PROGRAM (COPIES TO),   (9)          
*                     CLIENT (COPIES TO), PRODUCT (COPIES TO),                  
*                     PROGRAM D/S, FEED CODES, NET LIST, REVISIONS,             
*                     SKED, EQV PROGRAMS,                                       
*  MARKET LIST GENERATION MUST BE CHANGED                                       
*  TM    SPOT MANUAL INSTRUCTIONS                                               
*  TN    NET INSTRUCTIONS                                                       
*  TO    SPOT SHIPPING ORDERS                                                   
*  TP    SPOT PURGE/PATTERN GENERATE                                            
*  TQ    SPOT GEN INSTRUCTIONS                                                  
*  TR                                                                           
*  TS    NET SEED SPOT SEED                                                     
*  TT                               WAS SPOT TWX TRANS LIST                     
*  TU    SPOT COMML USAGE REPORT                                                
*  TV    SPOT COV GEN                                                           
*  TW                                                                           
*  TX                    AUTO TURN-AROUND SPOT - BUYACT, GOAL LISTS             
*                                         NET  - NET LIST                       
*  TY                                                                           
*  TZ                                                                           
*  T0                                                                           
*  T1                                                                           
*  T2                                                                           
*  T3                                                                           
*  T4                                                                           
*  T5                                                                           
*  T6                                                                           
*  T7                                                                           
*  T8                                                                           
*  T9    OLD SPONSOR TRAFFIC PLANNING REPORT                                    
*                                                                               
DDNAME   DC    CL8'DDNAME'                                                      
TRFSE    DC    C'SE=',X'00'                                                     
TRFSE#   DS    X                                                                
TRFSYS   DC    C'S=S  '                                                         
         ORG   TRFSYS+3                                                         
TRFSYSC  DS    CL2                                                              
*                                                                               
SPTSE#   DS    X                                                                
*                                                                               
*                                                                               
FILELIST DS   0D                                                                
         DC    CL8'USPTFILE'                                                    
XSPFIL   DC    CL8'ZXSPFILE'                                                    
         DC    CL8'NSPTDIR '                                                    
XSPDIR   DC    CL8'ZXSPDIR '                                                    
         DC    CL8'NSTAFIL '                                                    
         DC    CL8'NCTFILE'                                                     
SPRECV   DC    CL8'URECV   '                                                    
         DC    CL10'X'                                                          
SPTOPEN  DC    X'00'                                                            
SPTSYS   DC    X'00'                                                            
UTL      DC    A(0)                                                             
         LTORG                                                                  
SPTDIR   DC    CL8'SPTDIR  '                                                    
SPTFIL   DC    CL8'SPTFIL  '                                                    
NOCLACC  DC    C'ACCESS TO THIS CLIENT IS NOT AUTHORIZED'                       
         TITLE 'T21600 - SPOT TRAFFIC SPOOL CONTROLLER - NETWORK TABLE'         
*      NETWORK DIRECTORY OF RECORDS AND ACTIONS                                 
         SPACE                                                                  
RECACTG  DS    0D                                                               
NETGRID  EQU   *+1                                                              
         DC    X'01',C'NET     ',AL1(32),X'00CC'                                
         DC    X'01',C'SPOT    ',AL1(24),X'00C7'                                
                                                                                
         DC    X'02',C'LIST    ',AL1(29,29,00)                                  
                                                                                
         DC    X'03',AL1(32,29),X'9C2C000081',C'    '   NET LIST GRID           
         DC    X'03',AL1(24,29),X'9D65000081',C'    '   SPOT LIST GRID          
         DC    X'FF'                                                            
                                                                                
RECACTN  DS    0D                                                               
         SPACE                                                                  
*                                  X'01' ENTRIES ARE AVAILABLE RECORDS          
*                                  CL8 EXPANDED RECORD NAME                     
*                                  CL1 RECORD NUMBER                            
*                                  CL1 PHASE NUMBER FOR DATA DICTIONARY         
*                                  CL1 PHASE NUMBER FOR HELP SCREEN             
* NETWORK TRAFFIC                                                               
         SPACE                                                                  
NETPAT   EQU   *+1                                                              
         DC    X'01',C'PATTERN ',AL1(03),X'00C9'                                
NETLIST  EQU   *+1                                                              
         DC    X'01',C'NET     ',AL1(32),X'00CC'                                
NETREV   EQU   *+1                                                              
         DC    X'01',C'REVISION',AL1(33),X'00CD'                                
NETSKED  EQU   *+1                                                              
         DC    X'01',C'SKED    ',AL1(34),X'00CE'                                
NINSGEN  EQU   *+1                                                              
         DC    X'01',C'NINST   ',AL1(35),X'00CB'                                
         DC    X'01',C'NINSPDF ',AL1(35),X'00CB'                                
NETEPRG  EQU   *+1                                                              
         DC    X'01',C'EQVPRG  ',AL1(38),X'00C9'                                
NETTSUP  EQU   *+1                                                              
         DC    X'01',C'TSUPP   ',AL1(43),X'00CD'                                
NETDYPT  EQU   *+1                                                              
         DC    X'01',C'DAYPART ',AL1(41),X'00C3'                                
STATION  EQU   *+1                                                              
         DC    X'01',C'STATION ',AL1(11),X'00C8'                                
         DC    X'01',C'COMML   ',AL1(02),X'00C9'                                
*NOP     DC    X'01',C'BCOMML  ',AL1(58),X'00C2' BROADCAST COMML                
*****    DC    X'01',C'LCOMML  ',AL1(59),X'00C2' LEGAL COMML                    
         DC    X'01',C'SHIPPING',AL1(05),X'00C5'                                
         DC    X'01',C'HOUSE   ',AL1(12),X'00C3'                                
         DC    X'01',C'COMCLASS',AL1(50),X'00C3'                                
         DC    X'01',C'PROGLIST',AL1(28),X'00C3'                                
         DC    X'01',C'LABELIST',AL1(36),X'00C3'                                
         DC    X'01',C'TEXT    ',AL1(14),X'00C3'                                
         DC    X'01',C'STEXT   ',AL1(23),X'00C3'                                
         DC    X'01',C'COMTEXT ',AL1(61),X'00C3'                                
         DC    X'01',C'CLTLIST ',AL1(29),X'00C3'                                
         DC    X'01',C'PRDLIST ',AL1(30),X'00C3'                                
         DC    X'01',C'PGRLIST ',AL1(45),X'00C3'                                
         DC    X'01',C'FEED    ',AL1(31),X'00C3'                                
         DC    X'01',C'PROGRAM ',AL1(36),X'00CB'                                
         DC    X'01',C'CABLE   ',AL1(37),X'00CB'                                
         DC    X'01',C'COMMENT ',AL1(39),X'00C3'                                
         DC    X'01',C'PRODUCT ',AL1(40),X'00CD'                                
         DC    X'01',C'HELP    ',AL1(00),X'00C0'                                
         DC    X'01',C'PURGE   ',AL1(42),X'00DF'                                
         DC    X'01',C'ADDRESS ',AL1(43),X'00DF'                                
         DC    X'01',C'BILLING ',AL1(98),X'0000'  ** OFFLINE ONLY **            
         DC    X'01',C'ZYXWVUTS',AL1(97),X'00DF'  ** OFFLINE ONLY **            
         DC    X'01',C'PRODDEL ',AL1(46),X'00C3'                                
NETVEND  EQU   *+1                                                              
         DC    X'01',C'VENDOR  ',AL1(41),X'00C3'                                
*                                                    NETPAK UNIT CK             
*                                                    CKS COMMLS OKAY            
         DC    X'01',C'ERRLIST ',AL1(99),X'0000'  ** OFFLINE ONLY **            
         SPACE 2                                                                
*                                  X'02' ENTRIES ARE AVAILABLE ACTIONS          
*                                  CL8 EXPANDED ACTION NAME                     
*                                  CL1 ACTION NUMBER                            
*                                  CL1 ACTION EQUATE                            
*                                  CL1 SPARE                                    
         SPACE                                                                  
         DC    X'02',C'ADD     ',AL1(01,01,00)                                  
         DC    X'02',C'CHANGE  ',AL1(02,01,00)                                  
         DC    X'02',C'DISPLAY ',AL1(03,01,00)                                  
         DC    X'02',C'DELETE  ',AL1(04,01,00)                                  
         DC    X'02',C'SELECT  ',AL1(05,01,00)                                  
         DC    X'02',C'RESTORE ',AL1(06,01,00)                                  
         DC    X'02',C'LIST    ',AL1(10,10,00)                                  
         DC    X'02',C'RECAP   ',AL1(10,13,00)                                  
         DC    X'02',C'GENERATE',AL1(14,14,00)                                  
         DC    X'02',C'ASSIGN  ',AL1(16,16,00)                                  
         DC    X'02',C'SEED    ',AL1(17,17,00)                                  
         DC    X'02',C'PURGE   ',AL1(19,19,00)                                  
*        DC    X'02',C'LGRID   ',AL1(29,29,00)                                  
         DC    X'02',C'HELP    ',AL1(00,00,00)                                  
         EJECT                                                                  
*              DIRECTORY OF PHASES FOR SELECTED RECORD/ACTION                   
         SPACE                                                                  
*                                  X'03' ENTRIES ARE OK REC/ACT COMBOS          
*                                  CL1 RECORD NUMBER                            
*                                  CL1 ACTION EQUATE                            
*                                  CL1 PHASE NUMBER FOR SCREEN                  
*                                  CL1 PHASE NUMBER FOR EDIT                    
*                                  CL1 PHASE NUMBER FOR SPECS                   
*                                  CL1 PHASE NUMBER FOR REPORT                  
*                                  CL1 WHEN OK BITS 80=SCREEN 40=NOW            
*                                      20=SOON 10=OV 08=DDS                     
*                                  CL2 CODE TO PRINT ON REPORT                  
*                                  CL2 CODE FOR EOD JCL BOOK                    
         SPACE                                                                  
*                                 SC  SP  AC                                    
*                                   OV  RP                                      
         DC    X'03',AL1(02,01),X'F422000080',C'    '  COMM     MAINT           
**NOP**  DC    X'03',AL1(58,01),X'6F22000080',C'    '  LCOMM    MAINT           
**NOP**  DC    X'03',AL1(59,01),X'9F22000080',C'    '  BCOMM    MAINT           
         DC    X'03',AL1(02,13),X'D2420042F8',C'CRTC'  COMML    RECAP           
         DC    X'03',AL1(02,10),X'E2220022F8',C'CLTL'           LIST            
         DC    X'03',AL1(50,01),X'7A69000080',C'    '  COM CLASSMAINT           
         DC    X'03',AL1(50,10),X'79690069F8',C'CLTL'           LIST            
         DC    X'03',AL1(03,01),X'8161000080',C'    '  PATTERN  MAINT           
         DC    X'03',AL1(03,10),X'71610061F8',C'PLTL'           LIST            
         DC    X'03',AL1(03,14),X'8550005060',C'PGTP'  PATTERN  GEN             
         DC    X'03',AL1(14,01),X'820E000080',C'    '  TEXT     MAINT           
         DC    X'03',AL1(14,10),X'800E000EF8',C'TLTL'           LIST            
         DC    X'03',AL1(23,01),X'B313000080',C'    '  STEXT    MAINT           
         DC    X'03',AL1(23,10),X'A3130013F8',C'STTL'           LIST            
         DC    X'03',AL1(61,01),X'E131000080',C'    '  COMTEXT  MAINT           
         DC    X'03',AL1(61,10),X'D1310031F8',C'CTTL'           LIST            
         DC    X'03',AL1(11,01),X'5F56000080',C'    '  STATION  MAINT           
         DC    X'03',AL1(11,10),X'5E56000080',C'    '           LIST            
         DC    X'03',AL1(28,01),X'B818000080',C'    '  PROGRAM  MAINT           
         DC    X'03',AL1(28,10),X'A8180018F8',C'PGTL'           LIST            
         DC    X'03',AL1(29,01),X'B919000080',C'    '  CLIENT   MAINT           
         DC    X'03',AL1(29,10),X'A9190019F8',C'CTTL'           LIST            
         DC    X'03',AL1(30,01),X'BA1A000080',C'    '  PRODUCT  MAINT           
         DC    X'03',AL1(30,10),X'AA1A001AF8',C'PDTL'           LIST            
         DC    X'03',AL1(45,01),X'B555000080',C'    '  PGRLIST  MAINT           
         DC    X'03',AL1(45,10),X'A5550055F8',C'PDTL'           LIST            
         DC    X'03',AL1(31,01),X'BB1B000080',C'    '  FEED     MAINT           
         DC    X'03',AL1(31,10),X'AB1B001BF8',C'PFTL'           LIST            
         DC    X'03',AL1(32,16),X'BC1C000080',C'    '  NET      ASSIGN          
         DC    X'03',AL1(32,29),X'9C2C000081',C'    '           GRID            
         DC    X'03',AL1(32,10),X'AC2C002CF8',C'NATI'           LIST            
         DC    X'03',AL1(32,17),X'7262006278',C'SETS'           SEED            
*        NET SEED CKS SOON - ONLY RUNS WITH OPTION TEST                         
         DC    X'03',AL1(33,01),X'BD1D000080',C'    '  REVISION MAINT           
         DC    X'03',AL1(33,10),X'AD1D001DF8',C'RVTL'           LIST            
         DC    X'03',AL1(34,01),X'BE1E000080',C'    '  SKED     MAINT           
         DC    X'03',AL1(34,10),X'AE1E001EF8',C'NATL'           LIST            
         DC    X'03',AL1(35,14),X'BF1F000078',C'NITN'  NET INST GEN             
         DC    X'03',AL1(37,14),X'7350000078',C'CGTB' CABLEINST GEN             
         DC    X'03',AL1(05,14),X'D646004678',C'SHTO'  SHIP     GEN             
         DC    X'03',AL1(05,13),X'D5450045F8',C'SRTC'  SHIPPING RECAP           
         DC    X'03',AL1(43,01),X'9A5A005A80',C'    ' SHP ADDR MAINT            
         DC    X'03',AL1(43,10),X'9B5A0045F8',C'SRTL'  SHP ADDR LIST            
         DC    X'03',AL1(12,01),X'FC0C000080',C'    '  HOUSE    MAINT           
         DC    X'03',AL1(12,10),X'EC0C000CF8',C'HLTL'           LIST            
         DC    X'03',AL1(36,01),X'DE20000080',C'    '  PROG D/S MAINT           
         DC    X'03',AL1(36,10),X'DC200020F8',C'PGTL'           LIST            
         DC    X'03',AL1(38,01),X'8666000080',C'    '  EQV PROG MAINT           
         DC    X'03',AL1(38,10),X'76660066F8',C'EPTL'           LIST            
         DC    X'03',AL1(39,01),X'8353000080',C'    '  COMMENT  MAINT           
         DC    X'03',AL1(39,10),X'88530053F8',C'CMTL'           LIST            
*        DC    X'03',AL1(40,01),X'8968000080',C'    '  PRODUCT  MAINT           
         DC    X'03',AL1(40,10),X'8A68000080',C'    '  PRODUCT  LIST            
         DC    X'03',AL1(41,01),X'8B2B000080',C'    '  DAYPART  MAINT           
         DC    X'03',AL1(41,10),X'7B2B0000F8',C'DPTL'           LIST            
         DC    X'03',AL1(43,01),X'4F2F000080',C'    '  TSUPP    MAINT           
         DC    X'03',AL1(43,10),X'3F2F0000F8',C'TSTL'           LIST            
         DC    X'03',AL1(42,19),X'E795009518',C'PRTP'  PURGE    PURGE           
         DC    X'03',AL1(97,10),X'E799009918',C'CKTL'  NETPAK OVERNIGHT         
*                                                      UNIT CHECK               
         DC    X'03',AL1(98,10),X'AC91009108',C'BITL'  BILLING  LIST            
         DC    X'03',AL1(46,01),X'A212000080',C'    '  PRODDEL  DIS             
         DC    X'FF'                                                            
         TITLE 'T21600 - SPOT TRAFFIC SPOOL CONTROLLER - SPOT TABLE'            
*        SPOT  DIRECTORY OF RECORDS AND ACTIONS                                 
         SPACE                                                                  
RECACTS  DS    0D                                                               
         SPACE                                                                  
*                                  X'01' ENTRIES ARE AVAILABLE RECORDS          
*                                  CL8 EXPANDED RECORD NAME                     
*                                  CL1 RECORD NUMBER                            
*                                  CL1 PHASE NUMBER FOR DATA DICTIONARY         
*                                  CL1 PHASE NUMBER FOR HELP SCREEN             
***********************************************                                 
**** NEXT AVAILABLE RECORD ACTION IS  61   ****                                 
***********************************************                                 
         SPACE                                                                  
SPTCOMML EQU   *+1                                                              
         DC    X'01',C'COMML   ',AL1(02),X'00C2'                                
         DC    X'01',C'CCOMML  ',AL1(48),X'00C2' CANADIAN COMML                 
*NOP     DC    X'01',C'BCOMML  ',AL1(58),X'00C2' BROADCAST COMML                
*****    DC    X'01',C'LCOMML  ',AL1(59),X'00C2' LEGAL COMML                    
GNSTUDIO EQU   *+1                                                              
         DC    X'01',C'STUDIO  ',AL1(56),X'00DF'                                
SPTPROD  EQU   *+1                                                              
         DC    X'01',C'PRODUCT ',AL1(54),X'00CD'                                
         DC    X'01',C'TALENT  ',AL1(55),X'00CD'                                
         DC    X'01',C'PATTERN ',AL1(03),X'00C2'                                
         DC    X'01',C'BPAT    ',AL1(03),X'00C2'                                
         DC    X'01',C'INST    ',AL1(06),X'00C4'                                
         DC    X'01',C'BINST   ',AL1(06),X'00C4'                                
         DC    X'01',C'AUTOINST',AL1(07),X'00C6'                                
         DC    X'01',C'AUTOPDF ',AL1(07),X'00C6'  FOR DDLINK                    
         DC    X'01',C'FAXLET  ',AL1(49),X'00C6'                                
         DC    X'01',C'SHIPPING',AL1(05),X'00C5'                                
         DC    X'01',C'CONTACT ',AL1(08),X'00C3'                                
         DC    X'01',C'FLIGHT  ',AL1(10),X'00C9'                                
         DC    X'01',C'STATION ',AL1(11),X'00C8'                                
         DC    X'01',C'LABELIST',AL1(36),X'00C3'                                
         DC    X'01',C'HOUSE   ',AL1(12),X'00C3'                                
         DC    X'01',C'SATEL   ',AL1(13),X'00C3'                                
         DC    X'01',C'TEXT    ',AL1(14),X'00C3'                                
         DC    X'01',C'BUYACT  ',AL1(21),X'00CA'                                
         DC    X'01',C'BCM     ',AL1(21),X'00CA' SO CAN TELL XFRCTL             
         DC    X'01',C'TAM     ',AL1(21),X'00CA' TRF ACTV MGR                   
         DC    X'01',C'TCM     ',AL1(21),X'00CA' TRF CRISIS MGR                 
         DC    X'01',C'STEXT   ',AL1(23),X'00C3'                                
         DC    X'01',C'DPEQV   ',AL1(38),X'00C3'                                
         DC    X'01',C'CACT    ',AL1(37),X'00CA'                                
         DC    X'01',C'TALCOM  ',AL1(45),X'00CA'                                
         DC    X'01',C'STRANS  ',AL1(60),X'00CA'                                
         DC    X'01',C'COVINS  ',AL1(39),X'00C6'                                
         DC    X'01',C'COMTEXT ',AL1(40),X'00C2'                                
         DC    X'01',C'COMCLASS',AL1(50),X'00C3'                                
         DC    X'01',C'PURGE   ',AL1(41),X'00DF'                                
* JWT WARNER LAMBERT                                                            
         DC    X'01',C'BOOKLET ',AL1(42),X'00C3'                                
         DC    X'01',C'PRDEQV  ',AL1(43),X'00C3'                                
* DEALER TAGS                                                                   
         DC    X'01',C'DTEXT   ',AL1(23),X'00C3'                                
         DC    X'01',C'SPOT    ',AL1(24),X'00C7'                                
* NEW TRAFFIC BUY                                                               
SPTBUY   EQU   *+1                                                              
         DC    X'01',C'TBUY    ',AL1(26),X'00CC'                                
         DC    X'01',C'MKTLIST ',AL1(27),X'00C3'                                
         DC    X'01',C'MLIST   ',AL1(57),X'00C3'                                
         DC    X'01',C'GOAL    ',AL1(44),X'00C3'                                
         DC    X'01',C'MARKET  ',AL1(51),X'00C6'                                
         DC    X'01',C'AMS     ',AL1(52),X'00C6'                                
         DC    X'01',C'AMSPDF  ',AL1(52),X'00C6'  FOR DDLINK                    
         DC    X'01',C'TEST    ',AL1(09),X'0000'                                
         DC    X'01',C'HELP    ',AL1(00),X'00CF'                                
         DC    X'01',C'ERRLIST ',AL1(99),X'0000'  ** OFFLINE ONLY **            
         EJECT                                                                  
*                                  X'02' ENTRIES ARE AVAILABLE ACTIONS          
*                                  CL8 EXPANDED ACTION NAME                     
*                                  CL1 ACTION NUMBER                            
*                                  CL1 ACTION EQUATE                            
*                                  CL1 SPARE                                    
         SPACE                                                                  
         DC    X'02',C'ADD     ',AL1(01,01,00)                                  
         DC    X'02',C'CHANGE  ',AL1(02,01,00)                                  
         DC    X'02',C'DISPLAY ',AL1(03,01,00)                                  
         DC    X'02',C'DELETE  ',AL1(04,01,00)                                  
         DC    X'02',C'SELECT  ',AL1(05,01,00)                                  
         DC    X'02',C'RECAP   ',AL1(10,13,00)                                  
         DC    X'02',C'RESTORE ',AL1(06,01,00)                                  
         DC    X'02',C'LIST    ',AL1(10,10,00)                                  
         DC    X'02',C'REPORT  ',AL1(12,12,00)                                  
         DC    X'02',C'GENERATE',AL1(14,14,00)                                  
         DC    X'02',C'TAG     ',AL1(15,15,00)                                  
ACTASSGN EQU   *+1                                                              
         DC    X'02',C'ASSIGN  ',AL1(16,16,00)                                  
         DC    X'02',C'LABEL   ',AL1(17,17,00)                                  
         DC    X'02',C'BUY     ',AL1(18,18,00)                                  
         DC    X'02',C'PURGE   ',AL1(19,19,00)                                  
         DC    X'02',C'COPY    ',AL1(20,20,00)                                  
         DC    X'02',C'MOVE    ',AL1(21,21,00)                                  
         DC    X'02',C'CANCEL  ',AL1(22,22,00)                                  
         DC    X'02',C'USAGE   ',AL1(23,23,00)                                  
         DC    X'02',C'SEED    ',AL1(24,24,00)                                  
         DC    X'02',C'TRANSFER',AL1(25,25,00)                                  
         DC    X'02',C'HELP    ',AL1(00,00,00)                                  
         EJECT                                                                  
*              DIRECTORY OF PHASES FOR SELECTED RECORD/ACTION                   
         SPACE                                                                  
*                                  X'03' ENTRIES ARE OK REC/ACT COMBOS          
*                                  CL1 RECORD NUMBER                            
*                                  CL1 ACTION EQUATE                            
*                                  CL1 PHASE NUMBER FOR SCREEN                  
*                                  CL1 PHASE NUMBER FOR EDIT                    
*                                  CL1 PHASE NUMBER FOR SPECS                   
*                                  CL1 PHASE NUMBER FOR REPORT                  
*                                  CL1 WHEN OK BITS 80=SCREEN 40=NOW            
*                                      20=SOON 10=OV 08=DDS                     
*                                  CL2 CODE FOR REPORTS                         
*                                  CL2 CODE FOR EOD HANDLING                    
         SPACE                                                                  
*                                 SC  SP  AC                                    
*                                   OV  RP                                      
         DC    X'03',AL1(02,01),X'F202000080',C'    '  COMM     MAINT           
**NOP**  DC    X'03',AL1(58,01),X'6F02000080',C'    '  LCOMM    MAINT           
**NOP**  DC    X'03',AL1(59,01),X'9F02000080',C'    '  BCOMM    MAINT           
         DC    X'03',AL1(02,10),X'E2020002F8',C'CLTL'           LIST            
         DC    X'03',AL1(02,23),X'F048004878',C'CUTU'           USAGE           
* CANADIAN COMMERCIALS BELOW                                                    
         DC    X'03',AL1(48,01),X'E432000080',C'    '  CCOMM    MAINT           
         DC    X'03',AL1(48,10),X'E2320032F8',C'CLTL' (CANADIAN)LIST            
*                                                                               
         DC    X'03',AL1(02,13),X'D2420042F8',C'CRTC'  COMML    RECAP           
         DC    X'03',AL1(03,01),X'F303000080',C'    '  PATTERN  MAINT           
         DC    X'03',AL1(03,01),X'5C03000080',C'    '  BPAT     MAINT           
         DC    X'03',AL1(03,10),X'E3230023F8',C'PLTL'           LIST            
         DC    X'03',AL1(03,13),X'D3430043F8',C'PRTC'           RECAP           
         DC    X'03',AL1(07,14),X'F704000478',C'AITA'  AUTOINST GEN             
         DC    X'03',AL1(49,14),X'7767006738',C'ATTF'  FAXLET   GEN             
         DC    X'03',AL1(56,01),X'8E57005780',C'    '  STUDIO   MAINT           
         DC    X'03',AL1(56,10),X'9E57005780',C'    '           LIST            
         DC    X'03',AL1(06,13),X'D4440044F8',C'IRTC'  INST     RECAP           
         DC    X'03',AL1(06,01),X'F3440044F8',C'    '  PAT DISP RECAP           
         DC    X'03',AL1(05,13),X'D5450045F8',C'SRTC'  SHIPPING RECAP           
         DC    X'03',AL1(05,14),X'D646004678',C'SHTO'           GEN             
         DC    X'03',AL1(05,01),X'F505000080',C'    '           MAINT           
         DC    X'03',AL1(08,01),X'F808000080',C'    '  CONTACT  MAINT           
         DC    X'03',AL1(08,10),X'E808000880',C'    '           LIST            
         DC    X'03',AL1(09,01),X'F909000080',C'    '  TEST     MAINT           
         DC    X'03',AL1(09,10),X'E9090009F8',C'TLTL'           LIST            
         DC    X'03',AL1(10,01),X'FA0A000080',C'    '  FLIGHT   MAINT           
         DC    X'03',AL1(10,10),X'EA0A000AF8',C'FLTL'           LIST            
         DC    X'03',AL1(11,01),X'FB0B000080',C'    '  STATION  MAINT           
         DC    X'03',AL1(11,10),X'EB0B000BF8',C'SLTL'           LIST            
         DC    X'03',AL1(11,17),X'DB4B004B78',C'SBTL'           LABEL           
         DC    X'03',AL1(36,01),X'B010000080',C'    '  LABELIST MAINT           
         DC    X'03',AL1(36,10),X'A0100010F8',C'LLTL'           LIST            
         DC    X'03',AL1(12,01),X'FC0C000080',C'    '  HOUSE    MAINT           
         DC    X'03',AL1(12,10),X'EC0C000CF8',C'HLTL'           LIST            
         DC    X'03',AL1(13,01),X'FD0D000080',C'    '  SATELITE MAINT           
         DC    X'03',AL1(13,10),X'ED0D000DF8',C'SATL'           LIST            
         DC    X'03',AL1(14,01),X'FE0E000080',C'    '  TEXT     MAINT           
         DC    X'03',AL1(14,10),X'EE0E000EF8',C'TLTL'           LIST            
         DC    X'03',AL1(37,10),X'DD47004738',C'CATL'  CACT     LIST            
         DC    X'03',AL1(45,10),X'E021002138',C'CATL'  TALCOM   LIST            
         DC    X'03',AL1(60,12),X'E021002138',C'STST'  STRANS   REPORT          
         DC    X'03',AL1(55,25),X'8D49004938',C'CATL'  TALCOM   TRANSFR         
         DC    X'03',AL1(54,10),X'7C3A000080',C'    '  PRODUCT  LIST            
         DC    X'03',AL1(23,01),X'B313000080',C'    '  DTEXT    MAINT           
         DC    X'03',AL1(23,10),X'A3130013F8',C'DTTL'           LIST            
         DC    X'03',AL1(21,12),X'B125000038',C'BATL'  BUYACT   REPORT          
         DC    X'03',AL1(21,01),X'B111000080',C'    '  BUYACT   DISP            
         DC    X'03',AL1(21,10),X'A1110011F8',C'BATL'           LIST            
         DC    X'03',AL1(22,01),X'6B5B000080',C'    '  TAM      DISP            
         DC    X'03',AL1(24,16),X'D040000080',C'    '  SPOT     ASSIGN          
         DC    X'03',AL1(24,14),X'DA4A004A58',C'SGTQ'  SPOT     GEN             
         DC    X'03',AL1(24,10),X'75650065F8',C'SLTL'  SPOT     LIST            
         DC    X'03',AL1(24,24),X'782A002A78',C'SETS'  SPOT     SEED            
         DC    X'03',AL1(24,15),X'B414000080',C'    '  SPOT     TAG             
*DEAD MH DC    X'03',AL1(24,13),X'A4540054F8',C'SCTC'           RECAP           
         DC    X'03',AL1(26,16),X'B616000080',C'    '  TBUY     ASSIGN          
         DC    X'03',AL1(26,18),X'E626000080',C'    '  TBUY     BUY             
         DC    X'03',AL1(26,20),X'8727000080',C'    '  TBUY     COPY            
         DC    X'03',AL1(26,21),X'8727000080',C'    '  TBUY     MOVE            
         DC    X'03',AL1(26,22),X'8727000080',C'    '  TBUY     CANCEL          
         DC    X'03',AL1(26,10),X'A6160016F8',C'TVTL'           LIST            
         DC    X'03',AL1(44,10),X'F1410041F8',C'GLTL'  GOAL     LIST            
         DC    X'03',AL1(27,01),X'B717000080',C'    '  MKTLIST  MAINT           
         DC    X'03',AL1(27,10),X'A7170017F8',C'MSTL'           LIST            
         DC    X'03',AL1(27,14),X'A793009318',C'MBTL'           GEN             
         DC    X'03',AL1(38,01),X'D833000080',C'    '  DPEQV    MAINT           
         DC    X'03',AL1(38,10),X'D7330033F8',C'MSTL'           LIST            
         DC    X'03',AL1(39,14),X'D934000078',C'CITV'  COVINS   GEN             
         DC    X'03',AL1(51,14),X'7F35000078',C'CKTK'  MARKET   GEN             
         DC    X'03',AL1(52,14),X'7E36003678',C'CJTJ'  AMS      GEN             
         DC    X'03',AL1(40,01),X'E131000080',C'    '  COMTEXT  MAINT           
         DC    X'03',AL1(40,10),X'D1310031F8',C'CTTL'           LIST            
         DC    X'03',AL1(50,01),X'7A69000080',C'    '  COM CLASSMAINT           
         DC    X'03',AL1(50,10),X'79690069F8',C'CLTL'           LIST            
         DC    X'03',AL1(41,19),X'E794009418',C'PRTP'  PURGE    PURGE           
         DC    X'03',AL1(42,10),X'7060006018',C'BKTP'  BOOKLET  LIST            
         DC    X'03',AL1(43,01),X'8464000080',C'    '  PRDEQV   MAINT           
         DC    X'03',AL1(43,10),X'74640064F8',C'BKTL'  PRDEQV   LIST            
         DC    X'03',AL1(57,01),X'4C51000080',C'    '  MLIST    MAINT           
         DC    X'03',AL1(57,10),X'4D510051F8',C'MLTL'           LIST            
         DC    X'FF'                                                            
         TITLE 'T21600 - SPOT TRAFFIC SPOOL CONTROLLER'                         
         SPACE                                                                  
*&&DO                                                                           
* TABLE OF SPOT/TRAFFIC SE NUMBERS                                              
         SPACE                                                                  
         DS    0H                                                               
SYSTBL   DC    X'4102'             1                                            
         DC    X'4203'             2                                            
         DC    X'4307'             3                                            
         DC    X'460D'             4                                            
         DC    X'4711'             5                                            
         DC    X'4815'             6                                            
         DC    X'4922'             7                                            
         DC    X'4A27'             B                                            
         DC    X'4BE2'             E                                            
         DC    X'4CF2'             F                                            
         DC    X'4DF3'             G                                            
         DC    X'4EF4'             H                                            
         DC    X'51F5'             L                                            
         DC    X'52F6'             M                                            
         DC    X'53F7'             N                                            
         DC    X'5632'             Q                                            
         DC    X'5733'             S                                            
         DC    X'5936'             U                                            
         DC    X'5AF8'             V                                            
         DC    X'5BF9'             Y                                            
         DC    X'5CFA'             0                                            
         DC    X'A2A0'             IT                                           
         DC    X'D5C2'             Z                                            
SYSTBLL  EQU   (*-SYSTBL)/2                                                     
*&&                                                                             
         DS    0H                                                               
LOCKMSG  DC    C'CLIENT LOCKED DUE TO SOON MARKING -- TRY AGAIN LATER'          
         DS    0H                                                               
RECLENMS DC    C'** ERROR ** MUST ENTER AT LEAST 3 CHARACTERS *'                
         DS    0H                                                               
STAERRMS DC    C'** ERROR ** CALL DDS - LOCAL CABLE ERROR *'                    
         DS    0H                                                               
SIGNONM1 DC    C'* ERROR * YOU MUST SIGN ON TO STRAFFIC SYSTEM *'               
         DS    0H                                                               
NOTAUTMS DC    CL34'USER NOT AUTHORIZED FOR SYSTEM'                             
         DS    0H                                                               
AGYMS    DC    C'* ERROR * CALL DDS, NO AGY REC *'                              
         DS    0H                                                               
WRITERMS DC    C'* ERROR * SYSTEM IS READ ONLY *'                               
         DS    0H                                                               
WRITERMA DC    C'* ERROR * CALL DDS, NO SYSTEM *'                               
         EJECT                                                                  
*===============================================================                
* INITIALIZE                                                                    
*===============================================================                
                                                                                
INIT     NTR1  BASE=*,LABEL=*                                                   
         L     R3,SYSPARMS                                                      
         L     R2,8(R3)            A(SYSLIST)                                   
         L     RF,4(R2)            A(CALLOV)                                    
         ST    RF,CALLOV                                                        
*                                                                               
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR THIS SYSTEM + PROG           
*                                                                               
         MVI   DMCB+7,QCLPACK      GET A(CLPACK) AND A(CLUNPK)                  
         BRAS  RE,SPCALLOV                                                      
         MVC   CLPACK,DMCB                                                      
         MVI   DMCB+7,QCLUNPK                                                   
         BRAS  RE,SPCALLOV                                                      
         MVC   CLUNPK,DMCB                                                      
*                                                                               
         MVI   DMCB+7,QSTAVAL                                                   
         BRAS  RE,SPCALLOV                                                      
         MVC   STAVAL,DMCB                                                      
         MVI   DMCB+7,QSTAPACK                                                  
         BRAS  RE,SPCALLOV                                                      
         MVC   STAPACK,DMCB                                                     
*                                                                               
         TM    CONRECH+4,X'20'     TEST RECORD CHANGED                          
         BO    INIT10              NO                                           
         TM    CONACTH+1,X'20'     TEST ACTION FIELD PROTECTED                  
         BZ    INIT10                                                           
         NI    CONACTH+1,X'FF'-X'20' UNPROTECT IT                               
         OI    CONACTH+6,X'80'       AND XMT IT                                 
*                                                                               
INIT10   L     RF,SYSPARMS                                                      
         L     RF,16(RF)                                                        
         ST    RF,ACOMFACS                                                      
*                                                                               
         XC    ERRTEXT,ERRTEXT                                                  
*                                                                               
         ICM   RF,15,CGLOBBER-COMFACSD(RF)                                      
         BZ    INITX                                                            
         XC    SVXFRSID,SVXFRSID                                                
         MVI   SVXFROV,0                                                        
         GOTO1 (RF),DMCB,=C'GETD',BLOCK,24,GLVXCTL                              
         CLI   8(R1),0                                                          
         BNE   INITX                                                            
*                                                                               
INIT12   GOTO1 (RF),(R1),=C'GETF',CONRECH,,GLVXREC                              
         GOTO1 (RF),(R1),=C'DELE'  DELETE RECORD                                
*                                                                               
         GOTO1 (RF),(R1),=C'GETF',CONACTH,,GLVXACT                              
         GOTO1 (RF),(R1),=C'DELE'  DELETE ACTION                                
                                                                                
* GET 'WHEN' FIELD IF IT'S THERE                                                
                                                                                
         GOTO1 (RF),(R1),=C'GETF',CONWHENH,7,GLVSPRNT                           
                                                                                
* TRY FOR DATA IN GLVPRKEY                                                      
                                                                                
         GOTO1 (RF),(R1),=C'GETF',CONKEYH,,GLVPRKEY                             
         CLI   8(R1),0             IF NO KEY FIELD, TRY FOR MEDIA               
         JNE   INIT20                                                           
         GOTO1 (RF),(R1),=C'DELE'  DELETE KEY                                   
         J     INITX                                                            
*                                                                               
INIT20   DS    0H                                                               
         LA    R5,CONKEY                                                        
         XC    WORK,WORK                                                        
         GOTO1 (RF),(R1),=C'GETD',WORK,1,GLVSPMD                                
         CLI   8(R1),0                                                          
         JNE   INIT22              NO MEDIA FIELD FOR NINS/GEN                  
*                                                                               
         GOTO1 (RF),(R1),=C'DELE'                                               
         MVC   CONKEY(1),WORK                                                   
         MVI   CONKEY+1,C','                                                    
         LA    R5,CONKEY+2                                                      
*                                                                               
INIT22   XC    WORK,WORK                                                        
         GOTO1 (RF),(R1),=C'GETD',WORK,3,GLVSPCLT                               
         CLI   8(R1),0                                                          
         JNE   INITX                                                            
*                                                                               
         GOTO1 (RF),(R1),=C'DELE'                                               
         MVC   0(3,R5),WORK                                                     
*                                                                               
         LA    R4,2(R5)            POINT TO LAST CHAR OF CLIENT                 
         CLI   0(R4),C' '                                                       
         BH    *+8                                                              
         BCT   R4,*-8                                                           
         LA    R0,CONKEY-1         (LESS ONE FOR ARITH)                         
         SR    R4,R0                                                            
         STC   R4,CONKEYH+5                                                     
INITX    XIT1                                                                   
         DROP  RA                                                               
         LTORG                                                                  
         EJECT                                                                  
*GRID                                                                           
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -         
***********************************************************************         
* CHECK FOR GRIDS KEYWORD                                                       
***********************************************************************         
                                                                                
CHKGRID  NTR1  BASE=*,LABEL=*                                                   
         L     RC,SVADGEND                                                      
         USING GEND,RC                                                          
         USING CONHEADH-64,RA                                                   
         LHI   R5,(PCDRIVE-T216FFD)                                             
         AR    R5,RA                                                            
         USING PCDRIVEN,R5                                                      
         TM    PCGRIDS,PCGRPCQ                                                  
         BZ    *+8                                                              
         OI    GRIDST,GRIDSON                                                   
                                                                                
         CLC   CONOTH(4),=C'GRID'                                               
         BNE   CHKGRIDX                                                         
                                                                                
CHKGR010 DS    0H                                                               
         OI    PCGRIDS,PCGRPCQ     USING GRIDS                                  
         OI    GRIDST,GRIDSON      USING GRIDS                                  
         MVI   CONOTH,C' '                                                      
         MVC   CONOTH+1(L'CONOTH-1),CONOTH  CLEAR DATA FIELD                    
         MVI   CONOTHH+FHILD,0              FIELD LENGTH                        
         MVI   CONOTHH+FHIID,0              INPUT INDICATORS                    
         OI    CONOTHH+FHOID,FHOITR         TRANSMIT FIELD                      
         XC    CONHEAD,CONHEAD                                                  
         OI    CONHEADH+FHOID,FHOITR                                            
         B     CHKGRIDX                                                         
                                                                                
CHKGRIDX CR    RB,RB                                                            
         J     XIT                                                              
         LTORG                                                                  
         EJECT                                                                  
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -         
*GRID                                                                           
***********************************************************************         
* POINT TO NETPAK UNIT FILE                                                     
***********************************************************************         
         USING GEND,RC                                                          
INITNET  DS    0H                                                               
         MVI   SYSDIR,C'U'                                                      
         MVI   SYSDIR+1,C'N'                                                    
         MVI   SYSDIR+2,C'T'                                                    
         MVC   SYSFIL(3),SYSDIR                                                 
         MVI   LKEY+1,20          DETAILS OF DIRECTORY AND KEY                  
         MVI   DATADISP+1,27                                                    
         MVI   LSTATUS+1,1                                                      
         BR    RE                                                               
         SPACE                                                                  
* POINT TO SPOT FILE                                                            
         SPACE                                                                  
INITSPT  DS    0H                                                               
         MVI   SYSDIR,C'S'                                                      
         MVI   SYSDIR+1,C'P'                                                    
         MVI   SYSDIR+2,C'T'                                                    
         MVC   SYSFIL(3),SYSDIR                                                 
         MVI   LKEY+1,13          DETAILS OF DIRECTORY AND KEY                  
         MVI   DATADISP+1,24                                                    
         MVI   LSTATUS+1,1                                                      
         BR    RE                                                               
         SPACE                                                                  
* POINT TO SPOT FILE                                                            
         SPACE                                                                  
INITTRF  DS    0H                                                               
         MVI   SYSDIR,C'T'                                                      
         MVI   SYSDIR+1,C'R'                                                    
         MVI   SYSDIR+2,C'F'                                                    
         MVC   SYSFIL(3),SYSDIR                                                 
         MVI   LKEY+1,13          DETAILS OF DIRECTORY AND KEY                  
         MVI   DATADISP+1,24                                                    
         MVI   LSTATUS+1,1                                                      
         BR    RE                                                               
         SPACE                                                                  
* POINT TO EXTENDED SPOT FILE                                                   
         SPACE                                                                  
INITXSP  DS    0H                                                               
         MVI   SYSDIR,C'X'                                                      
         MVI   SYSDIR+1,C'S'                                                    
         MVI   SYSDIR+2,C'P'                                                    
         MVC   SYSFIL(3),SYSDIR                                                 
         MVI   LKEY+1,32          DETAILS OF DIRECTORY AND KEY                  
         MVI   DATADISP+1,42                                                    
         MVI   LSTATUS+1,4                                                      
         BR    RE                                                               
         SPACE                                                                  
* POINT TO CONTROL SYSTEM GENDIR/GENFIL FILE                                    
         SPACE                                                                  
INITGEN  DS    0H                                                               
         MVI   SYSDIR,C'G'                                                      
         MVI   SYSDIR+1,C'E'                                                    
         MVI   SYSDIR+2,C'N'                                                    
         MVC   SYSFIL(3),SYSDIR                                                 
         MVI   LKEY+1,32          DETAILS OF DIRECTORY AND KEY                  
         MVI   DATADISP+1,42                                                    
         MVI   LSTATUS+1,4                                                      
         BR    RE                                                               
         DROP  RC                                                               
         SPACE                                                                  
VMEDIA   NMOD1 0,**VMED**                                                       
         SPACE                                                                  
         L     RC,SVADGEND                                                      
         USING GEND,RC                                                          
         SPACE                                                                  
         CLI   SVAMSAUT,C'Y'                                                    
         BE    *+10                                                             
         XC    SVKEY,SVKEY         CLEAR SAVED KEY AREA                         
         GOTO1 ANY                                                              
         SPACE                                                                  
         XC    SVBCLTH,SVBCLTH                                                  
         SPACE                                                                  
         CLI   8(R2),C'C'          COMBINED NOT OKAY                            
         BE    VMED40                                                           
         SPACE                                                                  
         SPACE                                                                  
         BRAS  RE,INITSPT          POINT TO SPOT FILE                           
         SPACE                                                                  
         XC    KEY,KEY             GET AGENCY RECORD                            
         LA    R4,KEY                                                           
         USING AGYKEY,R4                                                        
         MVI   AGYKTYPE,X'06'                                                   
         MVC   AGYKAGY,AGENCY                                                   
         DROP  R4                                                               
*                                                                               
         MVI   RDUPDATE,C'N'       NOT READ FOR UPDATE                          
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE     DID WE GET IT                                
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R6,AIO1                                                          
         ST    R6,AIO                                                           
*                                                                               
         MVI   RDUPDATE,C'N'       NOT READ FOR UPDATE                          
         GOTO1 GETREC                                                           
*                                                                               
         MVI   ELCODE,X'02'                                                     
         BRAS  RE,GETEL                                                         
         B     *+8                                                              
         SPACE                                                                  
VMED20   BRAS  RE,NEXTEL                                                        
         BNE   VMED40                                                           
         CLC   2(1,R6),8(R2)                                                    
         BNE   VMED20                                                           
         MVC   QMED,8(R2)          SAVE INPUT MEDIA CODE                        
         MVC   BAGYMD,3(R6)        DIG OUT AGENCY/MEDIA                         
         MVC   MEDNM,4(R6)         MEDIA NAME                                   
         MVC   MEDCAPT,14(R6)      AND CAPTION                                  
         SPACE                                                                  
         L     RA,ATWA                                                          
         USING T216FFD,RA                                                       
         CLI   OFFLINE,C'Y'                                                     
         BE    VMED30                                                           
         CLI   RECNUM,X'36'                                                     
         BE    VMEDXIT                                                          
VMED30   DS    0H                                                               
         CLI   SPOTNETF,C'N'       IS THIS NETWORK                              
         BE    *+8                                                              
         BRAS  RE,INITTRF         CHANGE TO TRAFFIC FILE                        
         SPACE                                                                  
VMEDXIT  XIT1                                                                   
         SPACE                                                                  
VMED40   MVI   ERROR,INVMED                                                     
         GOTO1 ERREX                                                            
         DROP  RB,RC                                                            
         EJECT                                                                  
*        RTN TO FIND PRODUCT IN NEW CLIST                                       
         SPACE                                                                  
*NDPROD  NMOD1 0,**FNDP**                                                       
         EJECT                                                                  
* FIND ALL PRODUCTS THAT APPLY TO THIS BRAND LEVEL SECURITY CLIENT              
* AND RE-BUILD CLIENT PRODUCT LIST                                              
         SPACE                                                                  
BCPRLST  NMOD1 0,**BCPR**                                                       
         SPACE                                                                  
         L     RC,SVADGEND                                                      
         USING GEND,RC                                                          
         SPACE                                                                  
         L     RA,ATWA                                                          
         USING T216FFD,RA                                                       
         SPACE                                                                  
         NI    SECFLAG,X'FF'-NEPRDOFF INIT PRODUCT OFFICES NOT EQUAL            
         MVI   ERROR,SECLOCK       PRESET ERROR MESSAGE                         
         SPACE                                                                  
         LA    R0,NCLSTSIZ         MAX PRODS                                    
         LHI   R2,SVNCLIST-T216FFD TABLE OF CLIENT PROD CODES                   
         A     R2,ATWA                                                          
         LR    R3,R2               CURRENT PRODUCT                              
         SPACE                                                                  
         SPACE                                                                  
         BRAS  RE,INITSPT          POINT TO SPOT FILE                           
         SPACE                                                                  
BCPR10   XC    KEY,KEY                                                          
         MVC   KEY+1(1),BAGYMD                                                  
         MVC   KEY+2(2),BCLT                                                    
         MVC   KEY+4(3),0(R3)                                                   
*                                                                               
         MVI   RDUPDATE,C'N'       NOT READ FOR UPDATE                          
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                 NO PRODUCT RECORD ?                         
         SPACE                                                                  
         MVC   AIO,AIO1                                                         
         MVI   RDUPDATE,C'N'       NOT READ FOR UPDATE                          
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO                                                           
         USING PRDHDRD,R6                                                       
         L     RA,ATWA                                                          
         USING T216FFD,RA                                                       
*                                                                               
         XC    DMCB(8),DMCB                                                     
         MVC   DMCB+4(4),=X'D9000A38'  GET OFFICER ADDRESS                      
         GOTO1 CALLOV,DMCB                                                      
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         XC    WORKSEC,WORKSEC                                                  
         LA    R1,WORKSEC                                                       
         USING OFFICED,R1                                                       
         MVI   OFCSYS,C'S'         SYSTEM ID                                    
         MVC   OFCAUTH,T216FFD+6   ID AUTH VALUE                                
         MVC   OFCAGY,AGENCY                                                    
         MVC   OFCOFC,PTRAFOFC     PRODUCT OFFICE NUMBER                        
         MVC   OFCCLT,QCLT                                                      
         OC    OFCCLT,SPACES                                                    
         MVC   OFCSAGMD,BAGYMD                                                  
         MVC   OFCLMT(4),T216FFD+6                                              
         MVC   OFCSECD,ASECBLK                                                  
         DROP  R1,RA                                                            
*                                                                               
         L     RF,DMCB                                                          
         GOTO1 (RF),DMCB,(C'N',WORKSEC),ACOMFACS                                
         CLI   0(R1),0                                                          
         BNE   BCPR30              NO MATCH ON PRD OFFICE NUMBER                
*                                                                               
         CLI   SVPRDOFF,0          FIRST TIME                                   
         BE    BCPR20                                                           
*                                                                               
         CLC   SVPRDOFF,PTRAFOFC   BOTH BRANDS SAME OFFICE                      
         BE    BCPR25                                                           
         OI    SECFLAG,NEPRDOFF    PRODUCT OFFICES NOT EQUAL                    
*                                                                               
BCPR20   MVC   SVPRDOFF,PTRAFOFC   PROD OFFICE (BRAND LEVEL SEC)                
*                                                                               
BCPR25   CR    R2,R3               POINTING TO SAME ENTRY                       
         BE    BCPR28               YES                                         
*                                                                               
         MVC   0(4,R2),0(R3)       MOVE PROD TO NEXT AVAILABLE SLOT             
*                                                                               
BCPR28   LA    R2,4(R2)            BUMP TO NEXT PRD ENTRY                       
*                                                                               
BCPR30   LA    R3,4(R3)            BUMP TO NEXT PRD (CURRENT PRD)               
         CLI   0(R3),C' '          AT END OF TABLE?                             
         BNH   *+8                 YES                                          
         BCT   R0,BCPR10                                                        
*                                                                               
         SR    R3,R2               GET LENGTH                                   
         LTR   R3,R3                                                            
         BZ    BCPR50                                                           
*                                                                               
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R2,RE               FILL END OF LIST WITH NULLS                  
*                                                                               
* SAVE TWA2 WITH PRODUCT LIST                                                   
*                                                                               
BCPR50   CLI   OFFLINE,C'Y'        CHECK ONLINE                                 
         BE    BCPR60                                                           
         SPACE                                                                  
BCPR60   LHI   R2,SVNCLIST-T216FFD TABLE OF CLIENT PROD CODES                   
         A     R2,ATWA                                                          
*                                                                               
         OC    0(4,R2),0(R2)       ANY PRODUCTS                                 
         BZ    BCPR70               NO                                          
*                                                                               
         MVI   ERROR,0                                                          
*                                                                               
* RESTORE CLIENT RECORD                                                         
*                                                                               
BCPR70   DS    0H                                                               
         XC    KEY,KEY                                                          
         MVC   KEY+1(1),BAGYMD                                                  
         MVC   KEY+2(2),BCLT                                                    
*                                                                               
         MVI   RDUPDATE,C'N'       NOT READ FOR UPDATE                          
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                BETTER BE THERE                              
         SPACE                                                                  
         L     R6,AIO1                                                          
         ST    R6,AIO                                                           
*                                                                               
         MVI   RDUPDATE,C'N'       NOT READ FOR UPDATE                          
         GOTO1 GETREC                                                           
*                                                                               
*NOP     STH   R4,LKEY             RESTORE                                      
*                                                                               
         XIT1                                                                   
*                                                                               
         DROP  R6,R7,RB                                                         
         EJECT                                                                  
VALSLN   NMOD1 0,**VSLN**                                                       
         SPACE                                                                  
         L     RC,SVADGEND                                                      
         USING GEND,RC                                                          
         SPACE                                                                  
         MVI   ERROR,0                                                          
         CLC   =C'T/B',0(R1)       IS THIS FOR TRI-BACK                         
         BNE   VSLN10                                                           
         MVI   ACTUAL+1,X'FF'                                                   
         B     VSLN20              YES, NO NEED TO VALIDATE NUMBER              
         SPACE                                                                  
VSLN10   GOTO1 VALINUM             NOTE-EROPT SET OFF IN GENCON IF ERR          
         SPACE                                                                  
         CLI   ERROR,0             WAS ERROR FOUND                              
         MVI   ERROR,BADSLN                                                     
         BNE   VSLNX                YES                                         
         MVC   WORK(1),ACTUAL                                                   
         MVC   WORK+4(1),ACTUAL    MOVE HERE FOR EDIT COMPATABILITY             
         SPACE                                                                  
VSLN20   BRAS  RE,VALPRD                                                        
VSLNX    XIT1                                                                   
         SPACE 3                                                                
         DROP  RB,RC                                                            
         EJECT                                                                  
*********************************************************************           
* VALIDATE PRD (-SLN)   - ON EXIT WORK(3)   = EBCDIC PRODUCT                    
*                                 WORK+3(1) = PRODUCT CODE                      
*                                 WORK+4(1) = SPOT LENGTH (IF ENTERED)          
*                                                                               
VALPRD   NMOD1 0,**VPRD**                                                       
         SPACE                                                                  
         L     RC,SVADGEND                                                      
         USING GEND,RC                                                          
         SPACE                                                                  
         CLI   ERROR,BADSLN        PRE-SET TO BAD LEN                           
         BE    VPRD20              YES, JUST VALIDATE LEN                       
         SPACE                                                                  
         XC    BLOCK(64),BLOCK                                                  
         GOTO1 SCANNER,DMCB,(R2),(2,BLOCK),C',=,-'                              
         LA    R4,BLOCK                                                         
*                                                                               
         MVI   ERROR,INVPROD                                                    
         CLC   =C'AAA',12(R4)                                                   
         BE    VPRD26                                                           
         CLI   0(R4),2                                                          
         BL    VPRD26                                                           
         CLI   0(R4),3                                                          
         BH    VPRD26                                                           
         SPACE                                                                  
VPRD04   DS    0H                                                               
         XC    KEY,KEY                                                          
         MVC   KEY+1(1),BAGYMD                                                  
         MVC   KEY+2(2),BCLT                                                    
         MVC   KEY+4(3),12(R4)                                                  
         MVC   FILENAME,=C'SPTDIR  '                                            
*                                                                               
         MVI   RDUPDATE,C'N'       NOT READ FOR UPDATE                          
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   VPRD26                                                           
         MVC   FILENAME,=C'SPTFIL  '                                            
         MVC   AIO,AIO1                                                         
         MVI   RDUPDATE,C'N'       NOT READ FOR UPDATE                          
         GOTO1 GETREC                                                           
         XC    FILENAME,FILENAME                                                
*                                                                               
         L     R6,AIO                                                           
         USING PRDHDRD,R6                                                       
*                                                                               
         TM    POPT1,POPT1_THTR    THEATRICAL                                   
         BZ    *+8                                                              
         OI    SECFLAG,PRDTHTR                                                  
*                                                                               
         TM    SECFLAG,BLSSW       BRAND LEVEL SECURITY                         
         BZ    VPRD09               NO                                          
*                                                                               
         MVI   ERROR,SECLOCK       PRESET ERROR MESSAGE                         
*                                                                               
         MVC   SVPRDOFF,PTRAFOFC   PROD OFFICE (BRAND LEVEL SEC)                
*                                                                               
         L     RA,ATWA                                                          
         USING T216FFD,RA                                                       
*                                                                               
         OC    T216FFD+4(4),T216FFD+4                                           
         BZ    VPRD07              OPEN ACCESS                                  
*                                                                               
         XC    DMCB(8),DMCB                                                     
         MVC   DMCB+4(4),=X'D9000A38'  GET OFFICER ADDRESS                      
         GOTO1 CALLOV,DMCB                                                      
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         XC    WORKSEC,WORKSEC                                                  
         LA    R1,WORKSEC                                                       
         USING OFFICED,R1                                                       
         MVI   OFCSYS,C'S'         SYSTEM ID                                    
         MVC   OFCAUTH,T216FFD+6   ID AUTH VALUE                                
         MVC   OFCAGY,AGENCY                                                    
         MVC   OFCOFC,SVPRDOFF                                                  
         MVC   OFCCLT,QCLT                                                      
         OC    OFCCLT,SPACES                                                    
         MVC   OFCSAGMD,BAGYMD                                                  
         MVC   OFCLMT(4),T216FFD+6                                              
         MVC   OFCSECD,ASECBLK                                                  
         DROP  R1,RA                                                            
*                                                                               
         L     RF,DMCB                                                          
         GOTO1 (RF),DMCB,(C'N',WORKSEC),ACOMFACS                                
         CLI   0(R1),0                                                          
         BNE   VPRD26              ERROR                                        
         B     VPRD09                                                           
*                                                                               
VPRD07   CLC   SVCLTOFF,SVPRDOFF   CLT OFFICE TO PROD OFFICE                    
         BNE   VPRD26              GIVE ERROR                                   
*                                                                               
VPRD09   MVI   ERROR,0                                                          
*                                                                               
VPRD10   MVC   WORK(3),12(R4)      RETURN EBCDIC PRD CODE                       
         MVC   WORK+3(1),PCODE+1      AND BINARY PRD CODE                       
         MVC   WORK+5(20),PNAME       AND PRODUCT NAME                          
         DROP  R6                                                               
         SPACE                                                                  
         XC    FILENAME,FILENAME                                                
         EJECT                                                                  
* VALIDATE SPOT LENGTH IF INPUT *                                               
         SPACE                                                                  
         MVI   WORK+4,0                                                         
         CLI   1(R4),0             TEST INPUT                                   
         BE    VPRDX                                                            
         MVC   WORK+4(1),11(R4)    RETURN SLN IN BINARY                         
         MVI   ERROR,BADSLN                                                     
         CLI   1(R4),3                                                          
         BH    TRAPERR2                                                         
         TM    3(R4),X'80'         TEST NUMERIC                                 
         BZ    TRAPERR2                                                         
                                                                                
VPRD20   CLI   SPOTNETF,C'N'       THIS NETWORK                                 
         BNE   VPRD40                                                           
         SPACE                                                                  
                                                                                
         CLI   WORK+4,0                                                         
         BNH   VPRD26                                                           
         CLI   WORK+4,254                                                       
         BH    VPRD26                                                           
         B     VPRD30                                                           
*                                                                               
VPRD26   CLI   ERROPT,C'Y'         DOES USER WANT CONTROL ON ERROR              
         BNE   TRAPERR2            NO                                           
         B     VPRDX                                                            
                                                                                
VPRD30   MVI   ERROR,0             CLEAR ERROR CODE FOR USER                    
                                                                                
VPRDX    DS    0H                                                               
         XC    FILENAME,FILENAME                                                
         XIT1                                                                   
         SPACE                                                                  
*=========================================================                      
* USE NEW CORE RESIDENT LENGTH TABLE (SPSLENTAB)                                
*=========================================================                      
         SPACE                                                                  
VPRD40   DS    0H                                                               
         XC    DMCB(8),DMCB                                                     
         MVC   DMCB+4(4),=X'D9000A57'  GET A(SLNTAB)                            
         GOTO1 CALLOV,DMCB                                                      
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         SPACE                                                                  
         L     R1,DMCB             POINT TO START OF PHASE                      
         LH    RE,0(R1)            ENTRY LENGTH                                 
         L     RF,2(R1)            EOT DSPL                                     
         AR    RF,R1               POINT TO END OF TABLE                        
         AHI   R1,6                POINT TO FIRST ENTRY                         
         SPACE                                                                  
         SR    R0,R0                                                            
         LA    R0,C'T'                                                          
         CLI   QMED,C'T'                                                        
         BE    VPRD44                                                           
         CLI   QMED,C'N'                                                        
         BE    VPRD44                                                           
         CLI   QMED,C'C'                                                        
         BE    VPRD44                                                           
         SPACE                                                                  
         LA    R0,C'R'                                                          
         CLI   QMED,C'R'                                                        
         BE    VPRD44                                                           
         CLI   QMED,C'X'                                                        
         BE    VPRD44                                                           
         DC    H'0'                                                             
         SPACE                                                                  
VPRD44   CLC   =C'00',0(R1)        TEST DEFAULT TABLE                           
         BE    VPRD46                                                           
         CLC   AGENCY,0(R1)        ELSE MATCH AGY                               
         BNE   *+12                                                             
VPRD46   CLM   R0,1,2(R1)          AND MEDIA                                    
         BE    VPRD50                                                           
*                                                                               
         BXLE  R1,RE,VPRD44        NEXT ENTRY                                   
         DC    H'0'                                                             
*                                                                               
VPRD50   AHI   R1,4                POINT BEYOND HEADER                          
         SR    RE,RE                                                            
         IC    RE,WORK+4           GET SLN                                      
         AR    RE,RE               X 2                                          
         AR    RE,R1               POINT TO ENTRY                               
         CLI   1(RE),0             TEST SLN VALID                               
         BE    VPRD26              ZERO IS INVALID                              
         SPACE                                                                  
***      MVC   WORK+4(1),1(RE)     USE THE LENGTH IT TELLS                      
         B     VPRD30                                                           
         SPACE                                                                  
TRAPERR2 CLI   ERROPT,C'Y'         DOES USER WANT CONTROL ON ERROR              
         BE    VPRDX                                                            
         GOTO1 ERREX                                                            
         SPACE                                                                  
* SPOT SPOT LENGTH TABLE *                                                      
         SPACE                                                                  
*LNTABS  DC    AL1(5,10,15,20,30,40,45,50,60,75,90,105,120,150)                 
*        DC    5AL1(0)                                                          
SLNTABS  DS    0X                                                               
       ++INCLUDE SPSLNTAB                                                       
         DC    5X'00'    INSURANCE                                              
SLNTABSL EQU   *-SLNTABS                                                        
         SPACE                                                                  
* NETWORK UNIT LENGTH TABLE *                                                   
         SPACE                                                                  
SLNTABN  DC   AL1(3,5,7,8,10,15,20,25,28,30,33,35,37,40,42,45,50,57,60)         
         DC   AL1(70,75,80,90,105,120,150,180,230,240)                          
SLNTABNL EQU   *-SLNTABN                                                        
         EJECT                                                                  
*================================================================               
* VALIDATE PRODUCT GROUP *                                                      
* ON ENTRY R4 POINTS TO SCANNER TABLE ENTRY                                     
* ON EXIT, KEY CONTINS KEY OF FIRST PRODUCT GROUP                               
*================================================================               
                                                                                
VALPGR   NMOD1 0,**VPGR**                                                       
*                                                                               
         L     RC,SVADGEND                                                      
         USING GEND,RC                                                          
*                                                                               
         OC    0(2,R4),0(R4)       TEST CALL JUST FOR PGRP LEN                  
         BE    VALPGR2                                                          
*                                                                               
         CLI   1(R4),1             MUST BE SECOND ENTRY                         
         BL    PGRPERR              NO                                          
         CLI   1(R4),3                                                          
         BH    PGRPERR                                                          
*                                                                               
VALPGR2  XC    KEY,KEY             READ PGRDEF RECORD FOR LENGTHS               
         MVC   KEY(2),=X'0D01'                                                  
         MVC   KEY+2(3),BAGYMD     A-M/CLT                                      
         MVC   KEY+5(1),SVTN2PR1                                                
         MVI   RDUPDATE,C'N'                                                    
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'SPTDIR',KEY,KEY                       
*                                                                               
         CLC   KEY(6),KEYSAVE                                                   
         BNE   PGRPERR                                                          
*                                                                               
         MVC   AIO,AIO1                                                         
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 DATAMGR,DMCB,=C'GETREC',=C'SPTFILE',KEY+14,AIO,DMWORK            
*                                                                               
         L     R6,AIO                                                           
         LA    R6,24(R6)           POINT TO 01 ELEMENT                          
         CLI   0(R6),1                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING PRGEL01,R6                                                       
         LLC   R0,PRGBK1LN                                                      
         LLC   RE,PRGBK2LN                                                      
         AR    R0,RE                                                            
         LLC   RE,PRGBK3LN                                                      
         AR    R0,RE                                                            
         STC   R0,PGRLEN           SAVE TOTAL NUMBER OF DIGITS                  
         DROP  R6                                                               
*                                                                               
         OC    0(2,R4),0(R4)       TEST CALL JUST FOR LEN                       
         BZ    VPGRX                                                            
*                                                                               
         CLC   1(1,R4),PGRLEN      HAVE RIGHT NUMBER OF DIGITS?                 
         BNE   PGRPERR                                                          
*                                                                               
         LLC   R0,1(R4)                                                         
         LA    R1,22(R4)                                                        
         LA    RE,DUB                                                           
         MVC   DUB(4),=C'0000'                                                  
*                                                                               
VPGR10   CLI   0(R1),C'0'                                                       
         BL    PGRPERR                                                          
         CLI   0(R1),C'9'                                                       
         BH    PGRPERR                                                          
         MVC   0(1,RE),0(R1)                                                    
         LA    R1,1(,R1)                                                        
         LA    RE,1(,RE)                                                        
         BCT   R0,VPGR10                                                        
*                                                                               
         PACK  WORK(3),DUB(5)                                                   
         MVC   SVPGRP,WORK                                                      
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D81'                                                  
         MVC   KEY+2(3),BAGYMD                                                  
         MVC   KEY+5(1),SVTN2PRO+00                                             
         MVC   KEY+6(2),SVPGRP                                                  
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(8),KEYSAVE      TEST GROUP FOUND                             
         BNE   BDPGRPER                                                         
*                                                                               
VPGRX    XIT1                                                                   
         EJECT                                                                  
PGRPERR  MVC   GERROR,=Y(PGRPNG)                                                
         GOTO1 VTRAERR                                                          
*                                                                               
BDPGRPER MVC   GERROR,=Y(PRDGRPNF)                                              
         XC    ELEM,ELEM                                                        
         LA    R1,ELEM                                                          
         STCM  R1,7,GASUBST        A(SUBST TEXT)                                
         MVI   ELEM,7                                                           
         MVC   ELEM+1(1),SVTN2PRO+00                                            
*                                                                               
         UNPK  WORK+3(5),WORK(3)                                                
         SR    RE,RE                                                            
         IC    RE,PGRLEN           GET NUMBER OF DIGITS IN GROUP                
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   ELEM+2(0),WORK+3                                                 
         GOTO1 VTRAERR                                                          
         LTORG                                                                  
         EJECT                                                                  
*=====================================================================          
* VALIDATE DAYPART                                                              
*                                                                               
* AT ENTRY R2 CONTAINS A(DAYPART FIELD ON SCREEN)                               
*                                                                               
* IF PARAM1 ENTERED, BYTE 0 =  X'01' (DAYPART EQUATE ENTERED)                   
*                    BYTES 1-3 = A(ONE BYTE DAYPARTE EQUATE)                    
*                                                                               
* ON RETURN                                                                     
* QDPT  = 1 OR 2 BYTE DAYPART CODE                                              
* QDPT2 = 2 BYTE ALPHA DAYPART CODE                                             
*=====================================================================          
         SPACE                                                                  
         USING T216FFD,RA                                                       
VALDPT   NMOD1 0,**VDPT**                                                       
         SPACE                                                                  
         L     RC,SVADGEND                                                      
         USING GEND,RC                                                          
         SPACE                                                                  
         MVC   GERROR,=AL2(INVENTRY)                                            
         MVI   QDPT,0                                                           
         XC    QDPT2,QDPT2                                                      
*NOP     XC    DPTNAME,DPTNAME                                                  
*                                                                               
         CLI   0(R1),X'01'         DAYPART EQUATE ENTERED?                      
         BE    VDPT200                                                          
*                                                                               
         LA    R3,DPTTAB           CHECK IN DAYPART TABLE                       
         OC    8(2,R2),=C'  '                                                   
*                                                                               
VDPT005  DS    0H                                                               
         CLI   0(R3),X'FF'         NOT IN TABLE - CHECK RECORDS                 
         BE    VDPT010                                                          
         CLC   8(2,R2),0(R3)                                                    
         BE    DPTOK                                                            
*                                                                               
         LA    R3,10(R3)                                                        
         B     VDPT005                                                          
*                                                                               
DPTOK    DS    0H                                                               
         MVC   QDPT,0(R3)          PASS BACK DAYPART EQUATE                     
         MVC   QDPT2,0(R3)         PASS BACK DAYPART                            
*NOP     MVC   DPTNAME,2(R3)       PASS BACK DESCRIPTION                        
         B     VDPTX                                                            
*                                                                               
VDPT010  DS    0H                  CHECK DAYPART RECORDS                        
         LA    R4,KEY                                                           
         USING NDPTHDR,R4                                                       
         XC    KEY,KEY                                                          
         MVC   NDPTKTYP,=XL2'0D07'                                              
         MVC   NDPTAGM,BAGYMD                                                   
*                                                                               
VDPT020  DS    0H                                                               
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,=CL8'DMRDHI',=C'UNTDIR  ',KEY,KEY,0                 
         B     VDPT040                                                          
*                                                                               
VDPT030  DS    0H                                                               
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,=CL8'DMRSEQ',=C'UNTDIR  ',KEY,KEY,0                 
*                                                                               
VDPT040  CLC   KEY(5),KEYSAVE      AGY LEVEL/CLIENT LEVEL                       
         BNE   VDPT100             NO                                           
*                                                                               
         OC    8(2,R2),=C'  '                                                   
         CLC   NDPTDPTA,8(R2)      MATCH ON CODE?                               
         BNE   VDPT030                                                          
*                                                                               
         MVC   QDPT,NDPTDPTE       PASS BACK DAYPART EQUATE                     
         MVC   QDPT2,NDPTDPTA      PASS BACK DAYPART                            
*NOP     MVC   DPTNAME,NDPTDES     PASS BACK DESCRIPTION                        
         B     VDPTX                                                            
*                                                                               
* CHECK KEY FOR AGENCY OR CLIENT LEVEL CHECK                                    
* IF AGENCY LEVEL-RESET KEY-MOVE CLIENT CODE IN-RESTART SEARCH                  
* IF CLIENT LEVEL-EXIT ROUTINE-DPT WAS INVALID                                  
*                                                                               
VDPT100  OC    KEYSAVE+3(2),KEYSAVE+3                                           
         BNZ   VDPTERR                                                          
*                                                                               
         OC    BCLT,BCLT           CHECK CLIENT LEVEL?                          
         BZ    VDPTERR                                                          
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(5),KEYSAVE                                                   
         MVC   KEY+3(2),BCLT                                                    
         B     VDPT020                                                          
*                                                                               
VDPT200  DS    0H                  DAYPART EQUATE ENTERED                       
         LA    R3,DPTTAB           CHECK IN DAYPART TABLE                       
         L     R5,DMCB             A(1 BYTE DAYPART EQUATE FIELD)               
         MVC   WORK(1),0(R5)                                                    
*                                                                               
VDPT205  DS    0H                                                               
         CLI   0(R3),X'FF'         NOT IN TABLE - CHECK RECORDS                 
         BE    VDPT210                                                          
         CLC   WORK(1),0(R3)                                                    
         BE    DPTEOK                                                           
*                                                                               
         LA    R3,10(R3)                                                        
         B     VDPT205                                                          
*                                                                               
DPTEOK   DS    0H                                                               
         MVC   QDPT,0(R3)          PASS BACK DAYPART EQUATE                     
         MVC   QDPT2,0(R3)         PASS BACK DAYPART                            
*NOP     MVC   DPTNAME,2(R3)       PASS BACK DESCRIPTION                        
         B     VDPTX                                                            
*                                                                               
VDPT210  DS    0H                  CHECK DAYPART RECORDS                        
         L     R5,DMCB             A(1 BYTE DAYPART EQUATE FIELD)               
         MVC   WORK(1),0(R5)                                                    
*                                                                               
         LA    R4,KEY                                                           
         XC    KEY,KEY                                                          
         MVC   NDPTKTYP,=XL2'0D07'                                              
         MVC   NDPTAGM,BAGYMD                                                   
*                                                                               
         CLI   WORK,127            CHECK CLIENT LEVEL DAYPART                   
         BH    *+10                                                             
         MVC   NDPTCLT,BCLT                                                     
*                                                                               
         MVC   NDPTDPTE,WORK       DAYPART EQUATE                               
*                                                                               
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,=CL8'DMRDHI',=C'UNTDIR  ',KEY,KEY,0                 
         CLC   KEY(6),KEYSAVE                                                   
         BNE   VDPTERR                                                          
*NOP     BNE   VDPTX                                                            
*                                                                               
         MVC   QDPT,NDPTDPTE       PASS BACK DAYPART EQUATE                     
         MVC   QDPT2,NDPTDPTA      PASS BACK DAYPART                            
*NOP     MVC   DPTNAME,NDPTDES     PASS BACK DESCRIPTION                        
*                                                                               
VDPTX    DS    0H                                                               
         XC    GERROR,GERROR                                                    
VDPTXX   XIT1                                                                   
*                                                                               
VDPTERR  CLI   ERROPT,C'Y'         RETURN ON ERROR?                             
         BE    VDPTXX                                                           
         SPACE                                                                  
         OI    GENSTAT2,USGETTXT   FLAGS ERREX TO CALL GETTXT                   
         LA    RF,GETTXTCB                                                      
         USING GETTXTD,RF                                                       
         MVC   GTINDX,GINDEX       MESSAGE INDEX                                
         MVC   GTMSGNO,GERROR      MESSAGE NUMBER                               
         MVC   GTMTYP,GMSGTYPE     MESSAGE TYPE                                 
         MVI   GTMSYS,X'0D'        ALL MESSGES FROM STRAFFIC (X'0D')            
*                                                                               
         CLC   GERROR,=H'60'       MESSAGE NUMBER <= 60 ?                       
         BH    *+8                 NO -- USE CONTROL SYSTEM MESSAGES            
         MVI   GTMSYS,X'FF'        USE GENERAL SYSTEM                           
*                                                                               
         XC    DUB,DUB                                                          
         LA    R1,DUB                                                           
         STCM  R1,7,GASUBST                                                     
         MVI   DUB,8               L'SUBST TEXT + 1                             
         MVC   DUB+1(7),=C'DAYPART'                                             
*                                                                               
         MVC   GTASUBST,GASUBST    A(TABLE OF SUBST TEXT(S))                    
*                                                                               
         DROP  RF                                                               
TRAPERR1 GOTO1 ERREX                                                            
*                                                                               
DPTTAB   DS    0H                                                               
         DC    CL2'D ',CL8'DAYTIME'                                             
         DC    CL2'F ',CL8'FRINGE'                                              
         DC    CL2'P ',CL8'PRIME'                                               
         DC    CL2'K ',CL8'KIDS'                                                
         DC    CL2'S ',CL8'SPORTS'                                              
         DC    CL2'N ',CL8'NEWS'                                                
         DC    CL2'L ',CL8'LATE'                                                
         DC    CL2'Y ',CL8'YOUTH'                                               
         DC    CL2'E ',CL8'EARLY'                                               
         DC    CL2'T ',CL8'TEENS'                                               
         DC    CL2'C ',CL8'CABLE'                                               
         DC    CL2'X ',CL8'SYND'                                                
         DC    CL2'I ',CL8'SPECIAL'                                             
         DC    CL2'O ',CL8'OLYMPICS'                                            
         DC    CL2'W ',CL8'WEEKEND'                                             
         DC    CL2'U ',CL8'UNWIRED'                                             
         DC    CL2'V ',CL8'OVERNITE'                                            
         DC    CL2'W ',CL8'WKNDPM'                                              
         DC    CL2'M ',CL8'WKNDAM'                                              
         DC    CL2'H ',CL8'OTHER'                                               
         DC    CL2'B ',CL8'CBLSPORT'                                            
         DC    CL2'R ',CL8'RADIO'                                               
         DC    CL2'J ',CL8'PROMO-ID'                                            
         DC    CL2'A ',CL8'ACCESS'                                              
         DC    CL2'Q ',CL8'INTRACTV'                                            
         DC    XL1'FF',CL8' '      END OF TABLE                                 
         EJECT                                                                  
         DROP  R4,RB,RC                                                         
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
**********************************************************************          
* ISSUE SOX ERROR MESSAGES *                                                    
         SPACE                                                                  
         USING T216FFD,RA                                                       
SOXERR   NMOD1 0,**+SOX**                                                       
         SPACE                                                                  
         L     RC,SVADGEND                                                      
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM                       
         SPACE                                                                  
         LA    R2,CONACT                                                        
         XC    CONHEAD,CONHEAD                                                  
         SPACE                                                                  
         TM    SOXSW,XIROSYS       READ ONLY SYSTEM?                            
         BZ    *+14                                                             
         MVC   CONHEAD(L'SOXRDON),SOXRDON                                       
         B     SOXERX                                                           
         SPACE                                                                  
         TM    SOXSW,XIROMODE      READ ONLY MODE?                              
         BZ    *+14                                                             
         MVC   CONHEAD(L'SOXRDMD),SOXRDMD                                       
         B     SOXERX                                                           
         SPACE                                                                  
         TM    SOXSW,XIWRONGF      WRONG ADV?                                   
         BZ    *+14                                                             
         MVC   CONHEAD(L'SOXWRAD),SOXWRAD                                       
         B     SOXERX                                                           
         DC    H'0'                                                             
SOXERX   DS    0H                                                               
         GOTO1 ERREX2                                                           
         DROP  RB,RC                                                            
SOXRDON  DC    C'* THIS IS A READ ONLY SYSTEM - NO UPDATES *'                   
SOXRDMD  DC    C'* YOU ARE IN READ ONLY MODE - NO UPDATES *'                    
SOXWRAD  DC    C'* YOU ARE SIGNED ON TO THE WRONG ADV *'                        
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
**********************************************************************          
* INITIALIZE SYSTEM DEPENDENT VALUES *                                          
         SPACE                                                                  
         USING T216FFD,RA                                                       
SYSINIT  NMOD1 0,**+SYS**                                                       
         L     RC,SVADGEND                                                      
         USING GEND,RC                                                          
*                                                                               
         L     R1,=V(DUMMY)        RELOCATE SYSTEM VTYPES                       
         A     R1,SPTR00RR                                                      
         ST    R1,VDUMMY                                                        
*                                                                               
         MVI   SPACES,C' '                                                      
         MVC   SPACES+1(131),SPACES                                             
*                                                                               
         L     R2,=A(VCOMMON)      SET UP COMMON ENTRIES                        
         A     R2,SPTR00RR                                                      
         SR    R3,R3                                                            
         LA    R4,SYSCOMM                                                       
         LA    R5,VCOUNT                                                        
*                                                                               
SYS02    ST    R2,0(R4)                                                         
         STC   R3,0(R4)                                                         
         LA    R3,4(R3)                                                         
         LA    R4,4(R4)                                                         
         BCT   R5,SYS02                                                         
*                                                                               
* SET SYSTEM DEPENDENT VALUES *                                                 
*                                                                               
         MVI   SYSTEM,C'F'         TRAFFIC                                      
         MVI   MAXIOS,3            USES 3 I/O AREAS                             
         MVC   SIZEIO,=F'6000'     EACH I/O IS 6000 BYTES                       
         MVC   SYSDUMMY,VDUMMY     END OF SYSTEM BASE                           
         MVC   GETUSER,GETAGY      ROUTINE TO GET USER NAME AND ADDRESS         
*                                                                               
         BRAS  RE,INITSPT          POINT TO SPOT FILE                           
*                                                                               
         MVC   SYSDIR+3(3),=C'DIR'                                              
         MVC   SYSFIL+3(3),=C'FIL'                                              
*                                                                               
         MVI   GETMSYS,5           USES GETMSG FOR SYSTEM 5                     
         MVC   LWORK,=F'44000'     WE TOOK 44,000 BYTES IN NMOD                 
         MVC   RCPROG(2),=C'SP'    PREFIX FOR REPORT NO.                        
         MVC   SYSPHASE,PHASENM    PRESET FOR SYSTEM CALLOVS                    
         MVI   NTWA,X'20'          SAVE/RESTORE 1 LARGE TWA                     
         MVC   LSVTWA0,=AL2(TWA018K)                                            
*                                                                               
* FIX RECUP TO USE FACPAK LINKED VERSION                                        
*                                                                               
         L     RE,SYSPARMS                                                      
         L     RE,8(RE)            POINT TO FACILITIES LIST                     
         MVC   VRECUP,28(RE)                                                    
*                                                                               
         L     R1,SYSPARMS                                                      
         L     R1,16(,R1)          COMFACS                                      
         USING COMFACSD,R1                                                      
         L     RF,CGETFACT                                                      
         DROP  R1                                                               
         GOTO1 (RF),DMCB,0                                                      
         L     RE,DMCB             FAFACTS                                      
         USING FACTSD,RE                                                        
         L     R1,FASYSLST                                                      
*                                                                               
SYS02F   CLI   FAOVSYS,13          STRAFFIC                                     
         BNE   SYS04                                                            
*                                                                               
         CLC   =C'BPAT',CONREC     BPAT RECORD                                  
         BE    SYS03                                                            
*                                                                               
         CLC   =C'BINST',CONREC    BINST RECORD                                 
         BE    SYS03                                                            
*                                                                               
         CLC   =C'TAL',CONREC      TALCOM                                       
         BE    SYS03                                                            
         CLC   =C'STR',CONREC      OR STRANS                                    
         BNE   *+8                                                              
SYS03    OI    GENSTAT4,NEWSCRM    WANT TO BE CALLED WITH NEWSCREEN             
*                                                                               
         MVC   SYSFIL(3),=C'TRF'                                                
         MVC   SYSDIR(3),=C'TRF'                                                
         MVI   RCPROG+1,C'T'       PREFIX FOR REPORT NO.                        
         B     SYS06                                                            
*                                                                               
SYS04    CLI   FAOVSYS,02          SPOT                                         
         BNE   SYS10                                                            
*                                                                               
SYS06    LARL  R1,RECACTS          SPOT RECORD/ACTION DIRECTORY                 
         MVI   SPOTNETF,C'S'       SET FLAG TO SPOT                             
                                                                                
         CLC   =C'SPO',CONREC                                                   
         BNE   SYS14                                                            
         CLC   =C'LIS',CONACT                                                   
         BNE   SYS14                                                            
         TM    GRIDST,GRIDSON                                                   
         BZ    SYS14                                                            
         LARL  R1,RECACTG          NET RECORD/ACTION DIRECTORY                  
         B     SYS14                                                            
*                                                                               
SYS10    CLI   FAOVSYS,03          NET                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  RE                                                               
*                                                                               
         MVC   RCPROG(2),=C'NE'    PREFIX FOR REPORT NO.                        
         LARL  R1,RECACTN          NET RECORD/ACTION DIRECTORY                  
         MVI   SPOTNETF,C'N'       SET FLAG TO NETWORK                          
*                                                                               
         CLC   =C'COMT',CONREC     IS THIS COMTEXT RECORD                       
         BNE   *+12                                                             
         OI    GENSTAT4,NEWSCRM    WANT TO BE CALLED WITH NEWSCREEN             
         OI    GENSTAT1,USKYMRG                                                 
*                                                                               
         CLC   =C'PAT',CONREC                                                   
         BE    SYS13                                                            
         CLC   =C'CAB',CONREC                                                   
         BNE   *+8                                                              
SYS13    OI    GENSTAT4,NEWSCRM                                                 
         CLC   =C'NET',CONREC                                                   
         BNE   SYS14                                                            
         CLC   =C'LIS',CONACT                                                   
         BNE   SYS14                                                            
         TM    GRIDST,GRIDSON                                                   
         BZ    SYS14                                                            
         LARL  R1,RECACTG          NET RECORD/ACTION DIRECTORY                  
                                                                                
SYS14    DS   0H                                                                
         ST    R1,ARECACT                                                       
         LA    R0,SVSTART          SET SAVED STORAGE START ADDR                 
         ST    R0,ASTARTSV                                                      
         L     R1,SYSPARMS                                                      
*                                                                               
         ICM   RE,15,TWAMASTC                                                   
         BZ    SYS30                                                            
*                                                                               
         LARL  R1,UTL                                                           
         MVC   0(4,R1),MCUTL-MASTD(RE)                                          
*                                                                               
SYS30    DS   0H                                                                
         LHI   R5,SVNCLIST-T216FFD                                              
         AR    R5,RA                                                            
         ST    R5,ASVNCLST                                                      
         SHI   R5,8                                                             
         MVC   0(8,R5),=C'*SVCLST*'                                             
                                                                                
SYS32    DS   0H                                                                
         LH    R6,=Y(SECBLK-T216FFD)                                            
         AR    R6,RA                                                            
         ST    R6,ASECBLK                                                       
         LR    RE,R6                                                            
         LA    RF,L'SECBLK                                                      
         XCEF                                                                   
*                                                                               
         L     RF,SYSPARMS                                                      
         L     RF,16(,RF)          COMFACS                                      
         L     RF,CSECRET-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,('SECPINIT',ASECBLK),0                                 
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         OI    GENSTAT3,MULTFILS   GET MODE SETFILE                             
*                                                                               
*        TM    SECFLAG,NEMORPRD    COMPLETELY CONVERTED OR BOTH                 
*        BZ    *+18                                                             
*        CLC   =CL7'COMMENT',CONREC                                             
*        BNE   *+8                                                              
         OI    GENSTAT4,NODELLST                                                
*                                                                               
         CLI   TWAOFFC,C'*'        THIS A DDS TERMINAL?                         
         BE    SYS35                                                            
         CLC   =CL7'PRODDEL',CONREC                                             
         BNE   SYS35                                                            
         OI    GENSTAT7,GES7DDS                                                 
         MVI   LRECACT,13                                                       
*                                                                               
SYS35    DS    0H                                                               
         OC    TWAVPRNT,TWAVPRNT   ONLY HAVE ADDRESS OFFLINE                    
         BNZ   SYSINITX                                                         
         OI    GENSTAT1,APPLIC                                                  
SYSINITX XIT1                                                                   
*                                                                               
PHASENM  DC    X'D9021600'    PRESET FOR SYSTEM CALLOVS                         
         DROP  RB,RC                                                            
*                                                                               
* READ CLT REC & SAVE NEW CLIST                                                 
*                                                                               
RDCLST   NMOD1 0,**+RDC**                                                       
*                                                                               
         L     RC,SVADGEND                                                      
         USING GEND,RC                                                          
         MVC   FILENAME,=CL8'SPTDIR'                                            
         SPACE                                                                  
         LH    R0,LKEY                                                          
         MVI   LKEY+1,13                                                        
         XC    KEY,KEY                                                          
         MVC   KEY+1(1),BAGYMD                                                  
         MVC   KEY+2(2),SVBCLTH                                                 
*                                                                               
         MVI   RDUPDATE,C'N'       NOT READ FOR UPDATE                          
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         SPACE                                                                  
         L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         MVC   FILENAME,=CL8'SPTFIL'                                            
         USING CLTHDRD,R6                                                       
         MVI   RDUPDATE,C'N'       NOT READ FOR UPDATE                          
         GOTO1 GETREC                                                           
         STH   R0,LKEY                                                          
         SPACE                                                                  
* SAVE CLIENT PRODUCT LIST *                                                    
         SPACE                                                                  
         LHI   R0,SVNCLIST-T216FFD                                              
         A     R0,ATWA                                                          
         LA    RE,CLIST                                                         
         LA    RF,880                                                           
         LR    R1,RF                                                            
         MVCL  R0,RE                                                            
         SPACE                                                                  
         LR    R2,R0               SAVE R0 POINTER                              
         SPACE                                                                  
         LA    R1,140              FILL REMAINDER WITH NULLS                    
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         SPACE                                                                  
         CLC   CLEN,=H'1500'       IS THIS A NEW CLIST CLT                      
         BL    VUSER64              NO                                          
         SPACE                                                                  
         LR    R0,R2               RESTORE R0 POINTER                           
         LA    RE,CLIST2                                                        
         LA    RF,140                                                           
         LR    R1,RF                                                            
         MVCL  R0,RE                                                            
         DROP  R6                                                               
         SPACE                                                                  
VUSER64  DS   0H                                                                
         XC    FILENAME,FILENAME                                                
         XIT1                                                                   
         SPACE                                                                  
         LTORG                                                                  
         DROP  RA,RB                                                            
         EJECT                                                                  
*                                                                               
* MODE = 0 - PROTECT SOME FIELDS IF TALCOM LIST                                 
CHKSCRN  NTR1  BASE=*,LABEL=*                                                   
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR THIS SYSTEM + PROG           
         CLC   CONREC(6),=CL6'TALCOM'                                           
         BNE   CKSCRX                                                           
         CLC   CONACT(3),=CL3'LIS'                                              
         BNE   CKSCRX                                                           
         CLI   MODE,0              IF MODE IS 0, PROTECT FIELDS                 
         BNE   CKSCRX                                                           
*                                                                               
         OI    TRATALH+1,X'20'     PROTECT INPUT FIELDS                         
         OI    TRATALH+6,X'80'     TALENT AGENCY                                
         OI    TRATSTH+1,X'20'     TEST RUN                                     
         OI    TRATSTH+6,X'80'                                                  
*                                                                               
         NI    TRAPTRH+1,X'FF'-X'20'     AND UNPROTECT INPUT FIELDS             
         OI    TRAPTRH+6,X'80'           PARTNER                                
         NI    TRAMKTH+1,X'FF'-X'20'     MARKET                                 
         OI    TRAMKTH+6,X'80'                                                  
*                                                                               
CKSCRX   XIT1                                                                   
         DROP  RA                                                               
         LTORG                                                                  
         EJECT                                                                  
* IF TALCOM LIST SCREEN, HIDE/PROTECT SOME SCREEN FIELDS                        
* IF MODE = NEWSCREEN                                                           
CHKSCRN2 NTR1  BASE=*,LABEL=*                                                   
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR THIS SYSTEM + PROG           
         CLI   TWASCR,X'E0'        TALCOM LIST OR STRANS REPORT?                
         BNE   CKSCRNO                                                          
         CLC   CONREC(6),=CL6'TALCOM'  MUST BE TALCOM LIST                      
         BNE   CKSCRNO                                                          
         CLC   CONACT(3),=CL3'LIS'                                              
         BNE   CKSCRNO                                                          
         CLI   MODE,NEWSCR         IF MODE IS NEWSCREEN, HIDE FIELDS            
         BNE   CKSCRNO                                                          
*                                                                               
         OI    TRATAL1H+1,X'0C'    HIDE TALENT AGENCY FIELD                     
         OI    TRATAL1H+6,X'80'                                                 
         OI    TRATST1H+1,X'0C'    AND TEST RUN FIELD                           
         OI    TRATST1H+6,X'80'                                                 
         OI    TRATALH+1,X'20'     AND PROTECT INPUT FIELDS                     
         OI    TRATALH+6,X'80'                                                  
         OI    TRATSTH+1,X'20'                                                  
         OI    TRATSTH+6,X'80'                                                  
*                                                                               
         NI    TRAPTR1H+1,X'FF'-X'0C'    SHOW PARTNER FIELD                     
         OI    TRAPTR1H+6,X'80'                                                 
         NI    TRAMKT1H+1,X'FF'-X'0C'    AND MARKET FIELD                       
         OI    TRAMKT1H+6,X'80'                                                 
         NI    TRAPTRH+1,X'FF'-X'20'     AND UNPROTECT INPUT FIELDS             
         OI    TRAPTRH+6,X'80'                                                  
         NI    TRAMKTH+1,X'FF'-X'20'                                            
         OI    TRAMKTH+6,X'80'                                                  
*                                                                               
CKSCRYES XR    RC,RC                                                            
CKSCRNO  LTR   RC,RC                                                            
         XIT1                                                                   
         DROP  RA                                                               
         LTORG                                                                  
*-----------------------------------------------------------------*             
* IF CG/PG POPULATE PID IF MODE = NEWSCREEN                                     
*-----------------------------------------------------------------*             
CHKCGPG  NTR1  BASE=*,LABEL=*                                                   
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR THIS SYSTEM + PROG           
         CLI   OFFLINE,C'Y'        ONLY CHECK ONLINE                            
         BE    CCGPGNO                                                          
         CLI   TWASCR,X'85'        PATTERN GEN SCREEN                           
         BE    CCGPG10                                                          
         CLI   TWASCR,X'73'        CABLE GEN SCREEN                             
         BNE   CCGPGNO                                                          
*                                                                               
CCGPG10  DS    0H                                                               
         CLC   CONACT(3),=CL3'GEN'                                              
         BNE   CCGPGNO                                                          
*                                                                               
         CLI   MODE,NEWSCR         IF MODE IS NEWSCREEN, HIDE FIELDS            
         BNE   CCGPGNO                                                          
*                                                                               
CCGPG20  DS    0H                                                               
         CLC   CONREC(3),=CL6'PAT'  MUST BE PAT GEN                             
         BNE   CCGPG30                                                          
         L     R4,ASECBLK                                                       
         USING SECD,R4                                                          
         MVC   PG$TRAPID,SECPID                                                 
         OI    PG$TRAPIDH+6,X'80'                                               
         OI    PG$TRAPIDH+6,X'20'                                               
         OI    PG$TRAPIDH+6,X'0C'                                               
*                                                                               
         OI    PG$TRAPIDH+1,X'2C'                                               
         B     CCGPGYES                                                         
*                                                                               
CCGPG30  DS    0H                                                               
         CLC   CONREC(3),=CL6'CAB'  MUST BE PAT GEN                             
         BNE   CCGPGNO                                                          
         L     R4,ASECBLK                                                       
         USING SECD,R4                                                          
         MVC   CG$TRAPID,SECPID                                                 
         OI    CG$TRAPIDH+6,X'80'                                               
         OI    CG$TRAPIDH+6,X'20'                                               
         OI    CG$TRAPIDH+6,X'0C'                                               
*                                                                               
         OI    CG$TRAPIDH+1,X'2C'                                               
         B     CCGPGYES                                                         
*                                                                               
CCGPGYES XR    RC,RC                                                            
CCGPGNO  LTR   RC,RC                                                            
         XIT1                                                                   
         DROP  RA,R4                                                            
         LTORG                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
EMAILMSG NTR1  BASE=*,LABEL=*      COMPOSE E-MAIL MESSAGE                       
         GOTO1 DATAMGR,DMCB,=C'OPMSG',(L'MSGTEXT1,MSGTEXT1)                     
         J     XIT                                                              
         LTORG                                                                  
                                                                                
MSGTEXT1 DC    C'AUTONOTE*MNAS: **MORE BRANDS OUT OF BAL**'                     
                                                                                
         DROP  RB                                                               
         EJECT                                                                  
*===================================================================            
* SET RECORD ACTION KEY TO MOVE FROM COMML TO COMTEXT/LIST                      
* OR PASS KEY INFO FROM COMTEXT TO COMMERCIAL IN NET ONLY                       
*===================================================================            
                                                                                
SETCOMT  NTR1  BASE=*,LABEL=*                                                   
         USING CONHEADH-64,RA                                                   
*                                                                               
         XC    CONREC,CONREC                                                    
         MVC   CONREC(7),=C'COMTEXT'                                            
         NI    CONRECH+4,X'DF'                                                  
         MVI   CONRECH+5,7                                                      
         XC    CONACT,CONACT                                                    
         MVC   CONACT(4),=C'LIST'                                               
         NI    CONACTH+4,X'DF'                                                  
         MVI   CONACTH+5,4                                                      
*                                                                               
         LA    R2,CONOTHH          POINT TO 'OTHERS' DATA                       
         BAS   RE,NEXTUNP          AND THEN TO FIRST KEY FIELD                  
*                                                                               
         LA    R1,CONKEY                                                        
         MVC   0(1,R1),8(R2)       MEDIA                                        
         MVI   1(R1),C','                                                       
         LA    R1,2(R1)                                                         
*                                                                               
         BAS   RE,NEXTUNP                                                       
         MVC   0(3,R1),8(R2)       CLIENT                                       
         LA    R1,2(R1)            POINT TO LAST CHAR                           
         CLI   0(R1),C' '                                                       
         BH    *+6                                                              
         BCTR  R1,0                                                             
         MVI   1(R1),C','                                                       
         LA    R1,2(R1)                                                         
         MVI   0(R1),C','          NO ENTRY FOR PRODUCT                         
         LA    R1,1(R1)                                                         
*                                                                               
         BAS   RE,NEXTUNP                                                       
         SR    RE,RE                                                            
         IC    RE,0(R2)            GET FIELD LENGTH                             
         AHI   RE,-8               ADJUST FOR FLDHDR                            
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),8(R2)                                                    
*                                                                               
         LA    R1,12(R1)                                                        
         CLI   0(R1),C' '          FIND LAST NON-BLANK CHAR                     
         BH    *+8                                                              
         BCT   R1,*-8                                                           
         LA    R0,CONKEY-1         BECAUSE R1 POINTS TO LAST CHAR               
         SR    R1,R0                                                            
         STC   R1,CONKEYH+5                                                     
*                                                                               
SETCOMTX XIT1                                                                   
*                                                                               
NEXTUNP  SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         TM    1(R2),X'20'         TEST PROT                                    
         JO    NEXTUNP                                                          
         BR    RE                                                               
         LTORG                                                                  
         EJECT                                                                  
************************************************************                    
*  CODE FOR PFM                                                                 
************************************************************                    
*                                                                               
PFMXFR   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     RF,SYSPARMS                                                      
         L     RF,16(RF)                                                        
         ICM   RF,15,CGLOBBER-COMFACSD(RF)                                      
         BZ    PFMXT                                                            
*                                                                               
         LR    R3,RF               R3 HAS A(GLOBBER)                            
         XC    ELEM,ELEM                                                        
         LA    R5,ELEM                                                          
         USING GLPFMFIL,R5                                                      
         MVC   GLPFMFIL(6),=C'TRFFIL'                                           
*                                                                               
         L     RF,SYSPARMS                                                      
         L     RF,16(RF)                                                        
         ICM   RF,15,CDATAMGR-COMFACSD(RF)                                      
         MVC   KEYSAVE,TWAKEYSV                                                 
*                                                                               
PFMXFR15 GOTO1 (RF),DMCB,(0,=C'DMRDHI'),=C'TRFDIR',KEYSAVE,KEY                  
         MVC   GLPFMDA,KEY+14                                                   
         MVC   GLPFMKEY(4),=C'*   '                                             
*                                                                               
PFMXFR17 LR    RF,R3                                                            
         GOTO1 (RF),DMCB,=C'PUTD',ELEM,54,GLPFMCDQ                              
*                                                                               
         XC    ELEM,ELEM                                                        
         LA    R5,ELEM                                                          
         USING GLVXFRSY,R5                                                      
         MVC   GLVXFRSY,=C'STR'                                                 
         MVC   GLVXTOSY,=C'STR'                                                 
PFMXFR20 MVC   GLVXFRPR,=C'TRA'                                                 
         MVC   GLVXTOPR,=C'PFM'                                                 
         OI    GLVXFLG1,GLV1SEPS                                                
         LR    RF,R3                                                            
         GOTO1 (RF),DMCB,=C'PUTD',ELEM,24,GLVXCTL                               
         DROP  R5                                                               
*                                                                               
PFMXT    XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* THIS ROUTINE WILL PROCESS I/O REQUESTS FROM VARIOUS CALLERS                   
*                                                                               
* IN ADDITION TO DATAMGR'S RETURN ERRORS, DMCB+4 ALSO HAS FOLLOWING:            
*                                                                               
* X'80' END OF FILE                                 (DATAMGR)                   
* X'40' NON RECOVERABLE DISK ERROR                  (DATAMGR)                   
* X'20' DUPLICATE KEY ON ADD                        (DATAMGR)                   
* X'10' RECORD NOT FOUND                            (DATAMGR)                   
* X'02' RECORD IS DELETED                           (DATAMGR)                   
* X'04' UNIT FILE IS NOT UPDATED                                                
* X'06' PRD OVERFLOW IN UNIT FILE                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
PRCIOREQ NMOD1 PRCIOWKX-PRCIOWKD,PRCIOREQ,CLEAR=YES                             
*                                                                               
         LR    R6,RC                                                            
         USING PRCIOWKD,R6         LOCAL STORAGE                                
         LA    RC,SPOOLEND         REESTABLISH GEND                             
         MVC   KEYSAVE,KEY                                                      
         MVC   PRKEYSAV,KEY                                                     
         MVC   PRCIKEY1,KEY        SAVE KEY BEFORE I/O HANDLING                 
         MVC   PRCSVAIO,DMCB+4     SAVE AIO POINTER                             
         MVC   PRCSVDIS,DATADISP   SAVE ORIGINAL DATA DISPLACEMENT              
         LA    RE,PRCIAIO1                                                      
         ST    RE,PRCAAIO1                                                      
         MVC   PRCICMND,DMCB+3     SAVE COMMAND NUMBER                          
         NI    PRCICMND,X'0F'      ISOLATE COMMAND NUMBER                       
         MVC   PRCISVFL,DMCB+3     SAVE FILENUMBER                              
         NI    PRCISVFL,X'70'      ISOLATE FILE NUMBER                          
         MVC   PRCIINBT,DMCB+2     SAVE INPUT BITS                              
         MVI   PRCIFLAG,0          INIT FLAG                                    
         CLI   PRCICMND,GETQ                                                    
         BE    *+8                                                              
         CLI   PRCICMND,PUTQ                                                    
         BE    *+8                                                              
         CLI   PRCICMND,ADDRECQ                                                 
         BNE   PRCIO_14                                                         
         CLC   DMCB+4(4),AIO1                                                   
         BE    PRCIO_12                                                         
         CLC   DMCB+4(4),AIO2                                                   
         BE    PRCIO_12                                                         
         CLC   DMCB+4(4),AIO3                                                   
         BE    PRCIO_12                                                         
         XC    DMCB+4(4),DMCB+4    2ND PARAMETER IS INVALID                     
         MVC   PRCSVAIO,AIO        USE WHATEVER AIO IS POINTING TO              
PRCIO_12 MVC   PRCIPAR1(6*4),DMCB  SAVE I/O CALLER'S PARAMETER                  
*                                                                               
PRCIO_14 CLI   PRCICMND,ADDRECQ                                                 
         BNE   *+26                                                             
         L     RF,PRCSVAIO                                                      
         XC    KEY,KEY                                                          
         MVC   KEY(32),0(RF)                                                    
         MVC   PRCIKEY1,KEY                                                     
*                                                                               
         LA    RE,PIO_RTTB         POINT TO REC TYPE TABLE                      
PRCIO_16 CLI   0(RE),0             END OF TABLE?                                
         BNE   *+6                                                              
         DC    H'0'                MUST SUPPLY CORRECT REC TYPE                 
         CLC   0(2,RE),KEY         REC TYPE MATCH THAT IN TABLE?                
         BE    *+12                                                             
         LA    RE,L'PIO_RTTB(RE)   POINT TO NEXT ENTRY IN TABLE                 
         B     PRCIO_16                                                         
         MVC   PRCIFILE,2(RE)      SAVE FILE EQUATE FROM TABLE                  
         MVC   PRCIDISP,3(RE)      SAVE REC DISPLACEMENT FROM TABLE             
         MVC   PRCIKEYL,4(RE)      SAVE LENGTH OF KEY FROM TABLE                
         MVC   PRCIDALP,5(RE)      SAVE DISPLACEMENT TO AGY ALPHA               
*                                                                               
         TM    SECFLAG,NECONPRD    XSPF ONLY?                                   
         BZ    PRCIO_18                                                         
         MVI   PRCIFILE,XSPQ       SET TO XSPOT                                 
         MVI   PRCIDISP,32+2+4+4   SET XSPOT DISPLACEMENT TO ELEM               
         MVI   PRCIKEYL,32         SAVE XSPOT KEY LENGTH                        
*                                                                               
PRCIO_18 TM    SECFLAG,NECONPRD    XSPF ONLY?                                   
         BZ    PRCIO_20                                                         
         BRAS  RE,PRCIOCKP         CK FOR PUTREC                                
         BNE   PRCIO_X1                                                         
         LA    R1,DMCB                                                          
         BRAS  RE,IOCALL                                                        
         B     PRCIO_XX                                                         
*                                                                               
PRCIO_20 TM    SECFLAG,NEMORPRD    XSPOT & TARGET FILE (PARALLEL MODE)?         
         BZ    PRCIO_70                                                         
         BRAS  RE,PRCIOCKP         CK FOR PUTREC                                
         BNE   PRCIO_X1                                                         
         LA    R1,DMCB                                                          
         BRAS  RE,IOCALL           PROCESS XSPT FILE                            
         CLI   8(R1),0             ANY DATAMGR ERROR?                           
         BE    PRCIO_30                                                         
         CLI   PRCICMND,WRITEQ                                                  
         BE    PRCIO_X1                                                         
         CLI   PRCICMND,PUTQ                                                    
         BE    PRCIO_X1                                                         
         CLI   PRCICMND,ADDRECQ                                                 
         BE    PRCIO_X1                                                         
         B     PRCIO_XX                                                         
*                                                                               
PRCIO_30 CLI   PRCICMND,WRITEQ                                                  
         BE    PRCIO_32                                                         
         CLI   PRCICMND,PUTQ                                                    
         BE    PRCIO_32                                                         
         CLI   PRCICMND,ADDRECQ                                                 
         BE    PRCIO_32                                                         
         B     PRCIO_XX                                                         
*                                                                               
PRCIO_32 BRAS  RE,PRCIOKEY         SET TARGET KEY                               
         BNE   PRCIO_X2                                                         
         CLI   PRCICMND,WRITEQ                                                  
         BNE   PRCIO_46                                                         
         XC    DMCB(4*6),DMCB                                                   
         LA    R1,DMCB                                                          
         MVI   3(R1),HIGHQ+DIRQ                                                 
         OC    2(1,R1),PRCIINBT    DATAMGR INPUT BITS                           
         OC    3(1,R1),PRCIFILE    FILE EQUATE                                  
         MVC   PRKEYTMP,KEY                                                     
         BRAS  RE,IOCALL                                                        
                                                                                
         CLC   KEY(13),PRKEYTMP                                                 
         BE    PRCIO_36                                                         
         BRAS  RE,EMAILMSG         COMPOSE E-MAIL MESSAGE                       
         DC    H'0'                                                             
                                                                                
PRCIO_36 DS    0H                                                               
         TM    DMCB+8,X'FF'-X'02'                                               
         BNZ   PRCIO_X1                                                         
         LA    RE,KEY                                                           
         SR    RF,RF                                                            
         IC    RF,PRCIKEYL                                                      
         AR    RE,RF                                                            
         MVC   0(1,RE),PRCIKEY1+32                                              
         XC    DMCB(4*6),DMCB                                                   
         LA    R1,DMCB                                                          
         MVI   3(R1),WRITEQ+DIRQ                                                
         OC    3(1,R1),PRCIFILE    FILE EQUATE                                  
         BRAS  RE,IOCALL                                                        
         MVC   PRCIPARX(6*4),DMCB                                               
         MVC   KEY,PRCIKEY2                                                     
         XC    DMCB(4*6),DMCB                                                   
         LA    R1,DMCB                                                          
         MVI   3(R1),HIGHQ+DIRQ+XSPQ                                            
         OC    2(1,R1),PRCIINBT    DATAMGR INPUT BITS                           
         BRAS  RE,IOCALL                                                        
         MVC   DMCB(6*4),PRCIPARX                                               
         TM    DMCB+8,X'FF'-X'02'                                               
         BNZ   PRCIO_X1                                                         
         B     PRCIO_XX                                                         
*                                                                               
PRCIO_46 CLI   PRCICMND,PUTQ                                                    
         BNE   PRCIO_52                                                         
         BRAS  RE,PRCIOPUT         SET PUTREC SEQUENCE                          
         BNE   PRCIO_X1                                                         
         BRAS  RE,PRCIOREC         SET TARGET REC                               
         XC    DMCB(4*6),DMCB                                                   
         LA    R1,DMCB                                                          
         MVI   3(R1),PUTQ+FILQ                                                  
         OC    3(1,R1),PRCIFILE    FILE EQUATE                                  
         MVC   4(4,R1),PRCAAIO1                                                 
         BRAS  RE,IOCALL                                                        
         MVC   PRCIPARX(6*4),DMCB                                               
         MVC   KEY,PRCIKEY2                                                     
         GOTOR IOCALL,DMCB,HIGHQ+DIRQ+XSPQ                                      
         L     RF,PRCSVAIO                                                      
         GOTOR IOCALL,DMCB,GETQ+XSPQ+UPDATEQ,(RF)                               
         MVC   DMCB(6*4),PRCIPARX                                               
         CLI   DMCB+8,0                                                         
         BE    PRCIO_XX                                                         
         B     PRCIO_X1                                                         
*                                                                               
PRCIO_52 CLI   PRCICMND,ADDRECQ                                                 
         BE    *+6                                                              
         DC    H'0'                NO OTHER COMMAND AT THIS TIME                
         BRAS  RE,PRCIOREC         SET TARGET REC                               
         XC    DMCB(4*6),DMCB                                                   
         LA    R1,DMCB                                                          
         MVI   3(R1),ADDRECQ                                                    
         OC    3(1,R1),PRCIFILE    FILE EQUATE                                  
         MVC   4(4,R1),PRCAAIO1                                                 
         BRAS  RE,IOCALL                                                        
         MVC   PRCIPARX(6*4),DMCB                                               
         MVC   KEY,PRCIKEY2                                                     
         MVC   DMCB(6*4),PRCIPAR2                                               
         CLI   PRCIPARX+8,0                                                     
         BE    PRCIO_XX                                                         
         B     PRCIO_X1                                                         
*                                                                               
PRCIO_70 CLI   PRCICMND,GETQ                                                    
         BNE   PRCIO_74                                                         
         MVC   PRCISVDA,KEY+32+4   DISK ADDRESS OF RECORD                       
         SR    RE,RE                                                            
         IC    RE,PRCIKEYL                                                      
         AHI   RE,1                BY PASS STATUS BYTE                          
         LA    RF,KEY                                                           
         AR    RF,RE                                                            
         MVC   0(4,RF),PRCISVDA                                                 
         NI    DMCB+3,X'8F'        RESET FILE NUMBER                            
         OC    DMCB+3(1),PRCIFILE  FILE EQUATE                                  
         MVC   DMCB+4(4),PRCAAIO1  USE TEMPORARY AIO                            
         MVI   PRCIBYT1,C'Y'       TEMPORARY AIO IS USED                        
         B     PRCIO_78                                                         
*                                                                               
PRCIO_74 BRAS  RE,PRCIOKEY         SET TARGET KEY                               
         BNE   PRCIO_X2                                                         
         MVI   PRCIBYT1,0                                                       
         NI    DMCB+3,X'8F'        RESET FILE NUMBER                            
         OC    DMCB+3(1),PRCIFILE  FILE EQUATE                                  
         CLI   PRCICMND,ADDRECQ                                                 
         BE    PRCIO_76                                                         
         CLI   PRCICMND,PUTQ                                                    
         BNE   PRCIO_78                                                         
         MVC   PRCIPARX(6*4),DMCB                                               
         BRAS  RE,PRCIOPUT         SET PUTREC SEQUENCE                          
         BNE   PRCIO_X1                                                         
         MVC   DMCB(6*4),PRCIPARX                                               
PRCIO_76 MVC   DMCB+4(4),PRCAAIO1  USE TEMPORARY AIO                            
         MVI   PRCIBYT1,C'Y'       TEMPORARY AIO IS USED                        
         BRAS  RE,PRCIOREC         SET TARGET REC                               
PRCIO_78 LA    R1,DMCB                                                          
         BRAS  RE,IOCALL           PROCESS TARGET REC, DON'T CK FOR ERR         
*                                                                               
         CLI   PRCIBYT1,C'Y'       TEMPORARY AIO IS USED?                       
         BNE   PRCIO_82                                                         
         BRAS  RE,PRCIOXSP         SET XSPOT REC                                
         CLI   PRCICMND,GETQ                                                    
         BNE   PRCIO_82                                                         
         L     RE,PRCAAIO1                                                      
         MVC   KEY,0(RE)           NEEDED TO RECONSTRUCT XSPOT KEY              
         SR    RF,RF                                                            
         IC    RF,PRCIKEYL                                                      
         AHI   RF,2                2 BYTES RECORD LENGTH                        
         AR    RE,RF                                                            
         MVC   PRCIBYT2,0(RE)      RECORD CONTROL STATUS BYTE                   
         LA    RE,KEY                                                           
         IC    RF,PRCIKEYL                                                      
         AR    RE,RF                                                            
         MVC   0(1,RE),PRCIBYT2                                                 
*                                                                               
PRCIO_82 MVC   PRCIUNTK,KEY                                                     
         LA    R3,KEY                                                           
         BRAS  RE,PRCIOXSK         SET XSPOT KEY                                
         BNE   PRCIO_X2                                                         
         CLI   PRCIBYT1,C'Y'       TEMPORARY AIO IS USED?                       
         BNE   PRCIO_84                                                         
         L     RE,PRCSVAIO                                                      
         MVC   0(32,RE),KEY        SET XSPOT KEY IN REC                         
         B     PRCIO_XX                                                         
PRCIO_84 DS    0H                                                               
         MVC   PRCIUNTK,PRKEYSAV                                                
         LA    R3,PRKEYSAV                                                      
         BRAS  RE,PRCIOXSK         SET XSPOT KEY                                
         BNE   PRCIO_X2                                                         
         B     PRCIO_XX                                                         
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
PRCIO_X1 DC    H'0'                                                             
         OI    DMCB+8,X'04'        CANNOT UPDATE UNTF                           
         B     PRCIO_XX                                                         
*                                                                               
PRCIO_X2 DS    0H                                                               
                                                                                
         CLI   PRCICMND,HIGHQ                                                   
         BE    PRCIO_XX                                                         
         CLI   PRCICMND,SEQQ                                                    
         BE    PRCIO_XX                                                         
                                                                                
         LA    RE,PIO_RTTB         POINT TO REC TYPE TABLE                      
PRCIO_X3 CLI   0(RE),0             END OF TABLE?                                
         BNE   *+6                                                              
         DC    H'0'                MUST SUPPLY CORRECT REC TYPE                 
         CLC   0(2,RE),KEY         REC TYPE MATCH THAT IN TABLE?                
         BE    *+12                                                             
         LA    RE,L'PIO_RTTB(RE)   POINT TO NEXT ENTRY IN TABLE                 
         B     PRCIO_X3                                                         
         B     PRCIO_XX                                                         
                                                                                
PRCIO_XD DS    0H                                                               
         BRAS  RE,EMAILMSG         COMPOSE E-MAIL MESSAGE                       
         DC    H'0'                                                             
         MVC   DMCB(6*4),PRCIPAR2                                               
         OI    DMCB+8,X'06'        PRD OVERFLOW                                 
         B     PRCIO_X1                                                         
*                                                                               
PRCIO_XX LA    R1,DMCB                                                          
         CLI   8(R1),X'02'         RECORD IS DELETED?                           
         BNE   *+12                                                             
         TM    PRCIINBT,X'01'      PASS DELETED REC?                            
         BNZ   *+14                                                             
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   DATADISP,PRCSVDIS   RESTORE ORIGINAL DATA DISPLACEMENT           
* * * *  TM    PRCIFLAG,XSPTRCNQ   XSPOT REC CONVERSION TOOK PLACE?             
* * * *  BZ    *+10                                                             
* * * *  MVC   DATADISP,=X'002A'                                                
         CLI   PRCICMND,GETQ                                                    
         BNE   *+10                                                             
         MVC   KEY,PRCIKEY1        RESTORE ORIGINAL KEY                         
*                                                                               
XIT_R1   XIT1  REGS=(R1)           RETURN COMPLETION/ERROR                      
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
PRCIOKEY ST    RE,PRCISVRE         SET TARGET KEY                               
         MVC   PRCIKEY2,KEY        SAVE KEY AFTER I/O HANDLING                  
         MVC   PRCIPAR2(6*4),DMCB  SAVE DMCB AFTER I/O HANDLING                 
         XC    KEY,KEY             BUILD TARGET KEY                             
         LA    R2,PRCIKEY1                                                      
         USING NPTXKEY,R2                                                       
*                                                                               
K        USING REVKEY,KEY                                                       
         USING REVXKEY,R2                                                       
PIO_K20  CLC   PIO_0A1D,REVXKID    XSPT REVISION REC?                           
         BNE   PIO_K30                                                          
         MVI   K.REVKID,X'21'      REC ID                                       
         MVC   K.REVKAM,REVXKAM    AGENCY/MEDIA                                 
         MVC   K.REVKCLT,REVXKCLT  CLIENT                                       
         MVC   K.REVKNET,REVXKNET  NETWORK                                      
         MVC   K.REVKPRG,REVXKPRG  PROGRAM                                      
         MVC   K.REVKPER,REVXKPER  PERIOD                                       
         MVC   K.REVKNUM,REVXKNUM  REVISION NUMBER                              
         MVC   PRCIXPRD,REVXKPRD                                                
         OC    PRCIXPRD,PRCIXPRD   EXPANDED PRD CODE PRESENT?                   
         BZ    *+18                                                             
         BRAS  RE,PRCIOPRD         LOOK UP BINARY PRD CODE                      
         BNE   PIO_KNEQ            ERROR - PRD OVERFLOW                         
         MVC   K.REVKPRD,PRCIBPRD  PRODUCT                                      
         OC    REVXKPGR,REVXKPGR   PRODUCT GROUP PRESENT?                       
         BZ    *+14                                                             
         MVC   K.REVKPGR,REVXKPGR  PRODUCT GROUP                                
         OI    KEY+20,X'01'        INDICATE PRD GROUP                           
         B     PIO_KEQ                                                          
         DROP  R2,K                                                             
*                                                                               
K        USING DTXKEY,KEY                                                       
         USING DT2KEY,R2                                                        
PIO_K30  CLC   PIO_0A2D,DT2KID     XSPT DEALER TAG TEXT REC?                    
         BNE   PIO_K40                                                          
         MVC   K.DTXKID,DT2KID     REC ID                                       
         MVC   K.DTXKAM,DT2KAM     AGENCY/MEDIA                                 
         MVC   K.DTXKCLT,DT2KCLT   CLIENT                                       
         MVC   K.DTXKMED,DT2KMED   MEDIA                                        
         MVC   K.DTXKNET,DT2KNET   NETWORK                                      
         MVC   PRCIXPRD,DT2KPRD                                                 
         OC    PRCIXPRD,PRCIXPRD   EXPANDED PRD CODE PRESENT?                   
         BZ    *+18                                                             
         BRAS  RE,PRCIOPRD         LOOK UP BINARY PRD CODE                      
         BNE   PIO_KNEQ            ERROR - PRD OVERFLOW                         
         MVC   K.DTXKPRD,PRCIBPRD  PRODUCT                                      
         MVC   K.DTXKPG,DT2KPG     PAGE                                         
         B     PIO_KEQ                                                          
         DROP  R2,K                                                             
*                                                                               
PIO_K40  B     PIO_KEQ             FOR ADDITIONAL REC TYPES...                  
*                                                                               
PIO_KEQ  SR    RE,RE                                                            
         IC    RE,PRCIKEYL                                                      
         LA    RF,KEY                                                           
         AR    RF,RE                                                            
         OC    0(1,RF),32+0(R2)    STANDARD DIR STATUS                          
         MVC   1(4,RF),32+4(R2)    DISK ADDRESS                                 
         SR    RE,RE                                                            
PIO_KNEQ LTR   RE,RE                                                            
         L     RE,PRCISVRE                                                      
         BR    RE                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
PRCIOPRD MVI   PRCIBPRD,0                                                       
         CLC   PRCIXPRD,=X'FFFFFF'                                              
         JNE   *+12                                                             
         MVI   PRCIBPRD,X'FF'                                                   
         J     PIO_P92                                                          
         LHI   RF,SVNCLIST-T216FFD TABLE OF CLIENT PROD CODES                   
         A     RF,ATWA                                                          
PIO_P50  CLI   0(RF),0             ANY PRD CODE?                                
         JE    PIO_P_X                                                          
         CLC   PRCIXPRD,0(RF)                                                   
         JE    *+12                                                             
         LA    RF,4(RF)            BUMP TO NEXT PRD IN TABLE                    
         J     PIO_P50                                                          
         MVC   PRCIBPRD,3(RF)                                                   
PIO_P92  SR    RF,RF               WILL SET CC TO EQUAL                         
PIO_P_X  LTR   RF,RF                                                            
         BR    RE                  RETURN WITH CC                               
*                                                                               
PRCIOBPD XC    PRCIXPRD,PRCIXPRD                                                
         LHI   RF,SVNCLIST-T216FFD TABLE OF CLIENT PROD CODES                   
         A     RF,ATWA                                                          
PIO_B50  CLI   0(RF),0             ANY PRD CODE?                                
         JE    PIO_B_X                                                          
         CLC   PRCIBPRD,3(RF)                                                   
         JE    *+12                                                             
         LA    RF,4(RF)            BUMP TO NEXT PRD IN TABLE                    
         J     PIO_B50                                                          
         MVC   PRCIXPRD,0(RF)                                                   
         SR    RF,RF               WILL SET CC TO EQUAL                         
PIO_B_X  LTR   RF,RF                                                            
         BR    RE                  RETURN WITH CC                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
PRCIOREC ST    RE,PRCISVRE         CONVERT XSPOT REC TO TARGET REC              
         L     RE,PRCAAIO1                                                      
         LHI   RF,4096                                                          
         XCEFL                                                                  
         SR    R2,R2               TO CALCULATE REC LENGTH                      
         IC    R2,PRCIDISP         DISPLACEMENT TO ELEM IN REC                  
         L     RE,PRCAAIO1                                                      
         SR    RF,RF                                                            
         IC    RF,PRCIKEYL                                                      
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),KEY                                                      
*                                                                               
         AHI   RF,2+1                                                           
         AR    RE,RF               POINT TO FILE STATUS IN TARGET REC           
         L     RF,PRCSVAIO                                                      
         LA    RF,32+2(RF)         POINT TO STANDARD FILE STATUS                
         MVC   0(1,RE),0(RF)       SET STANDARD FILE STA IN TARGET REC          
         L     RE,PRCAAIO1                                                      
*                                                                               
         CLI   PRCIDALP,0          DISPLACEMENT TO AGY ALPHA PRESENT?           
         BE    PIO_R20                                                          
         SR    RF,RF                                                            
         IC    RF,PRCIDALP                                                      
         AR    RE,RF                                                            
         L     RF,PRCSVAIO                                                      
         MVC   0(2,RE),38(RF)      AGENCY ALPHA                                 
PIO_R20  L     RE,PRCAAIO1                                                      
         AR    RE,R2               POINT TO ELEM AREA                           
         L     RF,PRCSVAIO                                                      
         LA    RF,42(RF)           POINT TO ELEM AREA                           
         CLI   0(RF),0             FIRST ELEM PRESENT?                          
         BNE   *+6                                                              
         DC    H'0'                REC IS EMPTY...                              
         SR    R1,R1                                                            
PIO_R30  IC    R1,1(RF)            ELEM LENGTH                                  
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),0(RF)       COPY ELEM TO TARGET REC                      
         AHI   R1,1                RESTORE ELEM LENGTH                          
         AR    RE,R1               POINT TO NEXT ELEM                           
         AR    RF,R1                                                            
         AR    R2,R1               ADD ON TO TOTAL REC LENGTH                   
         CLI   0(RF),0             END OF XSPOT REC?                            
         BNE   PIO_R30                                                          
         AHI   R2,1                EXTRA BYTE                                   
PIO_R_X  L     RE,PRCAAIO1                                                      
         SR    RF,RF                                                            
         IC    RF,PRCIKEYL         TARGET KEY LENGTH                            
         AR    RE,RF                                                            
         STCM  R2,3,0(RE)          TARGET REC LENGTH                            
*                                                                               
         L     RF,PRCSVAIO                                                      
         USING REVXKEY,RF                                                       
         CLC   REVXKID,PIO_0A1D    REVISION RECORD?                             
         BNE   PIO_R_XX                                                         
         OC    REVXKPGR,REVXKPGR   BY PRD GROUP?                                
         BZ    PIO_R_XX                                                         
         L     RE,PRCAAIO1                                                      
         OI    20+2(RE),X'01'      CONTROL BYTE INDICATE PRD GROUP              
*                                                                               
PIO_R_XX L     RE,PRCISVRE                                                      
         BR    RE                                                               
         DROP  RF                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
PRCIOXSP ST    RE,PRCISVRE         CONVERT TO XSPOT REC                         
         L     RE,PRCSVAIO                                                      
         XC    0(42,RE),0(RE)      CALLER MIGHT USE REST FOR WORK               
         OI    PRCIFLAG,XSPTRCNQ   XSPOT REC CONVERSION TOOK PLACE              
         LHI   R2,32+2+4+4         TO CALCULATE REC LENGTH                      
*                                                                               
         L     RF,PRCAAIO1                                                      
         SR    RE,RE                                                            
         IC    RE,PRCIKEYL                                                      
         AR    RF,RE                                                            
         L     RE,PRCSVAIO                                                      
         MVC   32+2(1,RE),2(RF)    SET STANDARD FILE STA IN XSPOT REC           
*                                                                               
         L     RF,PRCAAIO1                                                      
         CLI   PRCIDALP,0          DISPLACEMENT TO AGY ALPHA PRESENT?           
         BE    PXSP_20                                                          
         SR    RF,RF                                                            
         IC    RF,PRCIDALP                                                      
         L     R1,PRCAAIO1                                                      
         AR    R1,RF                                                            
         L     RE,PRCSVAIO                                                      
         MVC   38(2,RE),0(R1)      AGENCY ALPHA                                 
PXSP_20  SR    RE,RE                                                            
         L     RF,PRCAAIO1                                                      
         IC    RE,PRCIDISP         DISPLACEMENT TO ELEM IN REC                  
         AR    RF,RE               POINT TO ELEM AREA                           
         L     RE,PRCSVAIO                                                      
         LA    RE,32+2+4+4(RE)     POINT TO ELEM AREA                           
         CLI   0(RF),0             FIRST ELEM PRESENT?                          
         BNE   *+6                                                              
         DC    H'0'                REC IS EMPTY...                              
         SR    R1,R1                                                            
PXSP_30  IC    R1,1(RF)            ELEM LENGTH                                  
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),0(RF)       COPY ELEM TO XSPOT REC                       
         AHI   R1,1                RESTORE ELEM LENGTH                          
         AR    RE,R1               POINT TO NEXT ELEM                           
         AR    RF,R1                                                            
         AR    R2,R1               ADD ON TO TOTAL REC LENGTH                   
         CLI   0(RF),0             END OF XSPOT REC?                            
         BNE   PXSP_30                                                          
         AHI   R2,1                EXTRA BYTE                                   
PXSP_X   L     RE,PRCSVAIO                                                      
         STCM  R2,3,32(RE)                                                      
         SHI   R2,1                                                             
         AR    RE,R2                                                            
         XC    0(2,RE),0(RE)       CLEAR 2 BYTES AFTER END OF RECORD            
         L     RE,PRCISVRE                                                      
         BR    RE                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
PRCIOXSK ST    RE,PRCISVRE         SET XSPOT KEY                                
         LA    R2,PRCIUNTK                                                      
         USING NPTKEY,R2                                                        
*                                                                               
         USING REVKEY,R2                                                        
PIO_X20  CLI   REVKID,X'21'        UNIT REVISION REC?                           
         BNE   PIO_X30                                                          
         XC    0(L'KEY,R3),0(R3)   BUILD XSPOT KEY                              
         MVC   0(L'REVXKID,R3),PIO_0A1D                                         
         MVC   REVXKAM-REVXKEY(L'REVXKAM,R3),REVKAM                             
         MVC   REVXKCLT-REVXKEY(L'REVXKCLT,R3),REVKCLT                          
         MVC   REVXKNET-REVXKEY(L'REVXKNET,R3),REVKNET                          
         MVC   REVXKPRG-REVXKEY(L'REVXKPRG,R3),REVKPRG                          
         MVC   REVXKPER-REVXKEY(L'REVXKPER,R3),REVKPER                          
         MVC   REVXKNUM-REVXKEY(L'REVXKNUM,R3),REVKNUM                          
         TM    20(R2),X'01'        PRD GROUP?                                   
         BZ    *+14                                                             
         MVC   REVXKPGR-REVXKEY(L'REVXKPGR,R3),REVKPGR                          
         B     PIO_XEQ                                                          
         MVC   PRCIBPRD,REVKPRD                                                 
         CLI   PRCIBPRD,0          BINARY PRD CODE PRESENT?                     
         BE    PIO_XEQ                                                          
         BRAS  RE,PRCIOBPD         LOOK UP EXTENDED PRD CODE                    
         BNE   PIO_XNEQ            ERROR - PRD OVERFLOW                         
         MVC   REVXKPRD-REVXKEY(L'REVXKPRD,R3),PRCIXPRD                         
         B     PIO_XEQ                                                          
*                                                                               
         USING DTXKEY,R2                                                        
PIO_X30  CLC   PIO_0A2D,DTXKID     SPOT DEALER TAG TEXT REC?                    
         BNE   PIO_X40                                                          
         XC    0(L'KEY,R3),0(R3)   BUILD XSPOT KEY                              
         MVC   0(L'DT2KID,R3),PIO_0A2D                                          
         MVC   DT2KAM-DT2KEY(L'DT2KAM,R3),DTXKAM                                
         MVC   DT2KCLT-DT2KEY(L'DT2KCLT,R3),DTXKCLT                             
         MVC   DT2KMED-DT2KEY(L'DT2KMED,R3),DTXKMED                             
         MVC   DT2KNET-DT2KEY(L'DT2KNET,R3),DTXKNET                             
         MVC   PRCIBPRD,DTXKPRD                                                 
         CLI   PRCIBPRD,0          BINARY PRD CODE PRESENT?                     
         BE    *+18                                                             
         BRAS  RE,PRCIOBPD         LOOK UP EXTENDED PRD CODE                    
         BNE   PIO_XNEQ            ERROR - PRD OVERFLOW                         
         MVC   DT2KPRD-DT2KEY(L'DT2KPRD,R3),PRCIXPRD                            
         MVC   DT2KPG-DT2KEY(L'DTXKPG,R3),DTXKPG                                
         B     PIO_XEQ                                                          
*                                                                               
PIO_X40  B     PIO_XEQ             FOR ADDITIONAL REC TYPES...                  
*                                                                               
PIO_XEQ  SR    RE,RE                                                            
         IC    RE,PRCIKEYL                                                      
         AR    R2,RE                                                            
         OC    32+0(1,R3),0(R2)    STANDARD DIR STATUS                          
         MVC   32+4(4,R3),1(R2)    DISK ADDRESS                                 
         SR    RE,RE                                                            
PIO_XNEQ LTR   RE,RE                                                            
         L     RE,PRCISVRE                                                      
         BR    RE                                                               
         DROP  R2                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
PRCIOCKP ST    RE,PRCISVR2         CK FOR PUTREC (XSPOT REC)                    
*                                                                               
         MVC   PRCIPARX(6*4),DMCB                                               
         MVC   PRCIBYT3,PRCIFILE   SAVE WORKING FILE VALUE                      
         MVC   PRCIFILE,PRCISVFL   DEFAULT TO CALLER'S FILE                     
         CLI   PRCICMND,PUTQ                                                    
         BNE   PIO_CEQ                                                          
         XC    KEY,KEY                                                          
         L     RF,PRCSVAIO                                                      
         MVC   KEY(32),0(RF)                                                    
         MVC   PRKEYSAV,KEY                                                     
         BRAS  RE,PRCIOPUT                                                      
         BNE   PIO_CNEQ                                                         
*                                                                               
PIO_CEQ  SR    RE,RE                                                            
*                                                                               
PIO_CNEQ LTR   RE,RE                                                            
         MVC   DMCB(6*4),PRCIPARX                                               
         MVC   PRCIFILE,PRCIBYT3   RESTORE WORKING FILE VALUE                   
         L     RE,PRCISVR2                                                      
         BR    RE                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
PRCIOPUT ST    RE,PRCISVRE         SET PUTREC SEQUENCE                          
*                                                                               
         XC    DMCB(6*4),DMCB                                                   
         LA    R1,DMCB                                                          
         MVI   3(R1),HIGHQ+DIRQ                                                 
         OC    2(1,R1),PRCIINBT    DATAMGR INPUT BITS                           
         OC    3(1,R1),PRCIFILE    FILE EQUATE                                  
         MVC   PRKEYTMP,KEY                                                     
         BRAS  RE,IOCALL                                                        
                                                                                
         CLC   KEY(13),PRKEYTMP                                                 
         BE    PIO_P10                                                          
         BRAS  RE,EMAILMSG         COMPOSE E-MAIL MESSAGE                       
         DC    H'0'                                                             
                                                                                
PIO_P10  DS    0H                                                               
         CLI   DMCB+8,0            TARGET REC ACCQUIRED?                        
         BNE   PIO_PNEQ                                                         
         XC    DMCB(4*6),DMCB                                                   
         LA    R1,DMCB                                                          
         LHI   RE,GETQ+UPDATEQ                                                  
         STCM  RE,15,0(R1)                                                      
         OC    3(1,R1),PRCIFILE    FILE EQUATE                                  
         MVC   4(4,R1),PRCAAIO1    USE TEMP AIO AREA                            
         BRAS  RE,IOCALL                                                        
         CLI   DMCB+8,0            TARGET REC ACCQUIRED?                        
         BNE   PIO_PNEQ                                                         
*                                                                               
PIO_PEQ  SR    RE,RE                                                            
*                                                                               
PIO_PNEQ LTR   RE,RE                                                            
         L     RE,PRCISVRE                                                      
         BR    RE                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
PIO_RTTB DS    0XL6                                                             
*                                                                               
PIO_0A61 DC    XL2'0A61'           PATTERN REC CODE (XSPT)                      
         DC    AL1(UNTQ)           TARGET DIR/FILE EQUATE                       
         DC    AL1(20+02+01+04)    TARGET DISPLACEMENT TO ELEM IN REC           
         DC    AL1(20)             TARGET LENGTH OF KEY                         
         DC    AL1(00)             TARGET DISPLACEMENT TO AGENCY ALPHA          
*                                                                               
PIO_0AE1 DC    XL2'0AE1'           PATTERN REC CODE PASSIVE (XSPT)              
         DC    AL1(UNTQ)           TARGET DIR/FILE EQUATE                       
         DC    AL1(20+02+01+04)    TARGET DISPLACEMENT TO ELEM IN REC           
         DC    AL1(20)             TARGET LENGTH OF KEY                         
         DC    AL1(00)             TARGET DISPLACEMENT TO AGENCY ALPHA          
*                                                                               
PIO_0A1D DC    XL2'0A1D'           REVISION REC CODE (XSPT)                     
         DC    AL1(UNTQ)           TARGET DIR/FILE EQUATE                       
         DC    AL1(20+02+01+04)    TARGET DISPLACEMENT TO ELEM IN REC           
         DC    AL1(20)             TARGET LENGTH OF KEY                         
         DC    AL1(00)             TARGET DISPLACEMENT TO AGENCY ALPHA          
*                                                                               
PIO_0A2D DC    XL2'0A2D'           DEALER TAG TEXT REC CODE (XSPT)              
         DC    AL1(SPTQ)           TARGET DIR/FILE EQUATE                       
         DC    AL1(13+02+01+04+04) TARGET DISPLACEMENT TO ELEM IN REC           
         DC    AL1(13)             TARGET LENGTH OF KEY                         
         DC    AL1(13+2+4+1)       TARGET DISPLACEMENT TO AGENCY ALPHA          
*                                                                               
         DC    X'00'               END OF TABLE                                 
*                                                                               
         LTORG                                                                  
         DROP  RB,R6                                                            
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* IO ALL PURPOSE DATAMGR HANDLER                                                
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* I/O CONTROLLER                                                                
*     ON ENTRY   KEY      = CONTAINS KEY FOR DATA MANAGER COMMANDS              
*                                                                               
*     PARAMETERS                                                                
*                P1       = I/O MASK,  COMMAND/FIL/DIR OR SETTINGS              
*                P2       = BYTES 1-3, A(I/O AREA) FOR FILE READ                
*                                                                               
*     AFTER I/O                                                                 
*                KEYSAVE  = KEY PASSED TO ROUTINE                               
*                KEY      = CONTAINS KEY RETURNED BY DATAMGR                    
*                AIO      = A(I/O AREA) FOR RECORD                              
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
IOCALL   NTR1  BASE=*,LABEL=*,WORK=(R6,IOWORKX-IOWORKD)                         
*                                                                               
         USING IOWORKD,R6          LOCAL STORAGE                                
         LM    R2,R3,0(R1)         I/O MASK AND I/O AREA                        
         STC   R2,IOWORK1          LOW ORDER BYTE OF I/O MASK                   
         MVC   IOWORK2,IOWORK1     SAVE LOW ORDER BYTE                          
         STCM  R2,2,IOWORK3        SAVE THIRD BYTE                              
         NI    IOWORK2,X'0F'       ISOLATE COMMAND NUMBER                       
         ZIC   R1,IOWORK2                                                       
         SLL   R1,3                MULTIPLY BY 8 TO DEVELOP INDEX               
         LA    RE,CMNDTAB-L'CMNDTAB(R1)                                         
         MVC   IOCMND,0(RE)        EXTRACT COMMAND NAME                         
         MVI   IODMINBT,0                                                       
         MVC   IOSVAIO,AIO         SAVE AIO POINTER                             
         MVC   IOSVLKEY,LKEY       SAVE ENGTH OF KEY                            
         MVC   IOSVCMMD,COMMAND    SAVE COMMAND                                 
         MVC   IOSVSYSD,SYSDIR     SAVE SYSDIR                                  
         MVC   IOSVSYSF,SYSFIL     SAVE SYSFIL                                  
         MVC   IOSVDDIS,DATADISP   SAVE DISPLACEMENT TO DATA                    
*                                                                               
IO2      MVC   IOWORK2,IOWORK1     REFRESH FLAG VALUES                          
         NI    IOWORK2,X'70'       ISOLATE FILE NUMBER                          
         ZIC   R1,IOWORK2          FILE NUMBER                                  
         LTR   R1,R1                                                            
         BNZ   *+8                                                              
         LHI   R1,XSPQ             IF DIR/FIL NOT PASSED, USE XSPQ              
         SRL   R1,4                DIVIDE BY 8 TO DEVELOP INDEX                 
         LA    R0,L'FILTAB                                                      
         MR    R0,R0                                                            
         LA    RE,FILTAB-L'FILTAB(R1)                                           
         MVC   IODIR,0(RE)         EXTRACT DIRECTORY NAME                       
         MVC   IOFILE,8(RE)        FILE NAME                                    
         MVC   IOVALS,16(RE)       INCLUDING KEY LENGTH                         
         ZIC   R1,IOWORK1                                                       
         EX    R1,*+8                                                           
         B     *+8                                                              
         TM    IOEXCPT,0                                                        
         BZ    *+6                                                              
         DC    H'0'                                                             
         TM    IOWORK3,X'01'       TEST FOR PASS DELETES                        
         BZ    *+8                 NO                                           
         OI    IODMINBT,X'08'                                                   
         TM    IOWORK3,X'02'       TEST FOR READ FOR UPDATE                     
         BZ    *+8                 NO                                           
         OI    IODMINBT,X'80'                                                   
         TM    IOWORK1,DIRQ        TEST FOR DIRECTORY READ                      
         BZ    IO6                                                              
*                                                                               
IO4      DS    0H                                                               
         GOTO1 DATAMGR,DMCB,(IODMINBT,IOCMND),IODIR,KEY,KEY                     
         B     IOX                                                              
*                                                                               
IO6      LTR   R3,R3               AIO PARAMETER PRESENT?                       
         BNZ   *+8                                                              
         L     R3,AIO              USE DEFAULT AIO POINTER                      
         ST    R3,AIO                                                           
         SR    RE,RE                                                            
         IC    RE,IODADSP                                                       
         LA    R2,KEY                                                           
         AR    R2,RE               POINT TO DISK ADDRESS                        
         MVC   IODISKA,0(R2)                                                    
         TM    IOEXCPT,DIRQ        TEST FOR FILE W DIRECTORY                    
         BZ    IO7                                                              
         LA    R2,KEY              FOR IS FILE, POINT TO KEY                    
*                                                                               
IO7      CLI   ACTELOPT,C'N'       NO ACTIVITY ELEM?                            
         BE    IO7_80                                                           
         CLC   =C'PUTREC',IOCMND                                                
         BE    IO7_30                                                           
         CLC   =C'ADDREC',IOCMND                                                
         BNE   IO7_80                                                           
IO7_30   SR    RE,RE                                                            
         IC    RE,IOKEYL                                                        
         STH   RE,LKEY                                                          
         IC    RE,IODDISP                                                       
         STH   RE,DATADISP                                                      
         MVC   COMMAND,IOCMND                                                   
         MVC   SYSDIR,IODIR                                                     
         MVC   SYSFIL,IOFILE                                                    
         CLC   =C'ADDREC',IOCMND                                                
         BNE   IO7_54                                                           
         CLI   KEY,0                                                            
         BNE   *+6                                                              
         DC    H'0'                                                             
         GOTO1 ADDREC                                                           
         B     IO7_90                                                           
IO7_54   GOTO1 PUTREC                                                           
         B     IO7_90                                                           
*                                                                               
IO7_80   GOTO1 DATAMGR,DMCB,(IODMINBT,IOCMND),IOFILE,(R2),AIO,DMWORK            
IO7_90   MVC   IODMOUBT,8(R1)                                                   
         TM    IOEXCPT,DIRQ        TEST FOR IS FILE                             
         BZ    IOX                 NO                                           
         L     RF,AIO                                                           
         ZIC   R1,IOKEYL                                                        
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     IOX                                                              
         MVC   KEY(0),0(RF)        EXTRACT KEY FROM RECORD                      
*                                                                               
IOX      LA    R1,DMCB                                                          
         MVC   AIO,IOSVAIO         RESTORE AIO POINTER                          
         MVC   LKEY,IOSVLKEY       RESTORE LENGTH OF KEY                        
         MVC   COMMAND,IOSVCMMD    RESTORE COMMAND                              
         MVC   SYSDIR,IOSVSYSD     RESTORE SYSTEM DIRECTORY                     
         MVC   SYSFIL,IOSVSYSF     RESTORE SYSTEM FILE                          
         MVC   DATADISP,IOSVDDIS   RESTORE DISPLACEMENT TO DATA                 
         J     XIT_R1                                                           
         LTORG                                                                  
*                                                                               
CMNDTAB  DS    0CL8                TABLE OF I/O COMMANDS                        
         DC    CL8'DMREAD  '                                                    
         DC    CL8'DMRSEQ  '                                                    
         DC    CL8'DMRDHI  '                                                    
         DC    CL8'DMWRT   '                                                    
         DC    CL8'DMADD   '                                                    
         DC    CL8'GETREC  '                                                    
         DC    CL8'PUTREC  '                                                    
         DC    CL8'ADDREC  '                                                    
*                                                                               
FILTAB   DS    0XL20               TABLE OF FIL/DIR AND ATTRIBUTES              
         DC    CL8'SPTDIR  ',CL8'SPTFILE ',AL1(0000,0013,0014,0024)             
         DC    CL8'UNTDIR  ',CL8'UNTFILE ',AL1(0000,0020,0021,0027)             
         DC    CL8'STATION ',CL8'STATION ',AL1(DIRQ,0017,0000,0000)             
         DC    CL8'TRFDIR  ',CL8'TRFFILE ',AL1(0000,0013,0014,0000)             
         DC    CL8'NTIDIR  ',CL8'NTIFILE ',AL1(0000,0018,0019,0000)             
         DC    CL8'CTFILE  ',CL8'CTFILE  ',AL1(DIRQ,0025,0000,0000)             
         DC    CL8'XSPDIR  ',CL8'XSPFIL  ',AL1(0000,0032,0036,0042)             
*                                                                               
         DROP  RB,R6                                                            
*                                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
IOWORKD  DSECT                     IOCALL ROUTINE LOCAL STORAGE                 
IOWORK   DS    CL96                FOR GETREC, PUTREC, AND ADDREC               
IOWORK1  DS    X                                                                
IOWORK2  DS    X                                                                
IOWORK3  DS    X                                                                
IOCMND   DS    CL8                 DATA MANAGER COMMAND                         
IODIR    DS    CL8                 DIRECTORY NAME                               
IOFILE   DS    CL8                 FILE NAME                                    
*                                                                               
IOVALS   DS    0XL4                                                             
IOEXCPT  DS    X                   EXCEPTION VALUES FOR FILE                    
IOKEYL   DS    X                   KEY LENGTH                                   
IODADSP  DS    X                   DISPLACEMENT TO DISK ADDRESS                 
IODDISP  DS    X                   DISPLACEMENT TO DATA                         
*                                                                               
IODMINBT DS    X                   DATA MANAGER IN BITS                         
IODMOUBT DS    X                   DATA MANAGER OUT BITS                        
IODISKA  DS    XL4                 HELD DISK ADDRESS FOR IO ROUTINE             
*                                                                               
IOSVAIO  DS    XL4                 SAVE AIO                                     
IOSVLKEY DS    XL(L'LKEY)          SAVE ENGTH OF KEY                            
IOSVCMMD DS    XL(L'COMMAND)       SAVE COMMAND                                 
IOSVSYSD DS    XL(L'SYSDIR)        SAVE SYSTEM DIRECTORY                        
IOSVSYSF DS    XL(L'SYSFIL)        SAVE SYSTEM FILE                             
IOSVFLNM DS    XL(L'FILENAME)      SAVE FILENAME                                
IOSVDDIS DS    XL(L'DATADISP)      SAVE DISPLACEMENT TO DATA                    
*                                                                               
IOWORKX  EQU   *                                                                
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
PRCIOWKD DSECT                     PROCESSING I/O                               
PRCAAIO1 DS    A                                                                
PRCSVAIO DS    F                   SAVE AIO                                     
PRCIPAR1 DS    6F                  PARAMETER LIST BEFORE I/O                    
PRCIPAR2 DS    6F                  PARAMETER LIST AFTER I/O                     
PRCIPARX DS    6F                  PARAMETER LIST FOR TEMP SAVING               
PRCISVRE DS    F                   SAVE RETURN REGISTER                         
PRCISVR2 DS    F                   SAVE RETURN REGISTER                         
PRCISVDA DS    A                   SAVE DISK ADDRESS FOR GETREC                 
PRCSVDIS DS    XL(L'DATADISP)      SAVE ORIGINAL DATA DISPLACEMENT              
PRCIKEY1 DS    XL(L'KEY)           SAVE KEY BEFORE I/O                          
PRCIKEY2 DS    XL(L'KEY)           SAVE KEY AFTER I/O                           
PRCIUNTK DS    XL(L'KEY)           SAVE UNIT REC KEY                            
PRCIXPRD DS    CL3                 ALPHA NUMERIC PRD CODE                       
PRCIBPRD DS    X                   BINARY PRD CODE FOUND                        
PRCIINBT DS    X                   SAVE CALLER'S DATAMGR INPUT BITS             
PRCICMND DS    X                   SAVE CALLER'S DATAMGR COMMAND EQUATE         
PRCISVFL DS    X                   SAVE FILE EQUATE FROM CALLER                 
PRCIFILE DS    X                   SAVE FILE EQUATE FROM TABLE                  
PRCIDISP DS    X                   SAVE REC DISPLACEMENT FROM TABLE             
PRCIKEYL DS    X                   SAVE LENGTH OF KEY FOR TARGET REC            
PRCIDALP DS    X                   SAVE DISPLACEMENT TO AGENCY ALPHA            
PRCIBYT1 DS    X                   WORKING BYTE1                                
PRCIBYT2 DS    X                   WORKING BYTE2                                
PRCIBYT3 DS    X                   WORKING BYTE3                                
*                                                                               
PRCIFLAG DS    X                   FLAG                                         
XSPTRCNQ EQU   X'80'               XSPOT RECORD IS CONVERTED                    
*                                                                               
PRKEYSAV DS    CL(L'KEYSAVE)                                                    
PRKEYTMP DS    CL(L'KEYSAVE)                                                    
*                                                                               
PRCIAIO1 DS    XL4096                                                           
*                                                                               
PRCIOWKX EQU   *                                                                
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
       ++INCLUDE SPSTAPACKD                                                     
         EJECT                                                                  
       ++INCLUDE DDSPOOLD                                                       
         EJECT                                                                  
       ++INCLUDE DDSPLWORKD                                                     
         EJECT                                                                  
       ++INCLUDE SPTRAFFD                                                       
         EJECT                                                                  
                                                                                
         ORG CONTAGH                                                            
       ++INCLUDE SPTRAE0D                                                       
         PRINT OFF                                                              
                                                                                
         ORG CONTAGH                                                            
*PREFIX=SP$                                                                     
       ++INCLUDE SPTRAF3D                                                       
*PREFIX=                                                                        
                                                                                
         ORG CONTAGH                                                            
*PREFIX=CX$                                                                     
       ++INCLUDE SPTRAE1D                                                       
*PREFIX=                                                                        
                                                                                
         ORG CONTAGH                                                            
*PREFIX=CL$                                                                     
       ++INCLUDE SPTRAD1D                                                       
*PREFIX=                                                                        
                                                                                
         ORG CONTAGH                                                            
*PREFIX=PG$                                                                     
       ++INCLUDE SPTRA85D                                                       
*PREFIX=                                                                        
                                                                                
         ORG CONTAGH                                                            
*PREFIX=CG$                                                                     
       ++INCLUDE SPTRA73D                                                       
*PREFIX=                                                                        
                                                                                
         PRINT ON                                                               
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
       ++INCLUDE SPTRAWORKD                                                     
       ++INCLUDE DMPRTQL                                                        
         EJECT                                                                  
         PRINT OFF                                                              
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
         EJECT                                                                  
PRDHDRD  DSECT                                                                  
       ++INCLUDE SPGENPRD                                                       
         EJECT                                                                  
AGYHDRD  DSECT                                                                  
       ++INCLUDE SPGENAGY                                                       
         EJECT                                                                  
MKTRECD  DSECT                                                                  
       ++INCLUDE SPGENMKT                                                       
         EJECT                                                                  
STARECD  DSECT                                                                  
       ++INCLUDE SPGENSTA                                                       
         EJECT                                                                  
ESTHDRD  DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
         EJECT                                                                  
       ++INCLUDE SPGENPRG                                                       
         EJECT                                                                  
* AVOID DUPLICATE NAMES FROM THIS RECORD AND SPGENPRG                           
*PREFIX=N$ =SPTRNPRG                                                            
       ++INCLUDE SPTRKEYS                                                       
*PREFIX=                                                                        
         EJECT                                                                  
       ++INCLUDE SPGENGRP                                                       
         EJECT                                                                  
       ++INCLUDE NEGENDPT                                                       
         PRINT OFF                                                              
         EJECT                                                                  
       ++INCLUDE CTGENFILE                                                      
         EJECT                                                                  
       ++INCLUDE DDCOMFACS                                                      
         EJECT                                                                  
       ++INCLUDE FASECRETD                                                      
         EJECT                                                                  
       ++INCLUDE FAFACTS                                                        
         EJECT                                                                  
       ++INCLUDE FASYSLSTD                                                      
         EJECT                                                                  
       ++INCLUDE DDOFFICED                                                      
         EJECT                                                                  
       ++INCLUDE DDCOREQUS                                                      
         EJECT                                                                  
       ++INCLUDE FALOCKETD                                                      
         EJECT                                                                  
DDNAMED  DSECT                                                                  
       ++INCLUDE DMDDNAMED                                                      
         EJECT                                                                  
MASTD    DSECT                                                                  
       ++INCLUDE DDMASTC                                                        
         EJECT                                                                  
       ++INCLUDE FATIOB                                                         
         EJECT                                                                  
       ++INCLUDE FAGETTXTD                                                      
         EJECT                                                                  
       ++INCLUDE SPSTABLK                                                       
         EJECT                                                                  
       ++INCLUDE DMDTFIS                                                        
       ++INCLUDE DDGLOBEQUS                                                     
       ++INCLUDE DDGLVXCTLD                                                     
       ++INCLUDE DDGLPFMD                                                       
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'214SPTRA00   04/17/19'                                      
         END                                                                    
