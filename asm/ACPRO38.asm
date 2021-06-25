*          DATA SET ACPRO38    AT LEVEL 097 AS OF 04/11/07                      
*PHASE T60B38A                                                                  
*INCLUDE KHDUMMY                                                                
         TITLE 'T60B38 - TEXT MAINTENANCE/DELETE'                               
T60B38   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T60B38**,R8,RR=R2                                              
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R9,ASUBSYSD                                                      
         USING SUBSYSD,R9          SYSTEM SPECIFIC WORK AREA                    
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA                                                   
         ST    R2,RELO                                                          
         SR    R2,R2                                                            
*                                                                               
         CLI   MODE,VALKEY                                                      
         BE    KEYLOGIC                                                         
         CLI   MODE,VALREC                                                      
         BE    RECLOGIC                                                         
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* LOGIC TO CLEAR STORAGE, VALIDATE THE SCREEN, BRANCH TO DELETE       *         
* ROUTINE AND SET INTMODE.                                            *         
***********************************************************************         
*                                                                               
KEYLOGIC LA    RE,LOCAL            CLEAR LOCAL STORAGE                          
         LA    RF,LOCALLN                                                       
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
         BAS   RE,VALHED           VALIDATE SCREEN                              
*                                                                               
         CLI   ACTNUM,ACTDEL       IS THE ACTION FOR DELETE ?                   
         BE    DELLOGIC            YES                                          
*                                                                               
         MVI   INTMODE,DISMODE     NO, INITIALIZE INTERNAL MODE                 
         CLI   RACHANGE,C'Y'       TEST FOR RECORD/ACTION CHANGE                
         BNE   *+8                                                              
         MVI   INTMODE,FSTMODE     YES - SET FIRST TIME LIST                    
         CLI   KEYCHG,C'Y'         TEST FOR CHANGE IN KEY FIELDS                
         BNE   *+8                                                              
         MVI   INTMODE,FSTMODE     YES - SET FIRST TIME LIST                    
*                                                                               
         CLI   ERRSWT,C'Y'         ARE WE BACK WITH PENDING ERROR ?             
         BE    KEYL02              YES                                          
         CLI   PFSWT,C'Y'          NO, ARE WE HERE BECAUSE OF A PFKEY ?         
         BNE   KEYL04              NO                                           
*                                                                               
KEYL02   BAS   RE,PROCPF           YES, SEE IF RESOLVED                         
         BNE   XIT                                                              
*                                                                               
KEYL04   BAS   RE,CHKREC           CHECK FOR CONFLICTS                          
*                                                                               
         CLI   INTMODE,FSTMODE                                                  
         BNE   *+8                                                              
         BAS   RE,BLDTWA           YES, CHANGE OR 1ST TIME                      
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* LOGIC TO TEST IF FIRST TIME THROUGH AND DISPLAY DATA ON SCREEN.     *         
* IF NOT FIRST TIME, CHECK FOR PFKEYS AND CHANGES TO BE EDITED.       *         
***********************************************************************         
*                                                                               
RECLOGIC BAS   RE,SETFLDS          SET FIRST/LAST FIELD ADDRESSES               
         CLI   INTMODE,FSTMODE     FIRST TIME THROUGH ?                         
         BNE   RECL040             NO                                           
*                                                                               
RECL020  BAS   RE,DISLOGIC         YES,JUST DO DISPLAY                          
         B     XIT                                                              
*                                                                               
RECL040  BAS   RE,PROCPF           HANDLE PFKEYS, IF ANY                        
         BNE   RECL020                                                          
         BAS   RE,TSTEDT           ANY CHANGES TO EDIT ?                        
         BNE   RECL020             NO, JUST DISPLAY                             
         BAS   RE,EDTLOGIC         YES, UPDATE AND DISPLAY                      
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* ROUTINES TO DELETE TEXT RECORDS.                                    *         
***********************************************************************         
*                                                                               
DELLOGIC BAS   RE,READTXT          READ THE TEXT RECORD                         
         BNE   NORECORD                                                         
         USING ACKEYD,R6                                                        
         OI    ACSTATUS,X'80'      SET DELETED BIT                              
         BAS   RE,WRITEIT          REWRITE THE RECORD                           
         MVI   ERROR,X'FE'                                                      
         LA    R2,TXTOGRH                                                       
         OI    GENSTAT2,USMYOK                                                  
         OI    CONHEADH+1,X'08'                                                 
         MVC   CONHEAD(L'DELMSG),DELMSG                                         
         GOTO1 ERREX2                                                           
         SPACE 3                                                                
NORECORD MVI   ERROR,NOTFOUND      RECORD MUST BE THERE IN ORDER                
         LA    R2,TXTOGRH                                                       
         B     ERREXIT              TO DELETE IT                                
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* SUBROUTINE TO VALIDATE HEADING FIELDS.                              *         
***********************************************************************         
*                                                                               
VALHED   NTR1                                                                   
         LA    R2,CONRECH                                                       
         GOTO1 SETHEIR                                                          
         MVI   KEYCHG,C'N'                                                      
         MVI   OPTION,0                                                         
         MVI   PFSWT,C'N'                                                       
*                                                                               
         XC    QKEY,QKEY                                                        
         LA    R2,TXTOGRH          OFFICE GROUP IS OPTIONAL                     
         BAS   RE,TSTKEY                                                        
         CLI   5(R2),0                                                          
         BE    VALH020                                                          
         GOTO1 VALOG                                                            
         MVC   QOFG,EFFOFG                                                      
*                                                                               
VALH020  LA    R2,TXTOFFH          OFFICE IS OPTIONAL                           
         BAS   RE,TSTKEY                                                        
         CLI   5(R2),0                                                          
         BE    VALH040                                                          
         MVI   ERROR,NOTOFNOG                                                   
         CLI   TXTOGRH+5,0                                                      
         BNE   ERREXIT                                                          
         GOTO1 VALOFF                                                           
         MVC   QOFF,EFFOFFC                                                     
*                                                                               
VALH040  LA    R2,TXTCLIH          CLIENT IS OPTIONAL                           
         BAS   RE,TSTKEY                                                        
         CLI   5(R2),0                                                          
         BE    VALH060                                                          
         MVI   ERROR,NOTCLNOG                                                   
         CLI   TXTOGRH+5,0                                                      
         BNE   ERREXIT                                                          
         MVI   ERROR,NOTCLNOF                                                   
         CLI   TXTOFFH+5,0                                                      
         BNE   ERREXIT                                                          
         GOTO1 VALCLI                                                           
         MVC   QCLI,CLICODE                                                     
*                                                                               
VALH060  LA    R2,TXTPROH          PRODUCT IS OPTIONAL                          
         BAS   RE,TSTKEY                                                        
         CLI   5(R2),0                                                          
         BE    VALH080                                                          
         MVI   ERROR,NEEDCLI                                                    
         CLI   TXTCLIH+5,0                                                      
         BE    ERREXIT                                                          
         GOTO1 VALPROD                                                          
         MVC   QPROD,PRODCODE                                                   
*                                                                               
VALH080  LA    R2,TXTJOBH          JOB IS OPTIONAL                              
         BAS   RE,TSTKEY                                                        
         CLI   5(R2),0                                                          
         BE    VALH100                                                          
         MVI   ERROR,NEEDPRO                                                    
         CLI   TXTPROH+5,0                                                      
         BE    ERREXIT                                                          
         GOTO1 VALJOB                                                           
         MVI   ELCODE,ACJBELQ      GET JOB ELEMENT                              
         BAS   RE,GETELIO                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   TXTFORM,C'B'        IF FORM CODE IS BILL                         
         BE    VALH090             THEN DO NOT TEST ESTIMATE TYPE               
*                                                                               
         USING ACJOBD,R6                                                        
         MVI   ERROR,BOESTERR                                                   
         TM    ACJBSTAT,JOBSMCSE                                                
         BO    ERREXIT                                                          
*                                                                               
         MVI   ERROR,OLDESERR                                                   
         TM    ACJBSTAT,ACJBNEWQ                                                
         BNO   ERREXIT                                                          
*                                                                               
VALH090  MVC   QJOB,JOBNUM                                                      
*                                                                               
VALH100  LA    R2,TXTCATH          CATEGORY IS OPTIONAL                         
         BAS   RE,TSTKEY                                                        
         CLI   5(R2),0                                                          
         BE    VALH120                                                          
         MVI   ERROR,NEEDJOB                                                    
         CLI   TXTJOBH+5,0                                                      
         BE    ERREXIT                                                          
         GOTO1 ANY                                                              
         MVC   QCAT,WORK                                                        
*                                                                               
VALH120  LA    R2,TXTWRKH          WORKCODE IS OPTIONAL                         
         BAS   RE,TSTKEY                                                        
         CLI   5(R2),0                                                          
         BE    VALH140                                                          
         MVI   ERROR,NOTCTNWK                                                   
         CLI   TXTCATH+5,0                                                      
         BNE   ERREXIT                                                          
         MVI   ERROR,NEEDJOB                                                    
         CLI   TXTJOBH+5,0                                                      
         BE    ERREXIT                                                          
         GOTO1 VALWORK                                                          
         MVC   QWORK,WORKCODE                                                   
         MVC   QSUFFIX,WORK+2                                                   
*                                                                               
         MVI   ERROR,SUFFIX        VALIDATE SUFFIX FOR BLANK, C OR N            
         MVI   ERRNDX,X'02'                                                     
         CLI   QSUFFIX,C'C'                                                     
         BE    VALH140                                                          
         CLI   QSUFFIX,C'N'                                                     
         BE    VALH140                                                          
         CLI   QSUFFIX,C' '                                                     
         BNE   ERREXIT                                                          
         MVI   QSUFFIX,0           NOTE SUFFIX AS ZERO INTERNALLY               
*                                                                               
VALH140  LA    R2,TXTMGRH          MEDIA GROUP IS OPTIONAL                      
         BAS   RE,TSTKEY                                                        
         CLI   5(R2),0                                                          
         BE    VALH180                                                          
         MVI   ERROR,NOTJBNME                                                   
         CLI   TXTJOBH+5,0                                                      
         BNE   ERREXIT                                                          
*                                                                               
VALH160  GOTO1 VALMG                                                            
         MVC   QMGR,MGROUP                                                      
*                                                                               
VALH180  LA    R2,TXTMEDH          MEDIA IS OPTIONAL                            
         BAS   RE,TSTKEY                                                        
         CLI   5(R2),0                                                          
         BE    VALH220                                                          
         MVI   ERROR,NOTJBNME                                                   
         CLI   TXTJOBH+5,0                                                      
         BNE   ERREXIT                                                          
         MVI   ERROR,NOTMENMG                                                   
         CLI   TXTMGRH+5,0                                                      
         BNE   ERREXIT                                                          
*                                                                               
VALH200  GOTO1 VALMED                                                           
         MVC   QMED,MEDIA                                                       
*                                                                               
VALH220  LA    R2,TXTFORMH         FORM IS E IF NOT ENTERED                     
         BAS   RE,TSTKEY                                                        
         CLI   5(R2),0                                                          
         BNE   VALH222                                                          
         MVI   8(R2),C'E'                                                       
         MVI   5(R2),X'01'                                                      
         OI    6(R2),X'80'                                                      
         NI    4(R2),X'FF'-X'20'                                                
*                                                                               
VALH222  GOTO1 ANY                                                              
         MVC   QFORM,WORK                                                       
         MVI   ERROR,INVALID                                                    
         CLI   QFORM,C'P'                                                       
         BE    VALH240                                                          
         CLI   QFORM,C'E'                                                       
         BE    VALH240                                                          
         CLI   QFORM,C'B'                                                       
         BNE   ERREXIT                                                          
         LA    R2,TXTCATH          CATEGORY IS NOT VALID                        
         CLI   5(R2),0                                                          
         BNE   ERREXIT                                                          
         LA    R2,TXTWRKH          WORKCODE IS NOT VALID                        
         CLI   5(R2),0                                                          
         BNE   ERREXIT                                                          
*                                                                               
VALH240  LA    R2,TXTWHERH         WHERE IS REQUIRED FOR DELETE                 
         BAS   RE,TSTKEY                                                        
         CLI   ACTNUM,ACTDEL                                                    
         BE    VALH243                                                          
         CLI   5(R2),0             WAS ANYTHING ENTERED ?                       
         BNE   VALH242             YES                                          
*                                                                               
VALH241  MVI   PFKEY,PF8                                                        
         MVI   PFSWT,C'Y'                                                       
         B     VALH280                                                          
*                                                                               
VALH242  CLI   KEYCHG,C'Y'         DID ANYTHING CHANGE ?                        
         BNE   VALH243                                                          
         TM    4(R2),X'80'         WAS WHERE ENTERED THIS TIME ?                
         BO    VALH243             YES                                          
         MVI   8(R2),0                                                          
         B     VALH241                                                          
*                                                                               
VALH243  GOTO1 ANY                                                              
         MVC   QWHER,WORK                                                       
         MVI   ERROR,INVALID                                                    
         LR    RE,R2                                                            
         SR    R1,R1                                                            
         IC    R1,5(R2)                                                         
         BCTR  R1,0                                                             
         LTR   R1,R1                                                            
         BZ    VALH260                                                          
         CLI   TXTCATH+5,0         CAN ONLY HAVE A 1-BYTE ENTRY IF              
         BNE   ERREXIT              CATEGORY OR WORKCODE ENTERED                
         CLI   TXTWRKH+5,0                                                      
         BNE   ERREXIT                                                          
         CLI   8(RE),C'S'                                                       
         BNE   ERREXIT                                                          
         LA    RE,1(RE)                                                         
         MVC   QWHER(1),WORK+1     SWITCH CHARACTERS AROUND                     
         MVC   QWHER+1(1),WORK                                                  
*                                                                               
VALH260  CLI   8(RE),C'H'                                                       
         BE    VALH280                                                          
         CLI   8(RE),C'F'                                                       
         BE    VALH280                                                          
         CLI   TXTCATH+5,0         HEADER OR FOOTER ONLY VALID ENTRIES          
         BNE   ERREXIT              IF CATEGORY OR WORKCODE ENTERED             
         CLI   TXTWRKH+5,0                                                      
         BNE   ERREXIT                                                          
         CLI   8(RE),C'T'                                                       
         BE    VALH280                                                          
         MVI   ERRNDX,X'01'                                                     
         B     ERREXIT                                                          
*                                                                               
VALH280  LA    R2,TXTCOMMH         STANDARD COMMENTS IS OPTIONAL                
         BAS   RE,TSTKEY                                                        
         CLI   5(R2),0                                                          
         BE    VALH300                                                          
         GOTO1 ANY                                                              
         MVC   QCOMMENT,BLANKS                                                  
         LA    R3,QCOMMENT+6                                                    
         SR    R1,R1                                                            
         IC    R1,5(R2)                                                         
         SR    R3,R1                                                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),8(R2)                                                    
*                                                                               
VALH300  BAS   RE,SETGO                                                         
         GOTO1 GETOPT,DMCB,GOBLOCK                                              
         MVC   GOTSCRL,GOTXTSCR    SAVE TEXT SCROLL LIST                        
         MVI   GOTSCRLE,X'00'      MAKE END OF TABLE                            
         CLI   KEYCHG,C'Y'         DID SOMETHING CHANGE ?                       
         BNE   VALH340             NO, EXIT                                     
         CLI   PFSWT,C'Y'          DID WE HAVE A MISSING FIELD ?                
         BE    VALH340                                                          
         LA    R2,TXTCATH                                                       
         CLI   5(R2),0                                                          
         BE    VALH320                                                          
         MVC   SCHEME,GOSCHEME                                                  
         GOTO1 VALCAT                                                           
         MVC   QCAT,CATEGORY                                                    
*                                                                               
VALH320  MVI   ERRSWT,C'N'         CLEAR ERROR SWITCH                           
         MVI   PFSWT,C'N'          CLEAR ERROR SWITCH                           
*                                                                               
VALH340  BAS   RE,LOAD             LOAD UP SCREEN BUFFER                        
         L     RE,ABUFFER          CLEAR IT                                     
         LA    RF,2304                                                          
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
VALHEDX  OC    QOFG(QREQLEN),QOFG  WAS REQUIRED DATA ENTERED ?                  
         BNZ   XIT                 YES                                          
         MVI   ERROR,MISSING       NO, PRINT MESSAGE                            
         LA    R2,TXTOGRH                                                       
         B     ERREXIT                                                          
         SPACE 3                                                                
TSTKEY   TM    4(R2),X'20'         HAS FIELD CHANGED ?                          
         BOR   RE                  NO, EXIT                                     
         OI    4(R2),X'20'         YES, INDICATE VALIDATED                      
         MVI   KEYCHG,C'Y'          AND SET SWITCH                              
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* SUBROUTINE TO READ PANEL RECORDS AND BUILD SCREEN.                  *         
***********************************************************************         
*                                                                               
BLDTWA   NTR1                                                                   
*                                                                               
         LA    R7,TWAPARM          INITIALIZE TWABLD PARAMETERS                 
         USING TWAPARMD,R7                                                      
         XC    TWAPARM,TWAPARM                                                  
*                                                                               
         CLC   QPANEL,DEFPAN       DO WE NEED COMMENT SCREEN ?                  
         BE    BLDCOM              YES                                          
*                                                                               
         LA    RE,KEYTAB           CLEAR KEYWORD TABLE                          
         LA    RF,KEYLEN                                                        
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
         MVI   NKEYS,0                                                          
*                                                                               
         L     RE,ABUFFER          COPY SCREEN HEADER                           
         LA    RF,CONTAGH-T60BFFD                                               
         LR    R1,RF                                                            
         L     R0,ATWA                                                          
         MVCL  RE,R0                                                            
*                                                                               
         MVC   DMCB+4(3),SYSPHASE  COPY TEXT MAINT HEADER NOW                   
         MVI   DMCB+7,X'C8'                                                     
         L     RE,ABUFFER                                                       
         LA    RE,CONTAGH-T60BFFD(RE)                                           
         ST    RE,DMCB                                                          
         GOTO1 CALLOV,DMCB                                                      
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     RE,ABUFFER                                                       
         ST    RE,TWAPATWA                                                      
         LA    RE,TXTTAGH-T60BFFD(RE)                                           
         ST    RE,TWAPAOUT                                                      
         MVC   TWAPAMAX,=F'2304'                                                
*                                                                               
         MVC   HALF,2(RE)          EXTRACT DISPL TO SCREEN START                
         LH    RF,HALF                                                          
         BCTR  RF,0                BACK UP ONE POSITION                         
         STH   RF,LASTEND                                                       
         SR    RE,RE                                                            
         D     RE,=F'80'                                                        
         LA    RF,1(RF)            ROW                                          
         LA    RE,1(RE)            COLUMN                                       
         STC   RF,LASTROW                                                       
         STC   RE,LASTCOL                                                       
*                                                                               
         LA    R3,0                INITIAL SEQUENCE NUMBER                      
*                                                                               
         LA    R2,TXTPANH                                                       
         LA    R6,KEY                                                           
         USING ACPNKEY,R6                                                       
         XC    ACPNKEY,ACPNKEY                                                  
         MVI   ACPNRTYP,ACPNEQU                                                 
         MVI   ACPNSREC,ACPNSEQU                                                
         MVC   ACPNCUL,CUL                                                      
         MVC   ACPNCODE,QPANEL                                                  
         MVI   ACPNNUM,0                                                        
         GOTO1 READ                MUST HAVE HEADER RECORD                      
*                                                                               
         MVI   ACPNNUM,1           GET FIELD DATA RECORD(S)                     
         GOTO1 HIGH                                                             
         MVI   ERROR,NOPANEL                                                    
         CLC   KEY(ACPNNUM-ACPNKEY),KEYSAVE                                     
         BNE   ERREXIT                                                          
*                                                                               
BLDT020  MVI   ELCODE,ACFDELQ      GET PANEL DATA ELEMENT                       
         BAS   RE,GETELIO                                                       
         BE    BLDT060                                                          
         DC    H'0'                MUST HAVE AT LEAST 1 ELEMENT                 
*                                                                               
BLDT040  BAS   RE,NEXTEL                                                        
         BNE   BLDT220                                                          
*                                                                               
         USING ACFDD,R6                                                         
BLDT060  CLI   ACFDSEQ,X'FF'                                                    
         BE    BLDT220                                                          
         CLI   ACFDSROW,X'00'                                                   
         BE    BLDT040                                                          
*                                                                               
         LA    R3,1(R3)            INCREMENT SEQUENCE #                         
*                                                                               
         LA    R4,TWAELEM                                                       
         USING TWAELEMD,R4                                                      
         XC    TWAELEM,TWAELEM                                                  
         MVI   TWAELCD,X'01'                                                    
         MVI   TWAELLN,TWAELLNQ                                                 
*                                                                               
         SR    RE,RE                                                            
         IC    RE,ACFDLEN          RE=ELEMENT LENGTH                            
*                                                                               
         SR    RF,RF                                                            
         IC    RF,ACFDPDLN         RF=PRINT DATA LENGTH                         
*                                                                               
         SR    RE,RF                                                            
         LA    RF,ACFDSCR-ACFDD                                                 
         CR    RE,RF               ANY SCREEN DATA ?                            
         BE    BLDT080             NO                                           
*                                                                               
         SR    RE,RF               RE=L'SCREEN DATA                             
         LR    R1,RE               SAVE IT                                      
         LA    RE,TWAELLNQ(RE)     COMPUTE ELEMENT LENGTH                       
         STC   RE,TWAELLN                                                       
*                                                                               
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   TWAEDTA(0),ACFDSCR                                               
*                                                                               
BLDT080  MVC   TWAEATB,ACFDATTR    ATTRIBUTE BYTE                               
         MVC   TWAEFLN,ACFDSLEN    SCREEN LENGTH                                
*                                                                               
         MVC   THISCOL,ACFDSCOL    EXTRACT THIS FIELD'S COLUMN                  
         TM    ACFDSROW,X'80'      AUTO ROW FEATURE ?                           
         BO    BLDT100             YES                                          
         TM    ACFDSROW,X'40'      PLUS ROW FEATURE ?                           
         BO    BLDT120             YES                                          
*                                                                               
         MVC   THISROW,ACFDSROW    NO, EXTRACT ROW                              
         B     BLDT140                                                          
*                                                                               
BLDT100  SR    R1,R1                                                            
         IC    R1,LASTROW          AUTO ROW FEATURE                             
         CLC   THISCOL,LASTCOL     THIS COLUMN > LAST COLUMN ?                  
         BH    *+8                 YES, MUST BE ON SAME ROW AS LAST ONE         
         LA    R1,1(R1)                                                         
         STC   R1,THISROW                                                       
         B     BLDT140                                                          
*                                                                               
BLDT120  SR    R1,R1                                                            
         IC    R1,LASTROW          PLUS ROW FEATURE                             
         MVC   BYTE,ACFDSROW                                                    
         NI    BYTE,X'FF'-X'40'    TURN OFF PLUS BIT                            
*                                                                               
         SR    R0,R0                                                            
         IC    R0,BYTE                                                          
         AR    R1,R0               ADD IN +N ROWS                               
         STC   R1,THISROW                                                       
*                                                                               
BLDT140  MVC   TWAECOL,THISCOL     FINISH BUILDING ELEMENT                      
         SR    R1,R1                                                            
         IC    R1,THISROW                                                       
*                                                                               
         SR    R0,R0                                                            
         IC    R0,LASTROW                                                       
         SR    R1,R0               COMPUTE RELATIVE LINE NUMBER                 
         STC   R1,TWAERLN                                                       
*                                                                               
         SR    RE,RE                                                            
         IC    RE,THISROW                                                       
         BCTR  RE,0                                                             
         MH    RE,=H'80'           GET SCREEN DISPLACEMENT TO FIELD             
*                                                                               
         SR    RF,RF                                                            
         IC    RF,THISCOL                                                       
         AR    RE,RF                                                            
         BCTR  RE,0                                                             
         CH    RE,LASTEND          IS DISPLACEMENT BEFORE PREVIOUS ?            
         BL    SCRERR              YES, ERROR                                   
         BH    BLDT160             NO                                           
*                                                                               
         TM    TWAEATB,X'20'       UNPROTECTED FIELDS CAN OVERLAP               
         BO    SCRERR                                                           
         TM    LASTATTB,X'20'                                                   
         BNZ   SCRERR                                                           
*                                                                               
BLDT160  SR    RF,RF                                                            
         IC    RF,TWAEFLN          GET FIELD LENGTH                             
         TM    TWAEATB,X'20'       IS FIELD PROTECTED ?                         
         BO    *+8                 YES                                          
         LA    RE,2(RE)            NO, ADD IN BEFORE & AFTER BYTES              
         AR    RE,RF                                                            
         BCTR  RE,0                NEW END OF LAST FIELD                        
         STH   RE,LASTEND                                                       
         MVC   LASTATTB,TWAEATB    SAVE ATTRIBUTE BYTE                          
         MVC   LASTROW(2),THISROW                                               
         CH    RE,=Y(23*80)        DOES SCREEN END BEFORE LINE 24 ?             
         BNL   SCRERR              NO, ERROR                                    
*                                                                               
BLDT180  OC    ACFDFLD,ACFDFLD     IS THERE A KEYWORD FIELD ?                   
         BZ    BLDT200             NO                                           
         BAS   RE,DUPKEY           YES, DOES KEYWORD ALREADY EXIST ?            
         BE    SCRERR              YES, ERROR                                   
*                                                                               
BLDT200  ST    R4,TWAPAFST         SET TWABLD ELEMENT ADDRESS                   
         BAS   RE,BUILDIT          BUILD SCREEN                                 
         MVC   TWAPAOUT,TWAPANXT   GET NEXT AVAILABLE ADDRESS                   
         B     BLDT040                                                          
*                                                                               
BLDT220  GOTO1 SEQ                 GET NEXT PANEL RECORD                        
         L     R4,AIO                                                           
         USING ACPNKEY,R4                                                       
         CLC   ACPNKEY(ACPNNUM-ACPNKEY),KEYSAVE                                 
         BE    BLDT020             GOT ANOTHER PANEL RECORD, GET DATA           
*                                                                               
BLDTX    STC   R3,NUMFLDS                                                       
         LA    R4,PFDATA           BUILD PFKEY LINE NOW                         
         USING TWAELEMD,R4                                                      
         LA    R1,PFLINE                                                        
         SR    R0,R0                                                            
         IC    R0,LASTROW                                                       
         SR    R1,R0                                                            
         STC   R1,TWAERLN                                                       
         ST    R4,TWAPAFST                                                      
         BAS   RE,BUILDIT                                                       
*                                                                               
         L     RE,ATWA             MOVE FROM STORAGE TO TWA                     
         LA    RE,TXTTAGH-T60BFFD(RE)                                           
*                                                                               
         L     RF,=AL4(QKEY-TXTWORK)                                            
         LR    R1,RF               SET FROM & TO LENGTHS                        
*                                                                               
         L     R4,ABUFFER                                                       
         LA    R4,TXTTAGH-T60BFFD(R4)                                           
         LR    R0,R4                                                            
*                                                                               
         MVCL  RE,R0                                                            
*                                                                               
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* SUBROUTINE TO BUILD COMMENT SCREEN AND HEADING.                     *         
***********************************************************************         
*                                                                               
BLDCOM   MVI   ACTSTAT,X'80'       INDICATE 'ACT' PROTECTED                     
         MVC   TWAPATWA,ATWA                                                    
         MVC   TWAPAMAX,=AL4(QKEY-T60BFFD)                                      
         LA    R2,TXTTAGH                                                       
         ST    R2,TWAPAOUT                                                      
         LA    R4,COMTITL          DO HEADING FIRST                             
         ST    R4,TWAPAFST                                                      
         BAS   RE,BUILDIT                                                       
*                                                                               
         LA    R4,COMDATA          DO DATA LINES NOW                            
         ST    R4,TWAPAFST                                                      
         LA    R0,MAXLINE                                                       
*                                                                               
BLDC020  L     R2,TWAPANXT                                                      
         ST    R2,TWAPAOUT                                                      
         BAS   RE,BUILDIT                                                       
         BCT   R0,BLDC020                                                       
*                                                                               
         L     R2,TWAPANXT         DO PF LINE NOW                               
         ST    R2,TWAPAOUT                                                      
         LA    R4,COMMPF                                                        
         USING TWAELEMD,R4                                                      
         ST    R4,TWAPAFST                                                      
         BAS   RE,BUILDIT                                                       
*                                                                               
BLDCOMX  MVI   NUMFLDS,(MAXLINE*2)   SET NUMFLDS TO MAXIMUM                     
         B     XIT                                                              
         SPACE 3                                                                
BUILDIT  ST    RE,SAVERE                                                        
         GOTO1 VTWABLD,TWAPARMD                                                 
         CLI   TWAPERRS,0          RETURN IF NO ERRORS                          
         BE    BUIL020                                                          
         CLI   TWAPERRS,TWAPEEMS   WAS MAXIMUM EXCEEDED ?                       
         BE    SCRERR              YES, PRINT MESSAGE                           
         DC    H'0'                NO, BLOW UP                                  
*                                                                               
BUIL020  L     RE,SAVERE                                                        
         BR    RE                                                               
         DROP  R7                                                               
         EJECT                                                                  
***********************************************************************         
* SUBROUTINE TO SAVE FIELD KEYWORDS AND SEQUENCE NUMBER. RETURN CODE  *         
* IS SET TO ZERO IF DUPLICATION OCCURS.                               *         
***********************************************************************         
*                                                                               
DUPKEY   ST    RE,SAVERE                                                        
         LA    RE,KEYTAB           GET KEYWORD TABLE                            
         SR    R1,R1                                                            
         ICM   R1,1,NKEYS                                                       
         BZ    DUPK040             NO ENTRIES YET                               
*                                                                               
DUPK020  CLC   ACFDFLD,0(RE)       DO FIELDS MATCH ?                            
         BE    DUPKX               YES, EXIT WITH EQ RETURN CODE                
         LA    RE,KEYLEN(RE)                                                    
         BCT   R1,DUPK020                                                       
*                                                                               
DUPK040  MVC   0(L'ACFDFLD,RE),ACFDFLD                                          
         STC   R3,4(RE)            SAVE SEQUENCE NUMBER                         
         SR    R1,R1               INCREMENT NUMBER OF ENTRIES                  
         IC    R1,NKEYS                                                         
         LA    R1,1(R1)                                                         
         STC   R1,NKEYS                                                         
         LTR   R8,R8               SET RETURN CODE TO NEQ                       
*                                                                               
DUPKX    L     RE,SAVERE                                                        
         BR    RE                                                               
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* SUBROUTINE TO READ TEXT RECORD AND GET HEADER AND DATA ELEMENTS. IF *         
* RECORD IS NOT FOUND OR AN ERROR EXISTS, A MESSSGE IS DISPLAYED AND  *         
* THE ROUTINE EXITED. IF RECORD IS FOUND, DATA, IF ANY, AND AN APPRO- *         
* PRIATE MESSAGE ARE DISPLAYED                                        *         
***********************************************************************         
*                                                                               
DISLOGIC NTR1                                                                   
         CLI   NUMFLDS,0                                                        
         BE    DISL100                                                          
*                                                                               
         BAS   RE,READTXT          READ THE TEXT RECORD                         
         BNE   DISL080             NOT FOUND                                    
         CLI   DELSW,C'Y'          FOUND - IS IT DELETED ?                      
         BE    DISL080             YES, SKIP DISPLAY ALSO                       
*                                                                               
DISL010  MVI   ELCODE,ACTHELQ      GET TEXT HEADER ELEMENT                      
         BAS   RE,GETELIO                                                       
         BE    DISL020             FOUND IT                                     
         BAS   RE,ADDHEAD          IF NOT, ADD IT BACK                          
         BAS   RE,WRITEIT                                                       
         B     DISL010                                                          
*                                                                               
DISL020  MVI   NDATA,0             CLEAR DATA COUNTER                           
*                                                                               
         OC    QCOMMENT,QCOMMENT   CREATING STANDARD COMMENT ?                  
         BZ    DISL030             NO, GET TEXT DATA ELEMENT                    
*                                                                               
         LA    R2,TXTCOMMH         YES, GET STANDARD COMMENT                    
         BAS   RE,READSTD                                                       
         BNE   DISL060                                                          
         MVI   ELCODE,X'3E'        FOUND IT, GET ELEMENTS                       
         B     *+8                                                              
*                                                                               
DISL030  MVI   ELCODE,ACTFELQ      GET TEXT DATA ELEMENT                        
         L     R2,AFSTFLD          ADDRESS FIRST SCREEN FIELD                   
         BAS   RE,GETELIO                                                       
         LA    R4,MAXLINE                                                       
         B     *+8                                                              
*                                                                               
         USING ACTFD,R6                                                         
DISL040  BAS   RE,NEXTEL                                                        
         BNE   DISL060             NO MORE DATA TO DISPLAY                      
         BAS   RE,DISDATA                                                       
         OC    QCOMMENT,QCOMMENT   IS THIS A STANDARD COMMENT ?                 
         BNZ   DISL050             YES, LIMIT TO MAXLINE                        
         CLI   ACTFTYPE,ACTFTREG   NO, IS IT A REGUALR COMMENT ?                
         BE    DISL050             YES, LIMIT THAT ONE TOO.                     
         B     DISL040             NO, TAKE THEM ALL                            
*                                                                               
DISL050  BCT   R4,DISL040                                                       
*                                                                               
DISL060  CLI   NDATA,X'00'         DATA TO DISPLAY ?                            
         BE    DISL080             NO                                           
         L     R2,AFSTFLD          POSITION CURSOR AT FIRST KEY FIELD           
         TM    1(R2),X'20'         IS THIS FIELD UNPROTECTED ?                  
         BZ    *+8                 YES, SAVE POSITION                           
         BAS   RE,BUMPTOUN         NO, FIND ONE                                 
         ST    R2,ACURFORC                                                      
         OC    QCOMMENT,QCOMMENT   ADDING STANDARD COMMENT ?                    
         BNZ   DISL120             NO                                           
         MVC   CONHEAD(L'DISMSG),DISMSG                                         
         B     DISLX                                                            
*                                                                               
DISL080  L     R2,AFSTFLD          POSITION CURSOR AT FIRST KEY FIELD           
         TM    1(R2),X'20'         IS THIS FIELD UNPROTECTED ?                  
         BZ    *+8                 YES, SAVE POSITION                           
         BAS   RE,BUMPTOUN         NO, FIND ONE                                 
         ST    R2,ACURFORC                                                      
         MVC   CONHEAD(L'NOMSG),NOMSG                                           
         B     DISLX                                                            
*                                                                               
DISL100  MVC   CONHEAD(L'NOSMSG),NOSMSG                                         
         B     XIT                                                              
*                                                                               
DISL120  LA    R2,TXTCOMMH                                                      
         XC    QCOMMENT,QCOMMENT                                                
         XC    8(L'QCOMMENT,R2),8(R2)                                           
         MVI   5(R2),0                                                          
         BAS   RE,TSTEDT                                                        
         BNE   *+8                                                              
         BAS   RE,EDTLOGIC                                                      
*                                                                               
DISLX    BAS   RE,SETVAL           VALIDATE ALL FIELDS                          
         B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* DISPLAY DATA FROM TEXT DATA ELEMENTS BY MATCHING THE KEYWORD TO A   *         
* TABLE ENTRY AND GETTING THE APPROPRIATE SEQUENCE #.                 *         
***********************************************************************         
*                                                                               
DISDATA  ST    RE,SAVERE                                                        
         OC    QCOMMENT,QCOMMENT   ARE WE DOING STAND ACOMMENTS ?               
         BNZ   DISSTD              YES                                          
*                                                                               
         USING ACTFD,R6                                                         
         CLI   ACTFTYPE,ACTFTREG   IS THIS REGULAR COMMENTS ?                   
         BE    DISREG              YES                                          
*                                                                               
         SR    R0,R0               NO                                           
         ICM   R0,1,NKEYS                                                       
         BZ    DISDX               NO KEYWORDS IN TABLE - EXIT                  
*                                                                               
         SR    R3,R3                                                            
         IC    R3,ACTFLEN          IS THERE ANY TEXT DATA ?                     
         SH    R3,=AL2(ACTFTEXT-ACTFEL)                                         
         BZ    DISDX               NO - EXIT                                    
         BCTR  R3,0                YES, GET LENGTH FOR MOVE                     
         LA    R5,KEYTAB                                                        
*                                                                               
DISD020  CLC   ACTFFLD,0(R5)       IS THIS THE MATCHING ENTRY ?                 
         BNE   DISD040             NO                                           
*                                                                               
         L     R2,AFSTFLD          ADDRESS FIRST FIELD ON SCREEN                
         SR    R1,R1               YES, GET THE SEQUENCE NUMBER                 
         IC    R1,4(R5)                                                         
         SH    R1,=H'1'                                                         
         BZ    DISD025             ITS THE FIRST FIELD                          
         BAS   RE,BUMP             BUMP TO CORRECT SEQUENCE NUMBER              
         BCT   R1,*-4                                                           
*                                                                               
DISD025  ZIC   RE,0(R2)            DERIVE FIELD LENGTH                          
         SH    RE,=H'9'                                                         
         TM    1(R2),X'02'         TEST FOR EXTENDED FIELD                      
         BZ    *+8                                                              
         SH    RE,=H'8'            YES-ADJUST FOR IT                            
         CR    R3,RE               TEST IF DATA IS LONGER THAN FIELD            
         BNH   *+6                 NO                                           
         LR    R3,RE               YES-ONLY MOVE LENGTH OF FIELD                
*                                                                               
         EX    R3,*+8                                                           
         B     DISDX                                                            
         MVC   8(0,R2),ACTFTEXT                                                 
*                                                                               
DISD040  LA    R5,KEYLEN(R5)       NO, GET NEXT ENTRY                           
         BCT   R0,DISD020                                                       
*                                                                               
DISDX    SR    R1,R1                                                            
         IC    R1,NDATA                                                         
         LA    R1,1(R1)                                                         
         STC   R1,NDATA                                                         
         L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* DISPLAY REGULAR COMMENTS.                                           *         
***********************************************************************         
*                                                                               
DISREG   BAS   RE,BUMP             GET PAST ACTION FIELD                        
         SR    R1,R1                                                            
         IC    R1,ACTFLEN                                                       
         SH    R1,=AL2(ACTFTEXT-ACTFEL)                                         
         BZ    DISDX               NO COMMENT, EXIT                             
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),ACTFTEXT                                                 
         BAS   RE,BUMP                                                          
         B     DISDX                                                            
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* DISPLAY STANDARD COMMENTS.                                          *         
***********************************************************************         
*                                                                               
         USING ACOMMD,R6                                                        
DISSTD   BAS   RE,BUMP             GET PAST ACTION FIELD                        
         SR    R1,R1                                                            
         IC    R1,ACOMLEN                                                       
         SH    R1,=AL2(ACOMMENT-ACOMEL)                                         
         BZ    DISDX               NO COMMENT, EXIT                             
         STC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),ACOMMENT                                                 
         OI    6(R2),X'80'                                                      
         BAS   RE,BUMP                                                          
         B     DISDX                                                            
         EJECT                                                                  
***********************************************************************         
* SUBROUTINE TO READ TEXT RECORD AND VALIDATE PANEL AND/OR COMMENT.   *         
* IF RECORD NOT FOUND, OR IS DELETED, PANEL CODE TO BE USED IS FOUND  *         
* AND AN ERROR FLAG IS SET.                                                     
***********************************************************************         
*                                                                               
CHKREC   NTR1                                                                   
         MVI   TXTPANH+5,0         CLEAR PANEL CODE FROM LAST TIME              
         XC    TXTPAN,TXTPAN                                                    
*                                                                               
         BAS   RE,READTXT                                                       
         BE    FOUNDTXT            FOUND RECORD                                 
*                                                                               
NOTFND   LA    R2,TXTPANH          SET DEFAULT PANEL CODE THEN                  
         MVC   QPANEL,DEFPAN                                                    
         MVC   8(L'QPANEL,R2),DEFPAN                                            
         MVI   5(R2),4                                                          
*                                                                               
         OC    QCOMMENT,QCOMMENT   WAS COMMENT ENTERED ?                        
         BNZ   TXTADD              YES, ADD THE RECORD THEN                     
*                                                                               
         BAS   RE,DEFAULT                                                       
         GOTO1 VALPAN                                                           
         MVC   QPANEL,PANEL                                                     
         BAS   RE,READTXT          RE-READ RECORD INTO IO AREA                  
*                                                                               
         CLC   QPANEL,DEFPAN       DEFAULT PANEL ?                              
         BE    CHKRECX             YES, DON'T ADD IT YET                        
*                                                                               
TXTADD   CLI   DELSW,C'Y'          NO, IS RECORD DELETED ?                      
         BE    HEADADD             YES, SKIP ADD                                
         CLI   FOUND,C'Y'          NO, DID WE FIND IT AND DELETE HEADER         
         BE    *+8                 YES                                          
         BAS   RE,ADDTXT                                                        
*                                                                               
HEADADD  BAS   RE,ADDHEAD                                                       
         BAS   RE,WRITEIT                                                       
         B     CHKRECX                                                          
*                                                                               
FOUNDTXT CLI   DELSW,C'Y'          WAS RECORD DELETED ?                         
         BE    NOTFND              TREAT AS IF NOT FOUND                        
*                                                                               
         MVI   ELCODE,ACTHELQ      GET HEADER ELEMENT                           
         BAS   RE,GETELIO                                                       
         BNE   NOTFND              NOT THERE, ADD IT                            
*                                                                               
         USING ACTHD,R6                                                         
         OC    QCOMMENT,QCOMMENT   WAS COMMENT ENTERED?                         
         BNZ   COMENTER            YES                                          
*                                                                               
         MVC   QPANEL,ACTHPAN      YES, MOVE TO SAVE AND SCREEN                 
         LA    R2,TXTPANH                                                       
         MVC   8(L'PANEL,R2),ACTHPAN                                            
         LA    R1,4                GET LENGTH                                   
         LA    R3,8+L'PANEL-1(R2)                                               
*                                                                               
PANLNG   CLI   0(R3),C' '          AT WE AT END ?                               
         BNE   PANLNGX             YES, SET LENGTH                              
         BCTR  R3,0                NO, BACK UP                                  
         BCT   R1,PANLNG                                                        
*                                                                               
PANLNGX  STC   R1,5(R2)                                                         
         B     CHKRECX                                                          
*                                                                               
COMENTER LA    R2,TXTCOMMH                                                      
         MVC   QPANEL,DEFPAN                                                    
         OC    ACTHPAN,ACTHPAN     IS THERE A PANEL NUMBER ?                    
         BZ    CHKRECX             NO                                           
*                                                                               
         MVC   PANCONMG+6(4),ACTHPAN                                            
         MVC   CONHEAD(L'PANCONMG),PANCONMG    YES, PRINT MESSAGE               
         ST    R2,ACURFORC                                                      
         MVI   ERRSWT,C'Y'                                                      
         MVI   ERROR,X'FE'                                                      
         OI    GENSTAT2,USMYOK                                                  
         OI    CONHEADH+1,X'08'                                                 
         GOTO1 ERREX2                                                           
*                                                                               
CHKRECX  B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* SUBROUTINE TO PROCESS PFKEYS.                                       *         
***********************************************************************         
*                                                                               
PROCPF   NTR1                                                                   
         CLI   PFKEY,PF1                                                        
         BNE   PROCPF2                                                          
*                                                                               
         MVI   PFKEY,0                                                          
         GOTO1 VCALL,WORK,=C'OPTION',=C'MAINT',(L'QOFG,QOFG),(L'QOFF,QOX        
               FF),(L'QCLI,QCLI),(L'QPROD,QPROD),(L'QJOB,QJOB),(L'QMGR,X        
               QMGR),(L'QMED,QMED),=C',',=C',',=C'E',0                          
         B     PROCPFY                                                          
*                                                                               
PROCPF2  CLI   PFKEY,PF2                                                        
         BNE   PROCPF5                                                          
*                                                                               
         MVI   PFKEY,0                                                          
         GOTO1 VCALL,WORK,=C'PANEL',=C'LIST',(L'QPANEL,QPANEL),0                
         B     PROCPFY                                                          
*                                                                               
PROCPF5  CLI   PFKEY,PF5                                                        
         BNE   PROCPF7                                                          
*                                                                               
         MVI   PFKEY,0                                                          
         L     RF,VCALL            USE VCALL SO USER CAN RETURN HERE            
         SR    R1,R1                                                            
         ICM   R1,1,CALLSP         UNLESS JOB ESTIMATE IS IN THE                
         BZ    PROCPF55            STACK                                        
         LA    RE,CALLSTK          RE=STACK POINTER                             
*                                                                               
         CLI   0(RE),X'32'         TEST FOR JOB ESTIMATE                        
         BE    *+16                YES                                          
         LA    RE,1(RE)                                                         
         BCT   R1,*-12                                                          
         B     PROCPF55                                                         
*                                                                               
         L     RF,VTRANSF          USE TRANSFER INSTEAD                         
*                                                                               
PROCPF55 GOTO1 (RF),WORK,=C'JOB',=C'ESTIMATE',(L'QCLI,QCLI),(L'QPROD,QPX        
               ROD),(L'QJOB,QJOB),0                                             
         B     PROCPFY                                                          
*                                                                               
PROCPF7  CLI   PFKEY,PF7                                                        
         BNE   PROCPF8                                                          
         B     PROCPF78                                                         
*                                                                               
PROCPF8  CLI   PFKEY,PF8                                                        
         BNE   PROCPF9                                                          
*                                                                               
PROCPF78 BAS   RE,GETSPOT                                                       
         BAS   RE,VALHED                                                        
         BAS   RE,CHKREC                                                        
         BAS   RE,BLDTWA                                                        
         BAS   RE,SETFLDS                                                       
         MVI   MODE,VALKEY                                                      
         MVI   PFSWT,C'N'                                                       
         B     PROCPFN                                                          
*                                                                               
PROCPF9  CLI   PFKEY,PF9           TOGGLE 'ACT' FIELD ?                         
         BNE   PROCPF10            NO                                           
         CLC   QPANEL,DEFPAN       YES, IF THIS A STANDARD COMMENT ?            
         BNE   PROCPFY             NO, INGORE THE KEY THEN                      
         BAS   RE,SETATTR          YES, REVERSE THE ATTRIBUTE                   
         B     PROCPFY                                                          
*                                                                               
PROCPF10 CLI   PFKEY,PF10                                                       
         BNE   PROCPFY                                                          
         CLI   ERRSWT,C'Y'         DO WE HAVE AN ERROR CONDITION ?              
         BNE   PROCPFY             NO, IGNORE KEY                               
         BAS   RE,READTXT          YES, READ TEXT RECORD                        
         BE    *+6                                                              
         DC    H'0'                MUST HAVE IT BY NOW                          
*                                                                               
         MVI   ELCODE,ACTHELQ      DELETE HEADER                                
         GOTO1 REMELEM                                                          
         MVI   ELCODE,ACTFELQ      DELETE DATA ELEMENTS                         
         GOTO1 REMELEM                                                          
         BAS   RE,WRITEIT          REWRITE THE RECORD                           
         MVI   INTMODE,FSTMODE     REDO SCREEN                                  
         MVI   ERRSWT,C'N'         CLEAR ERROR INDICATOR                        
         MVI   PFKEY,0                                                          
*                                                                               
PROCPFY  CR    RE,RE                                                            
         B     PROCPFX                                                          
*                                                                               
PROCPFN  LTR   RE,RE                                                            
*                                                                               
PROCPFX  B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* SUBROUTINE TO TEST FOR ANYTHING ON SCREEN TO EDIT. ON EXIT : CC=EQ  *         
* IF EDIT NEEDED; CC=NEQ IF EDIT NOT NEEDED.                          *         
***********************************************************************         
*                                                                               
TSTEDT   NTR1                                                                   
         L     R2,AFSTFLD          R2=A(FIRST FIELD)                            
*                                                                               
         SR    R3,R3                                                            
         ICM   R3,1,NUMFLDS        ANY FIELDS ON SCREEN ?                       
         BZ    TSTEDTN             NO, NOTHING TO EDIT                          
*                                                                               
         CLC   QPANEL,DEFPAN       ARE WE DOING COMMENTS ?                      
         BE    TSTE040             YES, DON'T LOOK FOR KEYWORDS                 
*                                                                               
         SR    R1,R1                                                            
         ICM   R1,1,NKEYS          NO, DO WE HAVE ANY KEYWORDS                  
         BZ    TSTEDTN             NO, NOTHING TO EDIT                          
*                                                                               
TSTE020  TM    1(R2),X'20'         IS THIS A PROTECTED FIELD ?                  
         BO    *+12                YES, SKIP IT                                 
         TM    4(R2),X'20'         NO, WAS IT CHANGED ?                         
         BZ    TSTEDTY             YES, INDICATE EDIT NEEDED                    
         BAS   RE,BUMP             NO, GET NEXT ENTRY                           
         BCT   R3,TSTE020           AND KEEP LOOKING                            
         B     TSTEDTN                                                          
*                                                                               
TSTE040  TM    ACTSTAT,X'80'       IS ACT PROTECTED ?                           
         BO    TSTE120             YES                                          
         LR    R0,R3               MAKE SURE DATA, IF ANY IS VALID              
         MVI   ERROR,INVALID                                                    
*                                                                               
TSTE060  TM    1(R2),X'20'         FIND UNPROTECTED FIELD                       
         BNO   TSTE080                                                          
         BAS   RE,BUMP                                                          
         BCT   R0,TSTE060                                                       
*                                                                               
TSTE080  CLI   5(R2),0                                                          
         BE    TSTE100                                                          
         CLI   8(R2),C'I'          MAKE SURE SEL FIELD IS VALID                 
         BE    TSTE100                                                          
         CLI   8(R2),C'D'                                                       
         BNE   ERREXIT                                                          
*                                                                               
TSTE100  BAS   RE,BUMP             GET TO NEXT SELECT                           
         BAS   RE,BUMP                                                          
         BCTR  R0,0                                                             
         BCT   R0,TSTE080                                                       
*                                                                               
TSTE120  L     R2,AFSTFLD          ALL GOOD, LOOK FOR CHANGES                   
         B     TSTE020                                                          
*                                                                               
TSTEDTN  LTR   R8,R8               SET CC=NEQ                                   
         B     TSTEDTX                                                          
*                                                                               
TSTEDTY  CLC   QPANEL,DEFPAN        ARE WE DOING COMMENTS ?                     
         BNE   TSTESET             NO, SKIP OVER                                
         L     R2,AFSTFLD          ALL GOOD, LOOK FOR CHANGES                   
         BAS   RE,FINDLAST                                                      
TSTESET  CR    R8,R8                                                            
*                                                                               
TSTEDTX  B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* EDIT THE CHANGES AND UPDATE, OR CREATE, TEXT DATA ELEMENTS. UPDATE/ *         
* ADD RECORD/HEADER AND DATA.                                         *         
***********************************************************************         
*                                                                               
EDTLOGIC NTR1                                                                   
         L     R2,AFSTFLD          ADDRESS FIRST SCREEN ENTRY                   
         BAS   RE,READTXT          READ THE TEXT RECORD                         
         BE    EDTL040             FOUND IT                                     
         BAS   RE,ADDTXT           NO, ADD TEXT                                 
*                                                                               
EDTL020  BAS   RE,ADDHEAD          ADD HEADER                                   
         BAS   RE,WRITEIT                                                       
         B     EDTL060                                                          
*                                                                               
EDTL040  CLI   DELSW,C'Y'          NOT THERE, IS IT DELETED ?                   
         BE    EDTL020             YES, ADD HEADER                              
         MVI   ELCODE,ACTFELQ      DELETE ALL TEXT DATA ELEMENTS                
         GOTO1 REMELEM                                                          
*                                                                               
EDTL060  CLC   QPANEL,DEFPAN       IS THIS REGULAR COMMENTS ?                   
         BE    EDTCOM              YES                                          
*                                                                               
         SR    R0,R0                                                            
         IC    R0,NUMFLDS          GET NUMBER OF FIELDS ON SCREEN               
*                                                                               
         LA    R3,1                SET SEQUENCE #                               
*                                                                               
EDTL080  TM    1(R2),X'20'         IS FIELD PROTECTED ?                         
         BO    EDTL160             YES, SKIP IT                                 
         CLI   5(R2),0             NO, ANY DATA ENTERED ?                       
         BE    EDTL160             NO, SKIP IT                                  
*                                                                               
EDTL100  LA    R5,KEYTAB           GET KEYWORD TABLE AND # OF ENTRIES           
         SR    R1,R1                                                            
         IC    R1,NKEYS                                                         
*                                                                               
EDTL120  CLM   R3,1,4(R5)          DO SEQUENCE NUMBERS MATCH ?                  
         BE    EDTL140             YES, ADD NEW TEXT DATA ELEMENT               
*                                                                               
         LA    R5,KEYLEN(R5)       NO, GET NEXT TABLE ENTRY                     
         BCT   R1,EDTL120                                                       
         B     EDTL160             NOT IN TABLE, GET NEXT FIELD                 
*                                                                               
EDTL140  BAS   RE,ADDDATA          EDIT AND ADD B0 ELEMENT                      
*                                                                               
EDTL160  LA    R3,1(R3)                                                         
         BAS   RE,BUMP                                                          
         BCT   R0,EDTL080                                                       
*                                                                               
EDTL180  BAS   RE,WRITEIT          REWRITE THE RECORD                           
         MVI   INTMODE,DISMODE      AND REDISPLAY DATA                          
         MVI   ERRSWT,C'N'         CLEAR ERROR SWITCH                           
         BAS   RE,DISLOGIC                                                      
         MVC   CONHEAD(L'EDTMSG),EDTMSG                                         
*                                                                               
EDTLX    B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* SUBROUTINE TO EDIT COMMENT CHANGES.                                 *         
***********************************************************************         
*                                                                               
EDTCOM   SR    R0,R0                                                            
         IC    R0,LASTLINE         LAST LINE OF DATA ON SCREEN                  
         LA    R3,1                SET SEQUENCE #                               
*                                                                               
EDTC040  MVI   THISACT,C' '                                                     
         TM    ACTSTAT,X'80'       IS SEL/ACT PROTECTED ?                       
         BO    EDTC080             YES                                          
         MVC   THISACT,8(R2)       SAVE ACTION CODE                             
         CLI   5(R2),0             NO DATA HERE, CHECK TEXT                     
         BE    EDTC080                                                          
         MVI   ERROR,INVALID                                                    
         CLI   THISACT,C'I'        ARE WE INSERTING ?                           
         BE    EDTC080             YES                                          
         CLI   THISACT,C'D'        NO, MUST BE DELETE THEN                      
         BNE   ERREXIT                                                          
*                                                                               
EDTC060  BAS   RE,BUMP             IF DELETE, SKIP OVER LINE                    
         BAS   RE,BUMP                                                          
         BCT   R0,EDTC040                                                       
         B     EDTC120                                                          
*                                                                               
EDTC080  BAS   RE,BUMP             GET TO TEXT                                  
         BAS   RE,ADDDATA          ADD DATA ELEMENT                             
         BAS   RE,BUMP             GET TO NEXT ACTION                           
         LA    R3,1(R3)                                                         
         CLI   THISACT,C'I'                                                     
         BNE   EDTC100                                                          
         BAS   RE,ADDDATA                                                       
         LA    R3,1(R3)                                                         
*                                                                               
EDTC100  BCT   R0,EDTC040                                                       
*                                                                               
EDTC120  GOTO1 VCLEARF,DMCB,AFSTFLD,ALSTFLD                                     
         B     EDTL180                                                          
         SPACE 3                                                                
FINDLAST ST    RE,SAVERE                                                        
         LR    R1,R2               SAVE STARTING ADDRESS                        
         MVI   LASTLINE,0                                                       
         LA    R3,1                                                             
         LA    R0,MAXLINE                                                       
         BAS   RE,BUMP             GET TO TEXT FIELD                            
*                                                                               
FINDL020 CLI   5(R2),0                                                          
         BE    *+8                                                              
         STC   R3,LASTLINE         SAVE NUMBER OF LINE WITH DATA                
         LA    R3,1(R3)                                                         
         BAS   RE,BUMP             GET TO NEXT TEXT FIELD                       
         BAS   RE,BUMP                                                          
         BCT   R0,FINDL020                                                      
*                                                                               
         LR    R2,R1               RESTORE R2                                   
         CLI   LASTLINE,0                                                       
         BNE   FINDLX                                                           
         MVI   ERROR,MISSING                                                    
         B     ERREXIT                                                          
*                                                                               
FINDLX   L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* READ TEXT RECORDS, EVEN DELETED ONES.                               *         
***********************************************************************         
*                                                                               
READTXT  ST    RE,SAVERE                                                        
         MVI   DELSW,C'N'          CLEAR DELETED RECORD SWITCH                  
         MVI   FOUND,C'N'          ASSUME NOT FOUND                             
         LA    R6,KEY                                                           
         USING ACTXKEY,R6          GET NEXT RECORD IF NONE                      
         XC    ACTXKEY,ACTXKEY                                                  
         MVI   ACTXRTYP,ACTXEQU                                                 
         MVI   ACTXSREC,ACTXSEQU                                                
         MVC   ACTXCUL,CUL                                                      
         MVC   ACTXOG,QOFG                                                      
         MVC   ACTXOFC,QOFF                                                     
         MVC   ACTXCLI,QCLI                                                     
         MVC   ACTXPROD,QPROD                                                   
         MVC   ACTXJOB,QJOB                                                     
         MVC   ACTXMG,QMGR                                                      
         MVC   ACTXMED,QMED                                                     
         MVC   ACTXCAT,QCAT                                                     
         MVC   ACTXWORK,QWORK                                                   
         MVC   ACTXSUFF,QSUFFIX                                                 
         MVC   ACTXFORM,QFORM                                                   
         MVC   ACTXWHER,QWHER                                                   
         OI    DMINBTS,X'08'       READ DELETED RECORDS AS WELL                 
         GOTO1 HIGH                                                             
         NI    DMINBTS,X'FF'-X'08'                                              
         L     R6,AIO                                                           
         USING ACKEYD,R6                                                        
         CLC   KEYSAVE(ACTXWHER-ACTXKEY+2),KEY                                  
         BNE   READTXTX            NOT FOUND, GET OUT                           
         MVI   FOUND,C'Y'          INDICATE FOUND                               
         TM    ACSTATUS,X'80'      IS IT DELETED ?                              
         BZ    READTXTX            NO, GET OUT                                  
         NI    ACSTATUS,X'FF'-X'80' YES, CLEAR DELETED BIT                      
         MVI   ELCODE,ACTHELQ      DELETE HEADER ELEMENT                        
         GOTO1 REMELEM                                                          
         MVI   ELCODE,ACTFELQ      DELETE DATA ELEMENT                          
         GOTO1 REMELEM                                                          
         MVI   DELSW,C'Y'          SET DELETED INDICATOR                        
         CR    R8,R8               SET RETURN CODE                              
*                                                                               
READTXTX L     RE,SAVERE                                                        
         BR    RE                                                               
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* READ STANDARD COMMENT RECORDS, EVEN DELETED ONES.                   *         
***********************************************************************         
*                                                                               
READSTD  ST    RE,SAVERE                                                        
         LA    R6,KEY                                                           
         XC    KEY,KEY                                                          
         MVI   KEY,X'0C'                                                        
         MVC   KEY+1(1),CUL                                                     
         MVC   KEY+2(6),BLANKS                                                  
         LA    R3,KEY+2+6                                                       
         SR    R1,R1                                                            
         IC    R1,5(R2)                                                         
         SR    R3,R1                                                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),8(R2)                                                    
         OI    DMINBTS,X'08'       READ DELETED RECORDS AS WELL                 
         GOTO1 HIGH                                                             
         NI    DMINBTS,X'FF'-X'08'                                              
         L     R6,AIO                                                           
         USING ACKEYD,R6                                                        
         CLC   KEYSAVE(ACTXWHER-ACTXKEY+2),KEY                                  
         BNE   READSTDX            NOT FOUND, GET OUT                           
         CR    R8,R8               SET RETURN CODE                              
*                                                                               
READSTDX L     RE,SAVERE                                                        
         BR    RE                                                               
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* SUBROUTINE TO ADD THE TEXT RECORD BASED ON KEY USED TO DO READ.     *         
***********************************************************************         
*                                                                               
ADDTXT   ST    RE,SAVERE                                                        
         NI    DMINBTS,X'F7'       TURN OFF READ DELETED RECORDS BIT            
         L     RE,AIO                                                           
         L     RF,SIZEIO                                                        
         XCEF                                                                   
         USING ACTXKEY,RE                                                       
         L     RE,AIO                                                           
         MVC   ACTXKEY,KEYSAVE     MOVE RECORD KEY TO IO AREA                   
         USING ACKEYD,RE                                                        
         MVC   ACLENGTH,=Y(ACRECORD-ACKEYD+1)                                   
         MVC   KEY(L'ACTXKEY),ACTXKEY                                           
         GOTO1 ADD                                                              
         L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* SUBROUTINE TO ADD THE TEXT HEADER ELEMENT.                          *         
***********************************************************************         
*                                                                               
ADDHEAD  ST    RE,SAVERE                                                        
         LA    R6,ELEMENT                                                       
         USING ACTHD,R6                                                         
         XC    ELEMENT,ELEMENT                                                  
         MVI   ACTHEL,ACTHELQ                                                   
         MVI   ACTHLEN,ACTHLENQ                                                 
         MVC   ACTHPAN,BLANKS                                                   
         LA    RE,TXTPANH                                                       
         SR    R1,R1                                                            
         IC    R1,5(RE)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   ACTHPAN(0),8(RE)                                                 
         GOTO1 ADDELEM                                                          
         L     RE,SAVERE                                                        
         BR    RE                                                               
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* SUBROUTINE TO ADD THE TEXT DATA ELEMENT.                            *         
***********************************************************************         
*                                                                               
ADDDATA  ST    RE,SAVERE                                                        
         LA    R6,ELEMENT                                                       
         USING ACTFD,R6                                                         
         XC    ELEMENT,ELEMENT                                                  
         MVI   ACTFEL,ACTFELQ                                                   
         CLC   QPANEL,DEFPAN       IS THIS REGULAR COMMENTS ?                   
         BNE   ADDD020             NO                                           
         MVI   ACTFTYPE,ACTFTREG   YES                                          
         STC   R3,ACTFSEQ          SAVE SEQUENCE #                              
         B     ADDD040                                                          
*                                                                               
ADDD020  MVI   ACTFTYPE,ACTFTPAN                                                
         MVC   ACTFFLD,0(R5)       SAVE KEYWORD                                 
*                                                                               
ADDD040  SR    RF,RF                                                            
         IC    RF,=YL1(ACTFTEXT-ACTFEL)                                         
*                                                                               
         CLI   5(R2),0             DO WE HAVE ANY DATA ?                        
         BNE   ADDD060             YES                                          
         LA    RF,1(RF)            NO, ADD BLANK LINE                           
         MVI   ACTFTEXT,C' '                                                    
         B     ADDD080                                                          
*                                                                               
ADDD060  SR    R1,R1                                                            
         IC    R1,5(R2)            GET LENGTH OF DATA FOR MOVE                  
         AR    RF,R1               ADD TO ELEMENT LENGTH                        
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   ACTFTEXT(0),8(R2)                                                
         OI    4(R2),X'20'         INDICATE FIELD HAS BEEN VALIDATED            
         OI    6(R2),X'80'         RETRANSMIT THE FIELD                         
*                                                                               
ADDD080  STC   RF,ACTFLEN          UPDATE LENGTH AND ADD ELEMENT                
         GOTO1 ADDELEM                                                          
         L     RE,SAVERE                                                        
         BR    RE                                                               
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* SUBROUTINE TO ADD PERSONAL ELEMENT AND ADD/UPDATE TEXT RECORD.      *         
***********************************************************************         
*                                                                               
WRITEIT  ST    RE,SAVERE                                                        
         GOTO1 PERSIN                                                           
         GOTO1 WRITE                                                            
         L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* LOAD THE BUFFER WHERE SCREEN DATA IS TO BE STORED.                  *         
***********************************************************************         
*                                                                               
LOAD     NTR1                                                                   
         L     RE,=V(DUMMY)                                                     
         A     RE,RELO                                                          
         ST    RE,DMCB             SET LOAD POINT FOR BUFFER                    
         MVC   DMCB+4(3),SYSPHASE                                               
         MVI   DMCB+7,X'50'        SET BUFFER OVERLAY NUMBER                    
         GOTO1 CALLOV,DMCB                                                      
         CLI   DMCB+4,X'FF'        TEST LOAD OK                                 
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   ABUFFER,DMCB        RF=A(BUFFER PHASE)                           
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*               SUBROUTINE TO DETERMINE WHICH "WHERE" TO USE.         *         
***********************************************************************         
*                                                                               
GETSPOT  NTR1                                                                   
         OC    GOTSCRL,GOTSCRL     ANY SCROLL TABLE FROM GETOPT ?               
         BZ    GETS02              NO                                           
         LA    R1,GOTSCRL                                                       
         B     GETS04                                                           
*                                                                               
GETS02   LA    R1,SCRLTAB          ADDRESS DEFAULT TABLE                        
*                                                                               
GETS04   LR    R5,R1               SAVE TABLE ADDRESS                           
         LA    R2,TXTWHERH                                                      
         MVC   WORK(2),8(R2)                                                    
         OC    WORK(2),BLANKS                                                   
*                                                                               
GETS06   CLI   0(R1),0             END OF TABLE ?                               
         BH    GETS16              NO                                           
*                                                                               
GETS08   LR    R1,R5               YES, START OVER                              
*                                                                               
GETS10   OC    QCAT,QCAT           ANY CATEGORY ?                               
         BNZ   GETS12              YES                                          
         OC    QWORK,QWORK         NO, ANY WORKCODE ?                           
         BZ    GETS14              NO                                           
*                                                                               
GETS12   CLI   0(R1),C'H'          MUST BE HEADER OR FOOTER IF                  
         BE    GETS14              CATEGORY OR WORKCODE ENTERED                 
         CLI   0(R1),C'F'                                                       
         BNE   GETS18                                                           
*                                                                               
GETS14   MVC   TXTWHER,0(R1)                                                    
         MVI   TXTWHERH+5,X'01'    SET DEFAULT LENGTH                           
         CLI   TXTWHER+1,C' '      DO WE HAVE A SECOND CHARACTER ?              
         BE    *+8                 NO                                           
         MVI   TXTWHERH+5,X'02'    YES, CHANGE LENGTH                           
         OI    TXTWHERH+6,X'80'                                                 
         NI    TXTWHERH+4,X'FF'-X'20'                                           
         OI    TXTWHERH+4,X'80'    INDICATE ENTERED THIS TIME                   
         B     XIT                                                              
*                                                                               
GETS16   CLC   0(2,R1),WORK        ARE WE WHERE WE STARTED ?                    
         BE    GETS18              YES                                          
         LA    R1,L'SCRLTAB(R1)    NO, KEEP LOOKING                             
         B     GETS06                                                           
*                                                                               
*                                  FORWARD SCROLL                               
GETS18   CLI   PFKEY,PF8                                                        
         BNE   GETS20                                                           
         LA    R1,L'SCRLTAB(R1)    IF END IS REACHED, STAY PUT                  
         CLI   0(R1),0                                                          
         BE    XIT                                                              
         B     GETS10                                                           
*                                                                               
*                                  BACKWARD SCROLL                              
GETS20   SH    R1,=YL2(L'SCRLTAB)                                               
         CR    R1,R5               HAVE WE REACHED THE START ?                  
         BL    XIT                 YES, STAY PUT                                
         CLI   0(R1),0             NO, DO WE HAVE ANY DATA HERE ?               
         BE    GETS20              NO, BACKUP AGAIN                             
         B     GETS10                                                           
         EJECT                                                                  
***********************************************************************         
* SUBROUTINE TO DETERMINE WHICH PANEL TO USE.                         *         
***********************************************************************         
*                                                                               
DEFAULT  NTR1                                                                   
         BAS   RE,SETGO            SET-UP GOBLOCK FOR GETOPTS                   
*                                                                               
         GOTO1 GETOPT,DMCB,GOBLOCK                                              
*                                                                               
         LA    R3,6                GET INDEX FOR CATEGORY                       
         CLI   TXTCATH+5,0         IS THERE A CATEGORY ?                        
         BNE   DEF060              YES                                          
         LA    R3,7                NO, GET INDEX FOR WORKCODE                   
         CLI   TXTWRKH+5,0         IS THERE A WORKCODE ?                        
         BNE   DEF060              YES                                          
*                                                                               
         LA    R1,WHERTBL          NO, FIND WHICH PANEL CODE TO USE             
*                                                                               
DEF020   CLC   QWHER,0(R1)                                                      
         BE    DEF040                                                           
         LA    R1,L'WHERTBL(R1)                                                 
         CLI   0(R1),0                                                          
         BNE   DEF020                                                           
*                                                                               
         MVC   8(L'QPANEL,R2),DEFPAN                                            
         B     DEF080                                                           
*                                                                               
DEF040   SR    R3,R3               GET INDEX VALUE                              
         IC    R3,2(R1)                                                         
*                                                                               
DEF060   MH    R3,=H'4'            MULTIPLY BY FIELD LENGTH                     
         LA    R3,GOPANBLK(R3)                                                  
         CLI   QFORM,C'B'                                                       
         BNE   *+8                                                              
         LA    R3,GOPANBIL-GOPANETI(R3)                                         
         MVC   8(L'QPANEL,R2),0(R3)                                             
*                                                                               
DEF080   LA    R1,4                GET MAXIMUM LENGTH PANEL CODE                
         LA    R3,8+L'QPANEL-1(R2) GET TO LAST BYTE                             
*                                                                               
DEF100   CLI   0(R3),C' '                                                       
         BNE   DEFX                                                             
         BCTR  R3,0                                                             
         BCT   R1,DEF100                                                        
*                                                                               
DEFX     STC   R1,5(R2)                                                         
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* VALIDATE ALL FIELDS ON SCREEN (AFTER DISPLAY)                       *         
***********************************************************************         
*                                                                               
SETVAL   NTR1                                                                   
         L     R2,AFSTFLD                                                       
         SR    R3,R3                                                            
         ICM   R3,1,NUMFLDS                                                     
         BZ    SETVALX                                                          
*                                                                               
         OI    4(R2),X'20'                                                      
         BAS   RE,BUMP                                                          
         BCT   R3,*-8                                                           
*                                                                               
SETVALX  B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* SETUP GOBLOCK PARAMETERS FOR CALL TO GETOPTS.                       *         
***********************************************************************         
*                                                                               
SETGO    NTR1                                                                   
         MVC   GOADM,DATAMGR                                                    
         LA    R1,GOXBLOCK                                                      
         ST    R1,GOAEXT                                                        
         LA    R1,GOPANBLK                                                      
         ST    R1,GOAPAN                                                        
         XC    GOACLI,GOACLI                                                    
         XC    GOAPRO,GOAPRO                                                    
         XC    GOAJOB,GOAJOB                                                    
         XC    GOACOMP,GOACOMP                                                  
         MVC   GOSELCUL,CUL                                                     
         MVC   GOSELOFC,QOFF                                                    
         MVC   GOSELOG,QOFG                                                     
         MVC   GOSELCLI,QCLI                                                    
         MVC   GOSELPRO,QPROD                                                   
         MVC   GOSELJOB,QJOB                                                    
         MVC   GOSELMG,QMGR                                                     
         MVC   GOSELMED,QMED                                                    
         MVI   GOWHICH,0                                                        
         MVI   GOANYMED,0                                                       
         MVI   GOSELLEV,0                                                       
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* SET ADDRESS OF FIRST AND LAST FIELD ON SCREEN.                      *         
***********************************************************************         
*                                                                               
SETFLDS  ST    RE,SAVERE                                                        
         LA    R2,TXTOGRH          POINT TO FIRST HEADER FIELD                  
         ST    R2,AFSTFLD                                                       
         CLI   NUMFLDS,X'00'       ANY DATA ?                                   
         BE    SETF020             NO, USE AS FIRST AND LAST FIELD              
*                                                                               
         LA    R2,TXTTAGH          GET ADDRESS OF FIRST FIELD                   
         CLC   QPANEL,DEFPAN       REGULAR COMMENT ?                            
         BNE   *+8                 NO                                           
         BAS   RE,BUMP             YES, SKIP OVER HEADER                        
         ST    R2,AFSTFLD                                                       
*                                                                               
         SR    R1,R1                                                            
         ICM   R1,1,NUMFLDS                                                     
         BZ    SETF020                                                          
         BAS   RE,BUMP                                                          
         BCT   R1,*-4                                                           
*                                                                               
SETF020  ST    R2,ALSTFLD          GET ADDRESS OF LAST FIELD                    
         L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
SETATTR  NTR1                                                                   
         L     R2,AFSTFLD                                                       
         SR    R3,R3                                                            
         ICM   R3,1,NUMFLDS                                                     
         BZ    SETAX                                                            
*                                                                               
SETA02   CLI   0(R2),0             END OF FIELDS?                               
         BE    SETA04              YES                                          
         XI    1(R2),X'20'         REVERSE BIT                                  
         OI    6(R2),X'80'         TRANSMIT FIELD                               
         BAS   RE,BUMP             GO TO DATA                                   
         BAS   RE,BUMP             GO TO NEXT ACT                               
         BCTR  R3,0                MINUS 1 FOR ACT                              
         BCT   R3,SETA02           MINUS 1 FOR DATA                             
*                                                                               
SETA04   XI    ACTSTAT,X'80'       REVERSE INDICATOR ALSO                       
*                                                                               
SETAX    B     XIT                                                              
         EJECT                                                                  
BUMPTOUN SR    R3,R3               ROUTINE TO GET TO NEXT UNPROTECTED           
         ICM   R3,1,NUMFLDS          FIELDS                                     
         BZR   RE                                                               
*                                                                               
BUMPT020 SR    RF,RF                                                            
         IC    RF,0(R2)                                                         
         AR    R2,RF                                                            
         CLI   0(R2),0                                                          
         BER   RE                                                               
         TM    1(R2),X'20'                                                      
         BNOR  RE                                                               
         BCT   R3,BUMPT020                                                      
         LA    R2,TXTOGRH          NONE FOUND, POINT TO OFFICE GROUP            
         BR    RE                  RETURN                                       
         SPACE 3                                                                
BUMP     ZIC   RF,0(R2)            BUMP TO NEXT FIELD                           
         AR    R2,RF                                                            
         BR    RE                  RETURN                                       
         EJECT                                                                  
GETELIO  L     R6,AIO                                                           
         GETEL (R6),DATADISP,ELCODE                                             
         SPACE 3                                                                
SCRERR   MVI   ERROR,SCREENER      INDICATE ERROR                               
*                                                                               
ERREXIT  GOTO1 VERRCUR                                                          
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
DEFPAN   DC    CL4'9999'                                                        
BLANKS   DC    CL132' '                                                         
DELMSG   DC    CL60'RECORD DELETED'                                             
NOMSG    DC    CL60'NO DATA TO DISPLAY'                                         
NOSMSG   DC    CL60'NO SCREEN TO DISPLAY, PRINT DEFINITION ONLY'                
DISMSG   DC    CL60'DATA DISPLAYED'                                             
LINKMSG  DC    CL60'STANDARD COMMENT LINKED'                                    
EDTMSG   DC    CL60'RECORD UPDATED'                                             
PANCONMG DC    CL60'PANEL      ALREADY EXISTS - HIT PF10 TO DELETE'             
         SPACE 3                                                                
PFDATA   DC    X'01',AL1(7+78),AL1(0),AL1(2),AL1(78),X'28',AL1(0)               
         DC    CL78'PF1=OPT MAINT PF2=PANEL LIST PF5=JOB EST PF7=BACKWAX        
               RD PF8=FORWARD PF12=RETURN'                                      
         DC    X'00'                                                            
*                                                                               
BLKLINE  DC    X'01',AL1(TWAELLNQ),AL1(0),AL1(2),AL1(77),X'20',AL1(0)           
         DC    X'00'                                                            
*                                                                               
COMTITL  DC    X'01',AL1(7+78),AL1(1),AL1(2),AL1(78),X'28',AL1(0)               
COMTLEN  DC    CL78' '                                                          
         ORG   COMTLEN                                                          
         DC    X'C183A35F40C39694948595A340E385A7A35F'                          
         ORG   ,                                                                
         DC    X'00'                                                            
*                                                                               
COMDATA  DC    X'01',AL1(TWAELLNQ),AL1(1),AL1(2),AL1(03),X'20',AL1(0)           
         DC    X'01',AL1(TWAELLNQ),AL1(0),AL1(7),AL1(72),X'08',AL1(0)           
         DC    X'00'                                                            
*                                                                               
COMMPF   DC    X'01',AL1(7+70),AL1(1),AL1(2),AL1(70),X'28',AL1(0)               
         DC    CL70'I=INSERT; D=DELETE; PF1=OPTION MAINT; PF2=PANEL LISX        
               T; PF5=JOB EST'                                                  
         DC    X'01',AL1(7+70),AL1(1),AL1(2),AL1(70),X'28',AL1(0)               
         DC    CL70'PF7=BACKWARD; PF8=FORWARD; PF9=PROTECT/UNPROTECT; PX        
               F12=RETURN'                                                      
         DC    X'00'                                                            
*                                                                               
         SPACE 3                                                                
WHERTBL  DS    0CL3                                                             
         DC    C'T ',AL1(0)                                                     
         DC    C'TS',AL1(1)                                                     
         DC    C'H ',AL1(2)                                                     
         DC    C'HS',AL1(3)                                                     
         DC    C'F ',AL1(4)                                                     
         DC    C'FS',AL1(5)                                                     
         DC    AL1(0)                                                           
         SPACE 3                                                                
SCRLTAB  DS    0CL2                                                             
         DC    C'T ',C'ST',C'H ',C'SH',C'F ',C'SF'                              
         DC    AL1(0)                                                           
         SPACE 3                                                                
RELO     DC    A(0)                                                             
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*DDTWABLDD                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDTWABLDD                                                      
         PRINT ON                                                               
*DDSPOOLD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
         PRINT ON                                                               
*DDSPLWORKD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDSPLWORKD                                                     
         PRINT ON                                                               
*ACGENBOTH                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
         PRINT ON                                                               
*ACGENFILE                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
*DDBIGBOX                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDBIGBOX                                                       
         PRINT ON                                                               
*ACPROWORKD                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACPROWORKD                                                     
         PRINT ON                                                               
         ORG   CONTAGH                                                          
         EJECT                                                                  
       ++INCLUDE ACPROC8D                                                       
         DS    0F                                                               
*                                                                               
         ORG   T60BFFD+2304                                                     
QKEY     DS    0CL(QKEYLEN)                                                     
QOFG     DS    CL(L'ACTXOG)                                                     
QOFF     DS    CL(L'EFFOFFC)                                                    
QCLI     DS    CL(L'ACTXCLI)                                                    
QPROD    DS    CL(L'ACTXPROD)                                                   
QJOB     DS    CL(L'ACTXJOB)                                                    
QMGR     DS    CL(L'ACTXMG)                                                     
QMED     DS    CL(L'ACTXMED)                                                    
QCAT     DS    CL(L'ACTXCAT)                                                    
QWORK    DS    CL(L'ACTXWORK)                                                   
QSUFFIX  DS    CL(L'ACTXSUFF)                                                   
QREQLEN  EQU   *-QOFG                                                           
QFORM    DS    CL(L'ACTXFORM)                                                   
QWHER    DS    CL(L'ACTXWHER)                                                   
QPANEL   DS    CL(L'ACTHPAN)                                                    
QCOMMENT DS    CL(L'ACTHCOM)                                                    
QKEYLEN  EQU   *-QOFG                                                           
*                                                                               
NUMFLDS  DS    X                   # OF FIELDS ON SCREEN                        
NKEYS    DS    X                   NUMBER OF ENTRIES IN KEYWORD TABLE           
NDATA    DS    X                   NUMBER OF ENTRIES WITH DATA                  
ERRSWT   DS    C                   PANEL CODE ERROR INDICATOR                   
PFSWT    DS    C                   PFKEY RESET INDICATOR                        
THISACT  DS    C                   ACTION CODE                                  
ACTSTAT  DS    X                   ATTRIBUTE INDICATOR FOR 'ACT'                
LASTLINE DS    X                   LAST LINE OF DATA ON SCREEN                  
*                                                                               
GOTSCRL  DS    CL(L'GOTXTSCR)                                                   
GOTSCRLE DS    XL1                                                              
*                                                                               
ABUFFER  DS    A                   ADDRESS OF BUFFER WHERE TWA IS BUILT         
*                                                                               
KEYTAB   DS    XL(KEYLEN*MAXKEYS)  KEYWORD TABLE                                
*                                                                               
FSTMODE  EQU   1                   FIRST TIME INDICATOR                         
DISMODE  EQU   2                   DISPLAY INDICATOR                            
MAXLINE  EQU   13                  MAXIMUM LINES OF DATA ON A SCREEN            
PFLINE   EQU   24                                                               
KEYLEN   EQU   (L'ACTFFLD+1)                                                    
MAXKEYS  EQU   150                 MAXIMUM NUMBER OF KEYWORDS IN TABLE          
         EJECT                                                                  
*                                                                               
* DSECT TO COVER LOCAL WORKING STORAGE                                          
*                                                                               
SUBSYSD  DSECT                                                                  
         ORG   DRGEN               USE THE DRONEBLK AREA                        
LOCAL    DS    0C                                                               
KEYCHG   DS    C                                                                
DELSW    DS    C                   DELETED RECORD INDICATOR                     
FOUND    DS    C                   INDICATOR TO STOP RECORD ADD                 
INTMODE  DS    X                   INTERNAL MODE                                
NUMSCRL  DS    X                   NUMBER OF TABLE ENTRIES                      
SAVERE   DS    A                   SAVE RE FOR SUBROUTINES                      
AFSTFLD  DS    A                   ADDRESS OF FIRST SCREEN FIELD                
ALSTFLD  DS    A                   ADDRESS OF LAST SCREEN FIELD                 
*                                                                               
LASTEND  DS    H                                                                
LASTATTB DS    X                                                                
LASTROW  DS    X                                                                
LASTCOL  DS    X                                                                
THISROW  DS    X                                                                
THISCOL  DS    X                                                                
*                                                                               
TWAPARM  DS    XL(TWAPARML)                                                     
TWAELEM  DS    XL(TWAELLNQ+80)                                                  
         EJECT                                                                  
*                                                                               
       ++INCLUDE ACPANBLOCK                                                     
*                                                                               
       ++INCLUDE ACGOBLOCK                                                      
*                                                                               
       ++INCLUDE ACGOXBLOCK                                                     
LOCALLN  EQU   *-LOCAL                                                          
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'097ACPRO38   04/11/07'                                      
         END                                                                    
