*          DATA SET ACCAP11    AT LEVEL 045 AS OF 03/02/21                      
*PHASE T61D11C                                                                  
*                                                                               
***********************************************************************         
*                                                                     *         
*  TITLE:        ACCAP11 -- STANDARD AND EDIT HOURS MAINT/LIST        *         
*                                                                     *         
*  COMMENTS:     MAINTAINS STANDARD AND EDIT HOURS RECORDS            *         
*                                                                     *         
*  CALLED FROM:  CAP CONTROLLER (T61D00), WHICH CALLS                 *         
*                GEGENCON (T00A30), WHICH CALLS THIS.                 *         
*                                                                     *         
*  INPUTS:       SCREENS ACCAPE2 (MAINTENANCE)                        *         
*                        ACCAPE1 (LIST)                               *         
*                                                                     *         
*  OUTPUTS:      UPDATED STD AND EDIT HOURS RECS, LIST                *         
*                                                                     *         
*  REGISTERS:    R0 -- WORK                                           *         
*                R1 -- WORK                                           *         
*                R2 -- SCREEN FIELD HEADER                            *         
*                R3 -- WORK                                           *         
*                R4 -- WORK                                           *         
*                R5 -- THIRD BASE                                     *         
*                R6 -- GETEL REGISTER                                 *         
*                R7 -- SECOND BASE                                    *         
*                R8 -- SPOOLD                                         *         
*                R9 -- SYSD                                           *         
*                RA -- TWA                                            *         
*                RB -- FIRST BASE                                     *         
*                RC -- GEND                                           *         
*                RD -- SYSTEM                                         *         
*                RE -- SYSTEM                                         *         
*                RF -- SYSTEM                                         *         
*                                                                     *         
*NSHE 040 011205 CHANGES TO IMPROVE EDIT HOURS FOR DAILY TIME         *         
*FPEA 041 200405 <1023945> - BUG IN EDIT HOURS ADD FIXED              *         
*DKEL 042 040506 ADDED FILTERS TO EDT HOURS/STD HOURS SCREEN (LR)     *         
*TKLU 043 090507 <BR10743D> APPLY OFFICE SECURITY                     *         
*TFRY 044 030708 <LO01-7861> MAINTAIN DISPLAY TARGET UTILISATION PCT  *         
*RKEJ 045 082420 SPEC-47885  DS - COST/EDIT HRS ISSUE FIX             *         
***********************************************************************         
         TITLE 'T61D11 - STANDARD AND EDIT HOURS MAINT/LIST'                    
T61D11   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**1D11**,R7,R5,RR=R3                                           
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING T61DFFD,RA          BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         ST    R3,RELO                                                          
         L     R1,=A(LITVALS)                                                   
         AR    R1,R3                                                            
         MVC   GLOBALS(GLOBALSL),0(R1)                                          
         L     R1,=A(LPFTABLE)                                                  
         A     R1,RELO                                                          
         ST    R1,ALPFTAB                                                       
         L     R1,=A(DAYTAB)                                                    
         A     R1,RELO                                                          
         ST    R1,ADAYTAB                                                       
         L     R1,=A(DPFTABLE)                                                  
         A     R1,RELO                                                          
         ST    R1,ADPFTAB                                                       
         L     R1,=A(PPFTABLE)                                                  
         A     R1,RELO                                                          
         ST    R1,APPFTAB                                                       
         CLI   TWASCR2,X'EC'       IS IT THE EDIT HRS DAILY SCRN?               
         BNE   INIT00                                                           
         CLI   ACTEQU,ACTLIST      IS IT AN ACTION OF LIST?                     
         BE    INIT00                                                           
         LA    R1,DLYSELH          MAKE AFRSTREC=DLYSEL                         
         ST    R1,AFRSTREC                                                      
*                                                                               
INIT00   OI    GENSTAT2,RETEQSEL   (FOR PAGING)                                 
         GOTO1 DICTATE,DMCB,C'L   ',DCLISTL,DSLISTL                             
         SR    R2,R2                                                            
         LA    R3,STLPFKYH                                                      
         L     R2,ALPFTAB          LIST PFKEYS                                  
         TM    TRANSTAT,RCHANG     RECORD TYPE CHANGED?                         
         BZ    *+8                 IF SO CLEAR TWASCR2                          
         MVI   TWASCR2,0                                                        
         CLI   ACTEQU,ACTLIST                                                   
         BNE   INIT08                                                           
         MVI   TWASCR2,0                                                        
         MVI   RUNINDS,0                                                        
*                                                                               
         MVI   STLDLYH+FLDATB-FLDHDRD,FATBHIGH+FATBPROT+FATBLC                  
         MVI   STLDAYTH+FLDATB-FLDHDRD,FATBPROT+FATBLC                          
         MVI   STLDAYH+FLDATB-FLDHDRD,0                                         
         CLI   RECNUM,RTEDT        EDIT HOURS                                   
         BE    INIT02                                                           
         OI    STLDLYH+FLDATB-FLDHDRD,FATBLOW  HIDE DAILY FIELDS IN STD         
         OI    STLDAYTH+FLDATB-FLDHDRD,FATBLOW                                  
         OI    STLDAYH+FLDATB-FLDHDRD,FATBLOW+FATBPROT                          
INIT02   OI    STLDLYH+FLDOIND-FLDHDRD,FOUTTRN TRANSMIT CHANGES                 
         OI    STLDAYH+FLDOIND-FLDHDRD,FOUTTRN                                  
         OI    STLDAYTH+FLDOIND-FLDHDRD,FOUTTRN                                 
*                                                                               
         B     INITPFKY                                                         
INIT08   SR    R3,R3                                                            
         SR    R2,R2                                                            
         CLI   PFKEY,8                                                          
         BE    INITPFKY                                                         
         CLI   PFKEY,7                                                          
         BE    INITPFKY                                                         
         L     R2,APPFTAB          MAINT PFKEYS                                 
         CLI   ACTEQU,ACTCHA                                                    
         BE    INIT10                                                           
         CLI   ACTEQU,ACTDIS                                                    
         BE    INIT10                                                           
         CLI   ACTEQU,ACTADD                                                    
         BE    INIT10                                                           
         CLI   ACTEQU,ACTSEL                                                    
         BNE   INITPFKY                                                         
*                                                                               
INIT10   SR    R1,R1                                                            
         LA    R2,STDDLYH                                                       
         IC    R1,FLDILEN-FLDLEN(R2)                                            
         LTR   R1,R1                                                            
         BZ    INIT20                                                           
         CLC   STDDLY(1),AC@YES                                                 
         BNE   INIT20                                                           
         XR    R3,R3                                                            
         OI    RUNINDS,RUNIDALY    SET RUNNING UNDER DAILY MODE                 
*                                                                               
         L     R2,ADPFTAB                                                       
         CLI   TWASCR2,X'EC'                                                    
         BE    INITPFKY                                                         
         MVI   DMCB+7,X'EC'                                                     
         B     INIT22                                                           
*                                                                               
INIT20   L     R2,APPFTAB                                                       
         MVI   RUNINDS,0                                                        
         XR    R3,R3                                                            
         CLI   TWASCR2,X'EB'                                                    
         BE    INITPFKY                                                         
         MVI   DMCB+7,X'EB'                                                     
         MVC   BOTSCR,DMCB+7                                                    
INIT22   MVC   DMCB+4(3),=X'D9061D'                                             
         MVC   TWASCR2,DMCB+7                                                   
         GOTO1 CALLOV,DMCB,(0,STDTAGH)                                          
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                KILL IF CAN'T LOAD SCREEN                    
         LA    R1,CONHEADH         RE-TRANSMIT ALL HEADER FIELDS                
         XR    RE,RE                                                            
         LA    RF,STDTAGH-1                                                     
         ICM   RE,1,0(R1)                                                       
         BZ    *+12                                                             
         OI    FLDOIND-FLDHDRD(R1),FOUTTRN                                      
         BXLE  R1,RE,*-12                                                       
         LA    R1,STDCOL1+L'STDCOL1                                             
         ST    R1,AFRSTREC                                                      
         CLI   TWASCR2,X'EB'                                                    
         BE    INITPFKY                                                         
         LA    R1,DLYSELH                                                       
         ST    R1,AFRSTREC                                                      
*                                                                               
INITPFKY GOTO1 INITIAL,DMCB,(X'40',(R2)),(R3)   INITIALIZE THE PFKEYS           
         MVC   BOTSCR,TWASCR2                                                   
         CLI   ACTEQU,ACTLIST                                                   
         BE    INIT34                                                           
         MVI   STDDLYTH+FLDATB-FLDHDRD,FATBHIGH+FATBPROT+FATBLC                 
         NI    STDDLYH+FLDATB-FLDHDRD,X'FF'-FATBPROT                            
         OI    STDYEARH+FLDOIND-FLDLEN,FOUTCUR                                  
         CLI   RECNUM,RTEDT        EDIT HOURS                                   
         BE    INIT32                                                           
         OI    STDDLYTH+FLDATB-FLDHDRD,FATBLOW                                  
         XC    STDDLY,STDDLY                                                    
         OI    STDDLYH+FLDATB-FLDHDRD,FATBPROT                                  
INIT32   OI    STDDLYTH+FLDOIND-FLDHDRD,FOUTTRN                                 
         OI    STDDLYH+FLDOIND-FLDHDRD,FOUTTRN                                  
INIT34   CLI   TWASCR2,X'EC'                                                    
         BNE   TSTMODES                                                         
         LA    R2,DLYSELH                                                       
         USING DAYLINED,R2                                                      
         OI    DLYPCTH+FLDOIND-FLDLEN,FOUTCUR  SET CURSOR                       
INIT36   CLI   0(R2),L'DLYPFKYH+L'DLYPFKY                                       
         BE    INIT40                                                           
         NI    FLDATB-FLDHDRD(R2),X'FF'-FATBPROT UN/PROT DAYS/HOURS             
         CLI   ACTEQU,ACTADD                     ACCORDING TO ACTION            
         BNE   INIT38                                                           
         OI    FLDATB-FLDHDRD(R2),FATBPROT                                      
INIT38   OI    FLDOIND-FLDHDRD(R2),FOUTTRN                                      
         LA    R2,DAYLINEL(R2)                                                  
         B     INIT36                                                           
*                                                                               
INIT40   MVI   FLDATB-FLDHDRD(R2),FATBHIGH+FATBPROT                             
         MVC   FLDDATA-FLDHDRD(L'AC@PFKE9,R2),AC@PFKE9                          
         CLI   ACTEQU,ACTADD                                                    
         BNE   INIT42                                                           
         OI    FLDATB-FLDHDRD(R2),FATBLOW                                       
INIT42   OI    FLDOIND-FLDHDRD(R2),FOUTTRN                                      
*                                                                               
TSTMODES CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VK                                                               
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VR                                                               
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    DK                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DR                                                               
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         BE    LR                                                               
         CLI   MODE,XRECADD        AFTER AN ADD - UPDATE STATUS                 
         BE    XP                                                               
         CLI   MODE,XRECPUT        AFTER AN PUT                                 
         BE    XP                                                               
*                                                                               
XYES     SR    RC,RC                                                            
XNO      LTR   RC,RC                                                            
XIT      XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
*        VALIDATE KEY                                                           
***********************************************************************         
*                                                                               
VK       DS    0H                                                               
         XC    CALKEY,CALKEY                                                    
         MVC   OFFICE,SPACES                                                    
         MVC   DEPT,SPACES                                                      
         MVC   SUBDPT,SPACES                                                    
         MVC   PERSON,SPACES                                                    
         MVC   DAILYFLG,SPACES                                                  
         MVC   ACCNT,SPACES                                                     
         MVI   SEQNUM,0                                                         
         MVI   BITS,0                                                           
         CLI   RECNUM,RTEDT        EDIT HOURS                                   
         BE    *+14                                                             
         MVC   CONREC(8),AC@STDHU                                               
         OI    CONRECH+6,X'80'                                                  
         TM    TRANSTAT,RACHANG                                                 
         BZ    VK03                                                             
         MVI   STPER,X'01'         START ON FIRST PAGE                          
VK03     CLI   ACTEQU,ACTDIS                                                    
         BE    *+10                                                             
         ZAP   TOTPAGE1,=P'0'                                                   
         MVI   BITS2,0                                                          
         GOTO1 GETLDG,DMCB,C'1R'   GET APPROPRIATE FIELD LENGTHS                
         TM    LDGROFP,X'40'                                                    
         BZ    *+8                                                              
         OI    BITS2,BITS22CO                                                   
         MVC   OFFDISP,LDGROFP     LEDGER OFFICE POSITION                       
         NI    OFFDISP,X'FF'-X'40' TURN OFF 2 CHAR BIT                          
         MVC   LEVELLN,ABCDLEN                                                  
         BAS   RE,VALOPTS          VALIDATE ANY OPTIONS                         
         CLI   ACTEQU,ACTLIST                                                   
         BE    VK50                LIST EVERYTHING                              
         EJECT                                                                  
***********************************************************************         
*        VALIDATE YEAR                                                          
***********************************************************************         
*                                                                               
VK05     LA    R2,STDYEARH                                                      
         BAS   RE,VALYR                                                         
         EJECT                                                                  
***********************************************************************         
*        VALIDATE OFFICE/DEPT/SUBDPT/PERSON - CAN ENTER UP TO ANY LEVEL         
*        CHECK THAT REC EXIST FOR LOWER LEVELS                                  
***********************************************************************         
*                                                                               
VK10     MVC   ACCNT,SPACES                                                     
         LA    R4,ACCNT            BUILD ACCOUNT IN ACCNT TO READ 1R            
*                                                                               
         USING EDTRECD,R6                                                       
         LA    R6,KEY2             CHECK FOR LOW LEVEL STD REC                  
         MVC   EDTKEY,SPACES                                                    
         MVI   EDTKTYP,STDKTYPQ    STANDARD HOURS RECORD                        
         MVI   EDTKSUB,STDKSUBQ                                                 
*                                                                               
         CLI   RECNUM,RTEDT        EDIT HOURS                                   
         BNE   *+12                                                             
         MVI   EDTKTYP,EDTKTYPQ    EDIT HOURS RECORD                            
         MVI   EDTKSUB,EDTKSUBQ                                                 
*                                                                               
         MVC   EDTKCPY,CMPY        COMPANY                                      
         MVC   EDTKSEQ,SEQNUM                                                   
         MVC   EDTKYR,CALDATE      YEAR - NEW FIELD                             
         TM    RUNINDS,RUNIDALY    ARE WE RUNNING UNDER DAILY MODE              
         BZ    VK12                                                             
         MVI   EDTKKSTA,EDTKSDAY   SET DAILY TYPE KEY                           
*                                                                               
VK12     LA    R2,STDOFFH          OFFICE                                       
         CLI   5(R2),0             ANY DATA?                                    
         BE    VK15                                                             
         LA    R1,8(R2)                                                         
         BAS   RE,OFFACC                                                        
         BNE   EINVSECL                                                         
         BAS   RE,CKFORSTD         CHECK FOR AGENCY DEFAULT LEVEL               
         CLC   STDOFFH+5(1),LEVELLN         SHOULD = LEN LEVEL A ACC            
         BH    ETOOLONG                                                         
         BL    ETOOSHRT                                                         
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         MVC   OFFICE(0),STDOFF    SAVE OFFICE                                  
         EX    R1,*-6                                                           
         MVC   0(0,R4),STDOFF      BUILD ACCOUNT TO READ 1R                     
         EX    R1,*-6                                                           
         LA    R4,1(R1,R4)                                                      
         OI    BITS,YESOFF                                                      
*                                                                               
VK15     LA    R2,STDDEPTH         ANY DEPARTMENT                               
         CLI   5(R2),0                                                          
         BE    VK20                                                             
         TM    BITS,YESOFF         MUST HAVE ENTERED AN OFFICE                  
         BZ    EMISHIGH                                                         
         CLC   5(1,R2),LEVELLN+1                                                
         BH    ETOOLONG                                                         
         BL    ETOOSHRT                                                         
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         MVC   DEPT(0),8(R2)                                                    
         EX    R1,*-6                                                           
         MVC   0(0,R4),8(R2)       R4 POINTS TO RIGHT PLACE IN ACCNT            
         EX    R1,*-6              BUILD ACCOUNT TO READ 1R                     
         LA    R4,1(R1,R4)                                                      
         OI    BITS,YESDPT                                                      
*                                                                               
VK20     LA    R2,STDSDPTH         ANY SUB DEPT FILTER                          
         CLI   5(R2),0                                                          
         BE    VK25                                                             
         TM    BITS,YESOFF+YESDPT  MUST HAVE HIGHER LEVELS                      
         BNO   EMISHIGH                                                         
         CLC   5(1,R2),LEVELLN+2                                                
         BH    ETOOLONG                                                         
         BL    ETOOSHRT                                                         
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         MVC   SUBDPT(0),8(R2)                                                  
         EX    R1,*-6                                                           
         MVC   0(0,R4),8(R2)       R4 POINTS TO RIGHT PLACE IN ACCNT            
         EX    R1,*-6              BUILD ACCOUNT TO READ 1R                     
         LA    R4,1(R1,R4)                                                      
         OI    BITS,YESSBDPT                                                    
*                                                                               
VK25     LA    R2,STDPERH          ANY PERSON                                   
         CLI   5(R2),0                                                          
         BE    VK26                                                             
         TM    BITS,YESOFF+YESDPT+YESSBDPT                                      
         BNO   EMISHIGH                                                         
         CLC   5(1,R2),LEVELLN+3                                                
         BH    ETOOLONG                                                         
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         MVC   PERSON(0),8(R2)                                                  
         EX    R1,*-6                                                           
         MVC   0(0,R4),8(R2)       R4 POINTS TO RIGHT PLACE IN ACCNT            
         EX    R1,*-6              BUILD ACCOUNT TO READ 1R                     
         OI    BITS,YESPERSN                                                    
*                                                                               
VK26     LA    R2,STDOFFH          CURSOR AT OFFICE ON ERROR                    
         BAS   RE,VALACCNT         VALIDATE ENTIRE ACCOUNT                      
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*        READ FOR TIMESHEET CALENDAR RECORD TO DISP                             
***********************************************************************         
*                                                                               
         BAS   RE,GETCAL                                                        
         TM    RUNINDS,RUNIDALY                                                 
         BNZ   VK50                                                             
         TM    TRANSTAT,RACHANG    RECORD OR ACTION CHANGE                      
*        BZ    VK50                                                             
         BO    *+12                                                             
         TM    BITS,CALDISPD       IS CALENDAR DISPLAYED YET                    
         BO    VK50                                                             
         BAS   RE,DISPCAL          DISPLAY CALENDAR                             
         EJECT                                                                  
***********************************************************************         
*        BUILD KEY                                                              
***********************************************************************         
*                                                                               
         USING EDTRECD,R6                                                       
VK50     LA    R6,BIGKEY                                                        
         MVC   EDTKEY,SPACES                                                    
         MVI   EDTKTYP,STDKTYPQ    STANDARD HOURS RECORD                        
         MVI   EDTKSUB,STDKSUBQ                                                 
*                                                                               
         CLI   RECNUM,RTEDT        EDIT HOURS                                   
         BNE   VK55                                                             
         MVI   EDTKTYP,EDTKTYPQ    EDIT HOURS RECORD                            
         MVI   EDTKSUB,EDTKSUBQ                                                 
         TM    RUNINDS,RUNIDALY                                                 
         BZ    VK55                                                             
         MVI   EDTKKSTA,EDTKSDAY                                                
*                                                                               
VK55     MVC   EDTKCPY,CMPY        COMPANY                                      
         MVC   EDTKOFC,OFFICE                                                   
         MVC   EDTKDPT,DEPT                                                     
         MVC   EDTKSBD,SUBDPT                                                   
         MVC   EDTKPER,PERSON                                                   
         MVC   EDTKYR,CALDATE      YEAR - NEW FIELD                             
         MVC   EDTKSEQ,SEQNUM                                                   
*                                                                               
         CLC   SAVEKEY(L'ACTKEY),BIGKEY                                         
         BE    VK65                                                             
         CLI   ACTEQU,ACTADD                                                    
         BE    VK65                                                             
         CLI   ACTEQU,ACTLIST                                                   
         BE    VK65                                                             
         BAS   RE,CLRSCR           CLEAR SCREEN                                 
VK60     TM    RUNINDS,RUNIDALY                                                 
         BNZ   VK65                                                             
         MVC   STDTOT,SPACES       CLEAR TOTAL                                  
         OI    STDTOTH+6,X'80'                                                  
*                                                                               
VK65     MVC   SAVEKEY,BIGKEY                                                   
         CLI   ACTEQU,ACTLIST      IS IT AN ACTION OF LIST?                     
         BE    XIT                                                              
         LA    R1,DLYPCTH          SET CURSOR TO BE PCT FIELD                   
         ST    R1,AFRSTREC                                                      
         B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*        VALIDATES YEAR AND RETURNS 2 BYTE PWOS YEAR IN WORK                    
*        R2 MUST BE POINTING AT FIELD HEADER                                    
***********************************************************************         
*                                                                               
         USING PERVALD,R3                                                       
VALYR    NTR1                                                                   
         XC    CALDATE,CALDATE     DATE TO READ FOR CALENDAR                    
         LA    R3,BLOCK                                                         
         GOTO1 PERVAL,DMCB,(5(R2),8(R2)),(X'62',BLOCK)                          
         CLI   DMCB+4,X'04'        ONLY ONE DATE INPUT                          
         BE    VY10                                                             
         CLI   DMCB+4,X'03'        TODAYS DATE RETURNED ON NO INPUT             
         BE    VY05                                                             
         CLI   DMCB+4,X'00'        EVERYTHING OK                                
         BE    VY10                                                             
         B     EINVYR                                                           
*                                                                               
VY05     MVC   CALDATE,PVALPSTA    TODAY'S DATE                                 
         B     *+10                                                             
VY10     MVC   CALDATE(1),PVALPSTA     PWOS YEAR                                
         B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
*        GET CALENDAR RECORD                                                    
***********************************************************************         
*                                                                               
GETCAL   NTR1                                                                   
         LA    R2,STDYEARH         FOR ERROR TO POINT TO YEAR                   
         XC    CALSTART,CALSTART   INIT                                         
         XC    CALEND,CALEND                                                    
         MVI   CALFLAG,0                                                        
         CLI   STPER,0             ENSURE SOMETHING HAS BEEN PUT THERE          
         BNE   *+8                                                              
         MVI   STPER,1             MAKE PERIOD ONE THE DEFAULT                  
*                                                                               
         USING CASRECD,R6          READ FOR CALENDAR                            
         LA    R6,BIGKEY                                                        
         XC    BIGKEY,BIGKEY                                                    
         MVI   CASKTYP,CASKTYPQ    X'3E'                                        
         MVI   CASKSUB,CASKSUBQ    X'0B'                                        
         MVC   CASKCPY,CMPY        COMPANY                                      
         MVC   CASKEMOA,CALDATE    DATE TO READ HIGH FOR                        
         GOTO1 HIGH                                                             
         CLC   BIGKEY(CASKEMOA-CASKEY),KEYSAVE     SAME COMP                    
         BNE   ENOCAL                                                           
         CLI   CALDATE+1,0         COMPARE ON YEAR IF 0                         
         BNE   GC04                                                             
         CLC   CASKEMOA(1),CALDATE                                              
         BH    ENOCAL                                                           
GC04     MVC   CALKEY,BIGKEY                                                    
*                                                                               
GC05     CLC   CASKEMOA(1),CALDATE    SAME DATE                                 
         BNE   GCSEQ                                                            
         OC    OFFICE,OFFICE                                                    
         BZ    GC10                                                             
         CLC   OFFICE,CASKOFC      SAME OFFICE                                  
         BNE   GCSEQ                                                            
         MVC   CALKEY,BIGKEY                                                    
         B     GC10                                                             
*                                                                               
GCSEQ    GOTO1 SEQ                                                              
         CLC   BIGKEY(CASKSMOA-CASKEY-2),CALKEY      SAME YR                    
         BE    GC05                                                             
*                                                                               
GC10     MVC   AIO,AIO3                                                         
         XC    BIGKEY,BIGKEY                                                    
         MVC   BIGKEY(L'ACTKEY),CALKEY                                          
         GOTO1 READ                                                             
         GOTO1 GETREC                                                           
         MVC   AIO,AIO1                                                         
*                                                                               
         L     R6,AIO3                                                          
         MVC   CALYEAR,CASKEMOA    END MONTH IS YEAR                            
*                                                                               
         USING TMRELD,R6           SAVE CALENDAR START AND END DATES            
         MVI   ELCODE,TMRELQ       TIMESHEET RULES ELEM                         
         BAS   RE,GETEL            GET START YEAR                               
         BNE   GC20                                                             
*                                                                               
         MVC   CALSTART,TMRSTART                                                
         MVC   CALEND,TMREND                                                    
*                                                                               
         USING TMPELD,R6           SAVE CALENDAR START AND END DATES            
GC20     L     R6,AIO3                                                          
         MVI   ELCODE,TMPELQ       TIMESHEET PERIOD ELEMS                       
         LA    R1,54               SET BIT IF FIND 54TH PERIOD                  
         STC   R1,WORK                                                          
         BAS   RE,GETEL            ARE THERE MORE THAN 53 PERIODS               
         B     GC30                                                             
*                                                                               
GC30NX   BAS   RE,NEXTEL                                                        
GC30     BNE   GCX                                                              
         CLC   TMPNUMB,WORK                                                     
         BL    GC30NX                                                           
         OI    CALFLAG,CALSPLIT    THERE ARE 2 PAGES TO CALENDAR                
*                                                                               
GCX      MVC   8(4,R2),SPACES      DISPLAY                                      
         MVC   WORK+10(1),CALYEAR                                               
         MVC   WORK+11(2),=X'0101'                                              
         MVI   BYTE,20                                                          
         OI    BYTE,X'20'          DON'T OUTPUT FUNNY YEARS                     
         GOTO1 DATCON,DMCB,(1,WORK+10),(BYTE,WORK)                              
         MVC   8(4,R2),WORK                                                     
*        CLC   WORK+6(2),=C'80'  IF YEAR IS < 80 MUST BE AFTER 2000             
*        BL    *+14                                                             
*        MVC   8(2,R2),=C'19'                                                   
*        B     *+10                                                             
*        MVC   8(2,R2),=C'20'                                                   
*        MVC   10(2,R2),WORK+6      CHARACTER YEAR                              
         MVI   5(R2),X'04'                                                      
         OI    6(R2),X'80'                                                      
         B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*        VALIDATE OFFICE,DEPT,SUBDPT,PERSON IN 1R                               
***********************************************************************         
*                                                                               
         USING ACTRECD,R4                                                       
VALACCNT NTR1                      VALIDATE LOCATION ENTEREDD                   
         LA    R4,KEY2             BUILD KEY                                    
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,CMPY        COMPANY                                      
         MVI   ACTKUNT,C'1'        UNIT 1                                       
         MVI   ACTKLDG,C'R'        LEDGER R                                     
         MVC   ACTKACT,ACCNT                                                    
         MVC   KEYSAVE,KEY2                                                     
         GOTO1 DATAMGR,DMCB,DMRDHI,ACCDIR,KEY2,KEY2,0                           
         CLC   KEY2(L'ACTKEY),KEYSAVE                                           
         BNE   EINVACC             INVALID ACCOUNT                              
*                                                                               
         USING EMPELD,R6                                                        
         L     R6,AIO2             MAKE SURE NOT SET UP FOR ACTUAL HRS          
         MVI   ELCODE,EMPELQ       EMPLOYEE HISTORY ELEMENT                     
         BAS   RE,GETEL                                                         
         BNE   VAX                                                              
         ST    R6,SVADDR                                                        
VA10NX   BAS   RE,NEXTEL           LOOK FOR MOST RECENT ELEM (LAST)             
         BNE   VA20                                                             
         ST    R6,SVADDR                                                        
         B     VA10NX                                                           
VA20     L     R6,SVADDR                                                        
         TM    EMPSTAT,EMPSACT     ACTUAL HOURS                                 
         BO    EACTHRS                                                          
VAX      B     XIT                                                              
         DROP  R4,R6                                                            
         EJECT                                                                  
***********************************************************************         
*        TURN OFF DELETE BITS                                                   
***********************************************************************         
         SPACE 1                                                                
DELOFF   NTR1                                                                   
         USING STDRECD,R6                                                       
         L     R6,AIO                                                           
         NI    STDRSTAT,X'FF'-X'80'                                             
         LA    R6,BIGKEY                                                        
         NI    STDKSTAT,X'FF'-X'80'                                             
         B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*        CHECK RECORD FOR ELEMENTS                                              
***********************************************************************         
         SPACE 1                                                                
ANYELEMS NTR1                                                                   
         USING STDRECD,R6          CHECK FOR STD HOURS                          
         L     R6,AIO              YEAR ELEMENT                                 
         NI    STDRSTAT,X'FF'-X'80' MAKE SURE NOT DELETED                       
         MVI   ELCODE,SHRELQ                                                    
         TM    RUNINDS,RUNIDALY                                                 
         BZ    AELEMS02                                                         
         MVI   ELCODE,DEDELQ                                                    
AELEMS02 BAS   RE,GETEL                                                         
         BE    XIT                                                              
*                                                                               
         L     R6,AIO                                                           
         USING STDRECD,R6                                                       
         OI    STDRSTAT,X'80'      MARK REC FOR DELETION                        
         LA    R6,BIGKEY                                                        
         OI    STDKSTAT,X'80'                                                   
         B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*        VALIDATE OFFICE ACCESS/SECURITY                                        
***********************************************************************         
*                                                                               
         USING OFFALD,R3                                                        
OFFACC   NTR1                                                                   
         L     R3,AOFFBLK                                                       
         LR    R2,R1                                                            
         TM    BITS2,BITS22CO      ONE BYTE OFFICES?                            
         BNZ   OFF02                                                            
         OC    OFFAOLDA,OFFAOLDA   OLD OFFICE LIMIT ACCESS                      
         BZ    OFFYES              NO-UNLIMITED ACCESS                          
         B     OFF04                                                            
*                                                                               
OFF02    OC    OFFANEWA,OFFANEWA   NEW OFFICE LIMIT ACCESS                      
         BZ    OFFYES              NO-UNLIMITED ACCESS                          
*                                                                               
OFF04    LA    R3,OFFAWORK         LIST OF VALID OFFICE CODES                   
         TM    BITS2,BITS22CO      AGENCY HAS ONE BYTE OFFICES?                 
         BZ    OFF06                                                            
         LH    RF,0(R3)            RF=# OF OFFICES IN LIST                      
         LA    R3,2(R3)            BUMP PAST COUNT OF OFFICES                   
         B     *+8                                                              
*                                                                               
OFF06    LA    RF,OLDOFLMX*2       MAX # IN LIST*2 (2 PAGES)                    
         ZIC   R1,LEVELLN                                                       
         BCTR  R1,0                                                             
*                                                                               
OFF08    EXCLC R1,0(R2),0(R3)                                                   
         BE    OFFYES                                                           
         LA    R3,1(R1,R3)                                                      
         TM    BITS2,BITS22CO      ONE BYTE OFFICES?                            
         BZ    OFF10                                                            
         BCT   RF,OFF08                                                         
         B     OFFNO                                                            
*                                                                               
OFF10    CLI   0(R3),0             END OF LIST                                  
         BE    OFFNO                                                            
         BCT   RF,OFF08                                                         
*                                                                               
OFFNO    B     XNO                                                              
OFFYES   B     XYES                                                             
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
*        VALIDATE HOURS REC FOR LOWER LEVELS                                    
*        KEY IN KEY2                                                            
***********************************************************************         
*                                                                               
CKFORSTD NTR1                      VALIDATE LOW LEVEL STD HOUR REC              
         L     R4,AIO2                                                          
         GOTO1 DATAMGR,DMCB,DMREAD,ACCDIR,KEY2,(R4)                             
         CLC   KEY2(L'ACTKEY),0(R4)                                             
         BNE   EMISSDF             MISSING DEFAULT LEVEL                        
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*        DISPLAYS CALENDAR IN TIMESHEET CAL RECORD                              
***********************************************************************         
*                                                                               
DISPCAL  NTR1                                                                   
         XC    SCRSTART,SCRSTART   SCREEN START AND END DATES                   
         XC    SCREND,SCREND                                                    
         XC    SVMONTH,SVMONTH                                                  
         XC    STYEAR,STYEAR                                                    
         OI    BITS,CALDISPD       CALENDAR DISPLAYED                           
*                                  CLEAR ALL PROTECTED                          
         LA    R2,STDCOL1H         FIRST FIELD                                  
         LA    R3,STDTOTH          LAST FIELD                                   
DCCLR    TM    1(R2),X'20'         PROTECTED?                                   
         BNO   DCCLR10             NO,  THEN SKIP                               
         ZIC   R1,0(R2)            FIELD LENGTH                                 
         SH    R1,=H'9'            8 FOR HEADER, 1 FOR EX                       
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),SPACES                                                   
         OI    6(R2),X'80'         TRANSMIT                                     
DCCLR10  ZIC   R1,0(R2)                                                         
         AR    R2,R1                                                            
         CR    R2,R3               ARE WE DONE                                  
         BL    DCCLR               NO                                           
*                                                                               
DC10     TM    CALFLAG,CALSPLIT    ARE THERE 2 PAGES TO CALENDAR                
         BNO   DC12                                                             
         CLI   PFKEY,0             IF NOT ENTER CLEAR SCREEN HOURS              
         BE    DC14                                                             
         BAS   RE,CLRSCR           CLEAR SCREEN                                 
         MVC   STDTOT,SPACES       CLEAR TOTAL                                  
         OI    STDTOTH+6,X'80'                                                  
         CLI   PFKEY,7             PAGE UP                                      
         BE    DC11                                                             
         CLI   PFKEY,8             PAGE DOWN                                    
         BNE   DC14                                                             
         LA    R1,54                                                            
         STC   R1,STPER            NEXT PAGE BEGINS WITH 54                     
         OI    CALFLAG,CALDISP                                                  
         B     DC14                                                             
*                                                                               
DC11     OI    CALFLAG,CALDISP                                                  
DC12     MVI   STPER,X'01'                                                      
DC14     NI    CALFLAG,X'FF'-CALPAGE2                                           
         CLI   STPER,X'36'         54TH PERIOD                                  
         BNE   *+8                                                              
         OI    CALFLAG,CALPAGE2                                                 
         L     R6,AIO3                                                          
         USING TMPELD,R6                                                        
         MVI   ELCODE,TMPELQ       TIMESHEET PERIODS ELEM                       
         BAS   RE,GETEL                                                         
         B     DC18                                                             
*                                                                               
DC18NX   BAS   RE,NEXTEL                                                        
DC18     BNE   DCX                                                              
         CLC   TMPNUMB,STPER                                                    
         BNE   DC18NX                                                           
         MVC   SCRSTART,TMPSTART   SAVE SCREEN START AND END                    
         GOTO1 DATCON,DMCB,(1,TMPSTART),(11,WORK)                               
         MVC   STYEAR,WORK+6       CHAR START YEAR                              
*                                                                               
DC20     LA    R2,STDCOL1H         COLUMN 1 HEADER                              
DC25     BAS   RE,DISPPER          DISPLAY NEXT ELEM                            
         BAS   RE,BUMP6            NEXT IN COLUM                                
         BAS   RE,NEXTEL           NEXT ELEM                                    
         BNE   DCX                                                              
         LA    R1,STDEND1H                                                      
         CR    R2,R1                                                            
         BNH   DC25                                                             
*                                                                               
         LA    R2,STDCOL2H         COLUMN 2 HEADER                              
DC30     BAS   RE,DISPPER          DISP TIMESHEET PERIOD ELEM                   
         BAS   RE,BUMP6            NEXT IN COLUM                                
         BAS   RE,NEXTEL           NEXT ELEM                                    
         BNE   DCX                                                              
         LA    R1,STDEND2H                                                      
         CR    R2,R1                                                            
         BNH   DC30                                                             
*                                                                               
         LA    R2,STDCOL3H         COLUMN 3 HEADER                              
DC35     BAS   RE,DISPPER          DISP TIMESHEET PERIOD ELEM                   
         BAS   RE,BUMP6            NEXT IN COLUM                                
         BAS   RE,NEXTEL           NEXT ELEM                                    
         BNE   DCX                                                              
         LA    R1,STDEND3H                                                      
         CR    R2,R1                                                            
         BNH   DC35                                                             
DCX      ST    R2,SVADDR           SAVE R2 FOR TOTAL LINE                       
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*        DISPPER - DISPLAYS TIMESHEET PERIOD ELEM                               
***********************************************************************         
*                                                                               
         USING CALLINED,R2         R2 = SCREEN LINE                             
         USING TMPELD,R6           R6 = ELEMENT                                 
DISPPER  NTR1                                                                   
         CLC   SVMONTH,TMPMTH+1                                                 
         BE    DP20                IF SAME MONTH DON'T DISPLAY                  
         XC    WORK,WORK           GET MONTH                                    
         MVC   WORK(L'TMPMTH),TMPMTH                                            
         MVI   WORK+2,X'01'        DOESN'T REALLY MATTER                        
         GOTO1 DATCON,DMCB,(1,WORK),(11,EBDATE)                                 
         MVC   CALMON,EBDATE       DISPLAYS MONTH                               
*                                                                               
DP20     MVI   BYTE,10             OUTPUT TYPE                                  
         CLI   LANGCODE,3                                                       
         BNE   *+8                                                              
         MVI   BYTE,17             OUTPUT TYPE FOR GERMANY                      
         GOTO1 DATCON,DMCB,(1,TMPSTART),(BYTE,WORK)                             
         MVC   CALSTDT,WORK                                                     
         MVI   CALHYPH,C'-'                                                     
         GOTO1 DATCON,DMCB,(1,TMPEND),(BYTE,WORK)                               
         MVC   CALENDT,WORK                                                     
*                                  DISPLAY NUMBER                               
         EDIT  (B1,TMPNUMB),(2,CALNUM),FILL=0                                   
*                                                                               
         MVC   SVMONTH,TMPMTH+1    SAVE MONTH                                   
         MVC   SCREND,TMPEND       SAVE LAST DATE ON SCREEN                     
         B     XIT                                                              
         DROP  R2,R6                                                            
         EJECT                                                                  
***********************************************************************         
*        VALIDATE RECORD                                                        
***********************************************************************         
*                                                                               
VR       NI    BITS,X'FF'-LAST                                                  
         NI    BITS,X'FF'-NOTFIRST                                              
         OI    GENSTAT2,RETEQSEL                                                
         BAS   RE,VALOPTS          VALIDATE ANY OPTIONS                         
*                                                                               
         CLI   ACTEQU,ACTADD                                                    
         BE    VRA                                                              
         CLI   ACTEQU,ACTDIS                                                    
         BE    VRA                                                              
         CLI   ACTEQU,ACTSEL                                                    
         BNE   *+14                                                             
         CLC   THISLSEL,AC@CHAU    WAS CHANGE SELECTED FROM LIST?               
         BNE   VRA                                                              
         TM    TRANSTAT,RCHANG     HAS RECORD CHANGED?                          
         BO    DR                                                               
*                                                                               
VRA      XC    BIGKEY,BIGKEY                                                    
         MVC   BIGKEY(L'SAVEKEY),SAVEKEY                                        
         OI    DMINBTS,X'08'       PASS BACK DELETED RECS                       
         GOTO1 HIGH                                                             
         CLC   BIGKEY(L'ACTKEY),KEYSAVE    NEW REC                              
         BNE   VR01                                                             
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         USING STDRECD,R6                                                       
         BAS   RE,DELOFF                                                        
         B     VR01A                                                            
*                                                                               
VR01     L     RE,AIO                                                           
         LA    RF,LIOS                                                          
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
         L     R6,AIO                                                           
         MVC   0(L'ACTKEY,R6),KEYSAVE                                           
*                                                                               
VR01A    BAS   RE,REMYEAR          REMOVE ELEMS FOR YEAR                        
         TM    OPTSTAT,OPTSREM     JUST REMOVING YEAR                           
         BZ    VR02                                                             
         BAS   RE,ANYELEMS                                                      
         B     VR40                                                             
*                                                                               
VR02     TM    OPTSTAT,OPTSHRS     HOURS OPTION                                 
         BNO   VR06                                                             
         TM    RUNINDS,RUNIDALY                                                 
         BNZ   VR04                                                             
         XC    ELEM,ELEM                                                        
         USING SHRELD,R6                                                        
         LA    R6,ELEM                                                          
         MVI   SHREL,SHRELQ                                                     
         MVI   SHRLN,SHRLNQ                                                     
         MVC   SHRSTART,CALSTART                                                
         MVC   SHREND,CALEND                                                    
         ZAP   SHRHOURS(4),HOURS(4)                                             
         GOTO1 ADDELEM                                                          
         B     VR40                                                             
         DROP  R6                                                               
*                                                                               
VR04     L     R3,ADAYTAB                                                       
VR05     CLI   0(R3),X'FF'                                                      
         BE    VR40                                                             
         XC    ELEM,ELEM                                                        
         USING DEDELD,R6                                                        
         LA    R6,ELEM                                                          
         MVI   DEDEL,DEDELQ                                                     
         MVI   DEDLN,DEDLN1Q                                                    
         MVC   DEDIND,0(R3)                                                     
         ZAP   DEDHRS(4),HOURS(4)                                               
         OI    GENSTAT5,ADDEQCOD                                                
         GOTO1 ADDELEM                                                          
         LA    R3,1(R3)                                                         
         B     VR05                                                             
         DROP  R6                                                               
*                                                                               
VR06     ZAP   SVHOURS,=P'0'                                                    
         TM    RUNINDS,RUNIDALY                                                 
         BZ    VR08                                                             
         BAS   RE,TARPCT           VALIDATE TARGET PCT                          
                                                                                
         LA    R2,DLYSELH                                                       
         L     R3,ADAYTAB                                                       
VR07     CLI   0(R3),X'FF'                                                      
         BE    VR40                                                             
         BAS   RE,VALDYHRS                                                      
         LA    R2,DAYLINEL(R2)                                                  
         LA    R3,1(R3)                                                         
         B     VR07                                                             
                                                                                
VR08     LA    R2,STDCOL1H         COLUMN 1 HEADER                              
VR10     BAS   RE,VALHOURS         VALIDATE HOURS                               
         TM    BITS,LAST           ALL DONE                                     
         BO    VR40                                                             
         BAS   RE,BUMP6            NEXT IN COLUM                                
         LA    R1,STDEND1H                                                      
         CR    R2,R1                                                            
         BNH   VR10                                                             
*                                                                               
         LA    R2,STDCOL2H         COLUMN 2 HEADER                              
VR20     BAS   RE,VALHOURS         VALIDATE HOURS                               
         TM    BITS,LAST                                                        
         BO    VR40                                                             
         BAS   RE,BUMP6            NEXT IN COLUM                                
         LA    R1,STDEND2H                                                      
         CR    R2,R1                                                            
         BNH   VR20                                                             
*                                                                               
         LA    R2,STDCOL3H         COLUMN 3 HEADER                              
VR30     BAS   RE,VALHOURS         VALIDATE HOURS                               
         TM    BITS,LAST                                                        
         BO    VR40                                                             
         BAS   RE,BUMP6            NEXT IN COLUM                                
         LA    R1,STDEND3H                                                      
         CR    R2,R1                                                            
         BNH   VR30                                                             
*                                                                               
VR32     OI    BITS,LAST                                                        
         BAS   RE,VALHOURS         GO ADD LAST ELEM                             
*                                                                               
VR40     MVI   IOOPT,C'Y'                                                       
         NI    GENSTAT5,X'FF'-ADDEQCOD                                          
         L     R6,AIO                                                           
         MVC   BIGKEY(L'ACTKEY),0(R6)                                           
         OI    DMINBTS,X'08'       READ FOR DELETES                             
         GOTO1 HIGH                                                             
         CLC   BIGKEY(L'ACTKEY),KEYSAVE                                         
         BNE   VR50                GO ADD THIS REC                              
         MVC   AIO,AIO2                                                         
         GOTO1 GETREC                                                           
         BAS   RE,DELOFF                                                        
         L     R6,AIO2                                                          
         CLC   BIGKEY(L'ACTKEY),0(R6)                                           
         BNE   VRX                                                              
         MVC   AIO,AIO1                                                         
         BAS   RE,ANYELEMS         CHECK IF SHOULD BE DELETED                   
VR48     GOTO1 PUTREC                                                           
         GOTO1 WRITE                                                            
         B     VRX                                                              
*                                                                               
VR50     GOTO1 ADDREC                                                           
*                                                                               
VRX      MVI   IOOPT,C'Y'          ALREADY DID OWN IO                           
         B     DR                                                               
         EJECT                                                                  
***********************************************************************         
*        VALHOURS - VALIDATES HOURS AND ADDS ELEMENTS IF NECESSARY              
***********************************************************************         
*                                                                               
         USING CALLINED,R2         R2 = SCREEN LINE                             
         USING SHRELD,R6           R6 = ELEMENT                                 
VALHOURS NTR1                                                                   
         TM    BITS,LAST                                                        
         BO    VH20                                                             
         CLC   AC@TOTU,CALMON      TOTAL LINE =DONE                             
         BE    VH01A                                                            
         OC    CALSTDT,CALSTDT     ANY CALENDAR DATE ON LINE?                   
         BZ    VH01A                                                            
         CLC   CALSTDT,SPACES      ANY CALENDAR DATE ON LINE?                   
         BNE   VH01                                                             
VH01A    OI    BITS,LAST                                                        
         B     VH20                NO, THEN ADD LAST ELEM                       
*                                                                               
VH01     XC    WORK,WORK                                                        
         TM    BITS,NOTFIRST       FIRST PERIOD?                                
         BO    VH02                NO                                           
*                                                                               
         MVC   WORK(5),CALSTDT    FIRST PERIOD START DATE HAS YEAR              
         MVI   WORK+5,C'/'                                                      
         MVC   WORK+6(2),STYEAR   CHARACTER YEAR                                
         MVC   WORK+8(6),CALSTDT+5                                              
         B     VH03                                                             
*                                                                               
VH02     DS    0H                  MAKE SURE START DATE HAS YEAR                
         MVC   WORK(5),CALSTDT                                                  
         MVI   WORK+5,C'/'                                                      
         MVC   WORK+6(2),NXTYEAR   CHARACTER YEAR                               
         MVC   WORK+8(6),CALHYPH   END DATE                                     
*                                                                               
VH03     DS    0H                  VALIDATE                                     
         MVC   BYTE,LANGCODE                                                    
         LA    R1,14               LENGTH                                       
         STC   R1,BYTE2                                                         
         OI    BYTE2,X'40'         ASSUME DDMM NOT MMYY                         
         GOTO1 PERVAL,DMCB,(BYTE2,WORK),(BYTE,BLOCK)                            
         LA    R3,BLOCK                                                         
         USING PERVALD,R3                                                       
*                                                                               
         MVC   NEWSTART,PVALPSTA                                                
         MVC   NEWEND,PVALPEND                                                  
*                                                                               
         XC    WORK,WORK                                                        
         GOTO1 DATCON,DMCB,(1,PVALPEND),(11,WORK)                               
         MVC   WORK+8(5),=C'-(1D)' ADD ONE DAY                                  
         CLI   LANGCODE,3                                                       
         BNE   *+10                                                             
         MVC   WORK+8(5),=C'-(1T)' ADD ONE DAY = T IN GERMAN                    
         XC    BLOCK(L'PVALOUTB),BLOCK                                          
         MVC   BYTE,LANGCODE                                                    
         LA    R1,13               LENGTH                                       
         STC   R1,BYTE2                                                         
         OI    BYTE2,X'40'         ASSUME DDMM NOT MMYY                         
         GOTO1 PERVAL,DMCB,(BYTE2,WORK),(BYTE,BLOCK)                            
         LA    R3,BLOCK                                                         
*                                    CONVERT FA TO F0, FB TO F1, ETC.           
         GOTO1 DATCON,DMCB,(0,PVALEEND),(X'20',PVALEEND)                        
*        CLI   PVALEEND,C'9'                                                    
*        BNH   VH04                                                             
*        SR    R1,R1                                                            
*        IC    R1,PVALEEND                                                      
*        SH    R1,=H'10'                                                        
*        STC   R1,PVALEEND                                                      
VH04     MVC   NXTYEAR,PVALEEND    CHAR YEAR FOR NEXT PERIOD START DATE         
         DROP  R3                                                               
*                                                                               
         CLI   CALHOURH+5,0        VALIDATE HOURS                               
         BNE   VH05                                                             
         LA    R2,CALHOURH                                                      
         B     ERRMISS                                                          
*                                                                               
VH05     ZIC   R0,CALHOURH+5                                                    
         GOTO1 CASHVAL,DMCB,(X'82',CALHOUR),(R0)                                
         CLI   DMCB,X'FF'                                                       
         BE    VHERR                                                            
         ZAP   HOURS(4),DMCB+8(4)                                               
         CP    HOURS(4),=P'999.99'   C'MON, CAN'T HAVE THAT MANY HOURS          
         BL    VH10                                                             
VHERR    LA    R2,CALHOURH                                                      
         B     EINVHRS                                                          
*                                                                               
VH10     DS    0H                  IF HOURS CHANGE ADD ELEMENT                  
         TM    BITS,NOTFIRST                                                    
         BNO   VH30                SKIP COMPARE ON FIRST TIME                   
         CP    HOURS(4),SVHOURS(4)     SAME HOURS?                              
         BE    VH40                    YES, UPDATE END DATE                     
*                                                                               
VH20     DS    0H                                                               
         XC    ELEM,ELEM                                                        
         LA    R6,ELEM                                                          
         MVI   SHREL,SHRELQ                                                     
         MVI   SHRLN,SHRLNQ                                                     
         MVC   SHRSTART,PERSTART                                                
         MVC   SHREND,PEREND                                                    
         ZAP   SHRHOURS(4),SVHOURS(4)                                           
*                                                                               
         TM    BITS,LAST           IF LAST PERIOD ON SCREEN                     
         BNO   VH25                                                             
         TM    BITS,NOELEMS        AND ADDING NEW YEAR                          
         BNO   VH25                                                             
         CLC   SHREND,CALEND       MAKE END DATE= CALENDAR END DATE             
         BE    VH25                                                             
         MVC   SHREND,CALEND                                                    
*                                                                               
VH25     GOTO1 ADDELEM                                                          
*                                                                               
VH30     ZAP   SVHOURS(4),HOURS(4)                                              
         MVC   PERSTART,NEWSTART                                                
VH40     MVC   PEREND,NEWEND                                                    
         OI    BITS,NOTFIRST                                                    
VHX      B     XIT                                                              
         DROP  R2,R6                                                            
         EJECT                                                                  
***********************************************************************         
* VALIDATE DAILY HOURS                                                *         
***********************************************************************         
         SPACE 1                                                                
         USING DAYLINED,R2         R2 = SCREEN LINE                             
         USING DEDELD,R6           R6 = ELEMENT                                 
VALDYHRS NTR1  ,                                                                
         CLI   DAYHRSH+5,0        VALIDATE HOURS                                
         BNE   VD05                                                             
         LA    R2,DAYHRSH                                                       
         B     ERRMISS                                                          
*                                                                               
VD05     CLI   ACTEQU,ACTADD       IF ADD DON'T CHECK THIS TIME INPUT           
         BE    VD07                                                             
         TM    DAYHRSH+(FLDIIND-FLDHDRD),FINPTHIS                               
         BZ    VDX                                                              
VD07     ZIC   R0,DAYHRSH+5                                                     
         GOTO1 CASHVAL,DMCB,(X'82',DAYHRS),(R0)                                 
         CLI   DMCB,X'FF'                                                       
         BE    VDERR                                                            
         ZAP   HOURS(4),DMCB+8(4)                                               
         CP    HOURS(4),=P'24.00'   C'MON, CAN'T HAVE THAT MANY HOURS           
         BL    VD10                                                             
VDERR    LA    R2,DAYHRSH                                                       
         B     EINVHRS                                                          
*                                                                               
VD10     XC    ELEM,ELEM                                                        
         LA    R6,ELEM                                                          
         MVI   DEDEL,DEDELQ                                                     
         MVI   DEDLN,DEDLN1Q                                                    
         MVC   DEDIND,0(R3)                                                     
         ZAP   DEDHRS(4),HOURS(4)                                               
         OI    GENSTAT5,ADDEQCOD                                                
         GOTO1 ADDELEM                                                          
         NI    GENSTAT5,X'FF'-ADDEQCOD                                          
*                                                                               
VDX      B     XIT                                                              
         DROP  R2,R6                                                            
         EJECT                                                                  
***********************************************************************         
* VALIDATE TARGET UTILISATION PERCENTAGE                                        
***********************************************************************         
         USING FFRELD,R6                                                        
TARPCT   NTR1  ,                                                                
         CLI   DLYPCTH+5,0         ANYTHING INPUT?                              
         BE    TPX                                                              
         CLI   ACTEQU,ACTADD       ACTION OF ADD?                               
         BE    TP01                                                             
         TM    DLYPCTH+(FLDIIND-FLDHDRD),FINPTHIS                               
         BZ    TPX                                                              
TP01     SR    R0,R0                                                            
         IC    R0,DLYPCTH+5                                                     
         GOTO1 CASHVAL,DMCB,(X'80',DLYPCT),(X'C0',(R0))                         
         CLI   DMCB,X'FF'          VALID VALUE?                                 
         BE    TPERR                                                            
         ZAP   PERCENT(4),DMCB+8(4)                                             
         CP    PERCENT,P100        IS IT LESS THAN 100?                         
         BH    TPERR                                                            
         CP    PERCENT,PZERO       IS IT MORE THAN ZERO?                        
         BL    TPERR                                                            
TP02     L     R6,AIO                                                           
         MVI   ELCODE,FFRELQ                                                    
         BAS   RE,GETEL            IS THERE ALREADY A RATE?                     
         BNE   TP05                                                             
TP03     CLI   FFRTYPE,FFRTPCT     IS IT A PERCENTAGE?                          
         BE    TP04                                                             
         BAS   RE,NEXTEL                                                        
         BNE   TP05                                                             
         B     TP03                                                             
TP04     CP    PERCENT,FFRPCT      IS IT THE SAME VALUE?                        
         BE    TPX                 -YES EXIT                                    
         ZAP   FFRPCT,PERCENT      REPLACE OLD PCT WITH NEW PCT                 
         B     TPX                                                              
*                                                                               
TP05     XC    ELEM,ELEM           ADD ELEMENT                                  
         LA    R6,ELEM                                                          
         MVI   FFREL,FFRELQ                                                     
         MVI   FFRLN,FFRLN1Q+L'FFRPCT                                           
         MVI   FFRTYPE,FFRTPCT                                                  
         ZAP   FFRPCT,PERCENT                                                   
         OI    GENSTAT5,ADDEQCOD                                                
         GOTO1 ADDELEM                                                          
         NI    GENSTAT5,X'FF'-ADDEQCOD                                          
         B     TPX                                                              
*                                                                               
TPERR    LA    R2,DLYPCTH          INVALID PERCENTAGE ERROR                     
         B     EINVPCT                                                          
*                                                                               
TPX      B     XIT                                                              
         DROP  R6                                                               
*                                                                               
PZERO    DC    P'0'                                                             
P100     DC    P'100'                                                           
*                                                                               
         EJECT                                                                  
***********************************************************************         
*        CHECK FOR OPTION REMOVE TO REMOVE ALL HOURS FOR THAT YEAR              
***********************************************************************         
*                                                                               
VALOPTS  NTR1                                                                   
         MVI   OPTSTAT,0                                                        
         LA    R2,CONOPTH                                                       
         CLI   5(R2),0             ANYTHING?                                    
         BE    OPTX                                                             
*                                                                               
         XC    BLOCK(200),BLOCK                                                 
         GOTO1 SCANNER,DMCB,(R2),BLOCK                                          
         CLI   DMCB+4,0                                                         
         BE    EINVOPT                                                          
*                                                                               
         USING SCANBLKD,R3                                                      
         LA    R3,BLOCK                                                         
         B     OPT10                                                            
*                                                                               
OPT10NX  LA    R3,SCBLKLQ(R3)                                                   
         CLI   0(R3),0                                                          
         BE    OPTX                                                             
*                                                                               
OPT10    ZIC   R1,SC1STLEN                                                      
         SH    R1,=H'1'                                                         
         BM    EINVOPT                                                          
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   SC1STFLD(0),AC@HOURU  ALL PERIODS SET TO HOURS GIVEN             
         BE    OPT40                                                            
         CLC   AC@HRSU,SC1STFLD      ALL PERIODS SET TO HOURS GIVEN             
         BE    OPT40                                                            
         CLC   AC@REMU,SC1STFLD      REMOVE HOURS FOR YEAR OPTION               
         BE    OPT50                                                            
         B     EINVOPT                                                          
         EJECT                                                                  
***********************************************************************         
*        HOURS= OPTION                                                          
*        WILL FORCE EVERY PERIOD IN YEAR TO SPECIFIED HOURS                     
***********************************************************************         
*                                                                               
OPT40    ZIC   R0,SC2NDLEN                                                      
         GOTO1 CASHVAL,DMCB,(X'82',SC2NDFLD),(R0)                               
         CLI   DMCB,X'FF'                                                       
         BE    EINVHRS                                                          
         ZAP   HOURS(4),DMCB+8(4)                                               
         TM    RUNINDS,RUNIDALY    ARE WE ON DAILY TIME                         
         BNZ   OPT42               YES                                          
         CP    HOURS(4),=P'999.99'   C'MON, CAN'T HAVE THAT MANY HOURS          
         BH    EINVHRS                                                          
         OI    OPTSTAT,OPTSHRS                                                  
         CLI   ACTEQU,ACTADD                                                    
         BNE   OPT10NX                                                          
         LA    R1,STDCOL1H                                                      
         USING CALLINED,R1         R2 = SCREEN LINE                             
         MVI   CALHOURH+5,1        MUST PUT A LENGTH IN TO FORCE                
         B     OPT10NX             GENCON  TO CALL WITH VALREC                  
         DROP  R1                                                               
*                                                                               
OPT42    CP    HOURS(4),=P'24.00'  DAY, CAN'T HAVE THAT MANY HOURS              
         BH    EINVHRS                                                          
         OI    OPTSTAT,OPTSHRS                                                  
         CLI   ACTEQU,ACTADD                                                    
         BNE   OPT10NX                                                          
         MVI   DLYHRSH+5,1         MUST PUT A LENGTH IN TO FORCE                
         B     OPT10NX             GENCON  TO CALL WITH VALREC                  
         EJECT                                                                  
***********************************************************************         
*        REMOVE OPTION                                                          
*        REMOVE IS ONLY VALID FOR AGY LEVEL IF THERE ARE NO HOURS FOR           
*        YEAR ON ANY LOWER LEVEL RECORDS                                        
***********************************************************************         
*                                                                               
OPT50    CLI   ACTEQU,ACTCHA       ONLY VALID WITH ACTION CHANGE                
         BNE   EREMCH                                                           
         OI    OPTSTAT,OPTSREM     SET BIT                                      
         ZAP   TOTPAGE1,=P'0'      CLEAR TOTAL                                  
*                                                                               
         TM    BITS,YESOFF         IS THIS A LOWER LEVEL REC                    
         BO    OPT10NX                                                          
*                                                                               
         USING EDTRECD,R6          BUILD KEY                                    
         LA    R6,KEY2                                                          
         MVC   EDTKEY,SPACES       BUILD STD HOURS RECORD KEY                   
         MVI   EDTKTYP,STDKTYPQ    RECORD TYPE                                  
         MVI   EDTKSUB,STDKSUBQ    SUB RECORD TYPE                              
*                                                                               
         CLI   RECNUM,RTEDT        EDIT HOURS                                   
         BNE   OPT54                                                            
         MVI   EDTKTYP,EDTKTYPQ    EDIT HOURS RECORD                            
         MVI   EDTKSUB,EDTKSUBQ                                                 
         TM    RUNINDS,RUNIDALY    ARE WE ON DAILY TIME                         
         BZ    OPT54               NO                                           
         MVI   EDTKKSTA,EDTKSDAY                                                
*                                                                               
OPT54    MVC   EDTKCPY,CMPY        COMPANY                                      
         XC    EDTKSEQ,EDTKSEQ     SEQ NUM= X'00'                               
         MVC   KEYSAVE,KEY2                                                     
         GOTO1 DATAMGR,DMCB,DMRDHI,ACCDIR,KEY2,KEY2,0                           
         CLC   KEY2(L'ACTKEY),KEYSAVE   DEFAULT                                 
         BNE   OPT10NX                                                          
*                                                                               
OPT54SEQ GOTO1 DATAMGR,DMCB,DMRSEQ,ACCDIR,KEY2,KEY2,0                           
         CLC   KEY2(3),KEYSAVE   SAME UP TO COMPANY                             
         BNE   OPT10NX                                                          
         CLI   RECNUM,RTEDT        EDIT HOURS                                   
         BNE   OPT58                                                            
         CLI   EDTKKSTA,EDTKSDAY   HAVE WE GOT DAILY EDIT HOURS REC             
         BE    OPT56               YES                                          
         TM    RUNINDS,RUNIDALY    ARE WE ON DAILY TIME                         
         BNZ   OPT54SEQ            YES - GET NEXT RECORD                        
         B     OPT58               PROCESS RECORD                               
*                                                                               
OPT56    TM    RUNINDS,RUNIDALY    ARE WE ON DAILY TIME                         
         BZ    OPT54SEQ            NO - GET NEXT RECORD                         
         CLC   EDTKSTDT,CALSTART                                                
         BNE   OPT54SEQ                                                         
         LA    R2,CONOPTH                                                       
         B     ENOREM              MATCH, THEN CAN'T CHANGE RECORD              
                                                                                
OPT58    LA    R2,KEY2                                                          
         AH    R2,LKEY                                                          
         AH    R2,LSTATUS                                                       
         GOTO1 DATAMGR,DMCB,GETRECD,ACCMST,(R2),AIO2,WORK                       
         L     R6,AIO2                                                          
         USING SHRELD,R6                                                        
         MVI   ELCODE,SHRELQ                                                    
         BAS   RE,GETEL                                                         
         B     OPT64                                                            
*                                                                               
OPT64NX  BAS   RE,NEXTEL                                                        
OPT64    BNE   OPT54SEQ                                                         
         CLC   SHRSTART,CALSTART                                                
         BH    OPT64NX             ALREADY PASSED DATE                          
         CLC   SHREND,CALSTART                                                  
         BL    OPT64NX                                                          
         LA    R2,CONOPTH                                                       
         B     ENOREM              MATCH, THEN CAN'T CHANGE RECORD              
*                                                                               
OPTX     LA    R2,CONOPTH                                                       
         TM    OPTSTAT,OPTSREM+OPTSHRS   CAN'T HAVE BOTH OPTIONS                
         BO    EINVOPT                                                          
         B     XIT                                                              
         DROP  R6,R3                                                            
         EJECT                                                                  
***********************************************************************         
*        REMOVES ALL EXISTING HOURS ELEMS                                       
* IF CALENDAR ON 2 PAGES DELETE JUST THE EXISTING PAGE EXCEPT                   
* IF USER TYPES IN OPTION 'REMOVE'                                              
***********************************************************************         
*                                                                               
REMYEAR  NTR1                                                                   
         OI    BITS,NOELEMS        NO ELEMS EXIST FOR YEAR                      
         TM    RUNINDS,RUNIDALY                                                 
         BNZ   RY30                                                             
         MVC   STDATE,SCRSTART     REMOVE WHAT'S ON THE SCREEN                  
         MVC   ENDATE,SCREND                                                    
*                                                                               
         TM    CALFLAG,CALSPLIT    IS CALENDAR ON 2 SCREENS                     
         BNO   RY08                                                             
*                                                                               
         TM    OPTSTAT,OPTSREM     OPTION REMOVE                                
         BNO   RY01                                                             
         MVC   STDATE,CALSTART     THEN REMOVE FOR WHOLE CALENDAR               
         MVC   ENDATE,CALEND                                                    
         B     RY08                                                             
*                                                                               
RY01     TM    CALFLAG,CALPAGE2    ARE WE ON PAGE 2                             
         BO    RY02                THEN GET SCRSTART-1DAY                       
*                                                                               
         XC    WORK,WORK           ELSE NEED SCREND+1 DAY                       
         GOTO1 DATCON,DMCB,(1,SCREND),(0,WORK)                                  
         GOTO1 ADDAY,DMCB,WORK,WORK+10,F'1'                                     
         GOTO1 DATCON,DMCB,(0,WORK+10),(1,NXTSTART)                             
         B     RY08                                                             
*                                                                               
RY02     DS    0H                                                               
         XC    WORK,WORK           NEED SCRSTART-1 DAY                          
         GOTO1 DATCON,DMCB,(1,SCRSTART),(0,WORK)                                
         GOTO1 ADDAY,DMCB,WORK,WORK+10,F'-1'                                    
         GOTO1 DATCON,DMCB,(0,WORK+10),(1,LSTEND)                               
*                                                                               
RY08     L     R6,AIO                                                           
         USING SHRELD,R6                                                        
         MVI   ELCODE,SHRELQ       HOURS ELEM                                   
         BAS   RE,GETEL                                                         
         B     RY10                                                             
*                                                                               
RYNX     MVI   ELCODE,SHRELQ       HOURS ELEM                                   
         BAS   RE,NEXTEL                                                        
RY10     BNE   RYX                                                              
         CLC   SHREND,STDATE       ANY OVERLAP                                  
         BL    RYNX                                                             
         CLC   SHRSTART,ENDATE                                                  
         BH    RYX                                                              
         CLC   SHRSTART,STDATE                                                  
         BL    RY14                                                             
         CLC   SHREND,ENDATE                                                    
         BNH   RY20                JUST DELETE                                  
         MVC   SHRSTART,NXTSTART   REPLACE WITH START OF NEXT PAGE              
         NI    BITS,X'FF'-NOELEMS        ELEMS EXISTED FOR YEAR                 
         B     RYX                                                              
*                                                                               
RY14     CLC   SHREND,ENDATE       ELEM START IS LOWER THAN START               
*        BNL   RY20                                                             
         BH    RY20                                                             
         MVC   SHREND,LSTEND       REPLACE WITH NEW END                         
         NI    BITS,X'FF'-NOELEMS        ELEMS EXISTED FOR YEAR                 
         B     RYNX                                                             
*                                                                               
RY20     MVI   0(R6),X'FF'                                                      
         NI    BITS,X'FF'-NOELEMS        ELEMS EXISTED FOR YEAR                 
         B     RYNX                                                             
*                                                                               
RY30     MVC   STDATE,CALSTART     THEN REMOVE FOR WHOLE CALENDAR               
         MVC   ENDATE,CALEND                                                    
         USING DAYLINED,R2         R2 = SCREEN LINE                             
         LA    R2,DLYSELH          FIND SCREEN LINE FOR ELEMENT                 
         L     R3,ADAYTAB                                                       
RY32     L     R6,AIO                                                           
         USING EDTRECD,R6                                                       
         LA    R6,EDTRFST                                                       
         USING DEDELD,R6                                                        
         SR    R0,R0                                                            
*                                                                               
RY34     CLI   DEDEL,0                                                          
         BE    RY44                                                             
         CLI   DEDEL,DEDELQ       HOURS ELEM                                    
         BE    RY40                                                             
RY36     IC    R0,DEDLN                                                         
         AR    R6,R0                                                            
         B     RY34                                                             
                                                                                
RY40     TM    OPTSTAT,OPTSREM     HAVE WE OPTION REMOVE                        
         BNZ   RY50                YES - DELETE EVERYTHING                      
         TM    OPTSTAT,OPTSHRS     HOURS OPTION                                 
         BNZ   RY50                YES - DELETE EVERYTHING                      
*        CLI   DEDLN,DEDLN1Q       NO - KEEP EXCEPTION ELEMENTS                 
*        BH    RY36                                                             
         CLC   DEDIND,0(R3)        HAVE WE FOUND THE CORRECT LINE               
         BNE   RY36                NO - CONTINUE SEARCHING                      
         TM    DAYHRSH+(FLDIIND-FLDHDRD),FINPTHIS   ANY INPUT                   
         BNZ   RY50                YES - DELETE ELEMENT                         
RY44     LA    R2,DAYLINEL(R2)     NO - CONTINUE TILL END                       
         LA    R3,1(R3)                                                         
         CLI   0(R3),X'FF'         END OF TABLE                                 
         BE    RYX                                                              
         B     RY32                                                             
         DROP  R2                                                               
                                                                                
RY50     MVI   0(R6),X'FF'         MARK ELEMENTS FOR DELETION                   
         NI    BITS,X'FF'-NOELEMS                                               
         B     RY44                                                             
*                                                                               
RYX      MVI   ELCODE,X'FF'                                                     
         GOTO1 REMELEM                                                          
         B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*        DISPLAY KEY                                                            
***********************************************************************         
*                                                                               
         USING EDTRECD,R6                                                       
DK       LA    R6,BIGKEY                                                        
         MVC   SAVEKEY,BIGKEY                                                   
*                                                                               
         NI    BITS,X'FF'-YESOFF                                                
         CLC   EDTKOFC,SPACES      IS THERE AN OFFICE                           
         BE    *+8                                                              
         OI    BITS,YESOFF                                                      
         MVC   OFFICE(L'EDTKOFC),EDTKOFC                                        
*                                                                               
         MVC   STDOFF(L'EDTKOFC),EDTKOFC                                        
         OI    STDOFFH+6,X'80'                                                  
         MVC   STDDEPT(L'EDTKDPT),EDTKDPT                                       
         OI    STDDEPTH+6,X'80'                                                 
         MVC   STDSDPT(L'EDTKSBD),EDTKSBD                                       
         OI    STDSDPTH+6,X'80'                                                 
         MVC   STDPER,EDTKPER                                                   
         OI    STDPERH+6,X'80'                                                  
*                                                                               
         CLI   RECNUM,RTEDT        EDIT HOURS                                   
         BNE   DK02                                                             
         MVC   STDDLY(L'AC@NO),AC@NO                                            
         OI    STDDLYH+6,X'80'                                                  
         MVI   STDDLYH+5,L'AC@YES                                               
         CLI   EDTKKSTA,EDTKSDAY                                                
         BNE   *+10                                                             
         MVC   STDDLY(L'AC@YES),AC@YES                                          
*                                                                               
DK02     ZIC   R1,LSTCOUNT         DO THIS CODE INSTEAD OF CODE BELOW           
         SLL   R1,1                IN UK BECAUSE OF DUMP CAUSED BY              
         LA    R3,LISTTAB          LISTTAB HAVING A BAD ADDRESS.                
         AR    R3,R1                                                            
         MVI   0(R3),X'FF'                                                      
*                                  SINCE SELECTING DOESN'T DO IT                
*&&DO                                                                           
*&&US                                                                           
         L     R3,LISTADDR         ADDRESS OF LAST ENTRY+2                      
         MVI   0(R3),X'FF'         MARK END OF TABLE                            
*&&                                                                             
*&&                                                                             
*                                  SINCE SELECTING DOESN'T DO IT                
         LA    R3,LISTTAB          RESET POINTER TO BEGINNING                   
DK05     CLC   0(1,R3),SELLISTN    COMPARE FOR # IN LIST                        
         BE    DK10                                                             
         LA    R3,2(R3)            BUMP TABLE                                   
         CLI   0(R3),X'FF'         END OF TABLE                                 
         BNE   DK05                TRY NEXT LIST ENTRY                          
         LA    R2,STDYEARH         ZERO INPUT GIVES TODAY'S DATE                
         B     DK20                                                             
*                                                                               
DK10     LA    R2,STDYEARH                                                      
         XC    WORK,WORK                                                        
         MVC   WORK(1),1(R3)                                                    
         MVC   WORK+1(2),=X'0101'                                               
         GOTO1 DATCON,DMCB,(1,WORK),(23,WORK+10)                                
         MVC   8(4,R2),WORK+10     MOVE IN CHAR YEAR                            
         MVI   5(R2),4             SET LENGTH                                   
*                                                                               
DK20     BAS   RE,VALYR                                                         
         BAS   RE,GETCAL           GET CALENDAR REC                             
         TM    RUNINDS,RUNIDALY                                                 
         BNZ   DKX                                                              
         BAS   RE,DISPCAL          DISPLAY CALENDAR                             
DKX      CLI   ACTEQU,ACTSEL       ACTION SELECT                                
         BNE   *+10                                                             
         MVC   SVLSEL,THISLSEL     SAVE FIELD FOR LATER COMPARES                
         B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*        DISPLAY RECORD                                                         
***********************************************************************         
*                                                                               
DR       BAS   RE,GETCAL                                                        
         TM    RUNINDS,RUNIDALY                                                 
         BNZ   DR75                                                             
         BAS   RE,DISPCAL          DISPLAY CALENDAR                             
*                                                                               
         TM    CALFLAG,CALPAGE2                                                 
         BNO   *+14                                                             
         ZAP   YRTOTAL,TOTPAGE1                                                 
         B     *+10                                                             
         ZAP   YRTOTAL,=P'0'                                                    
*                                                                               
         NI    BITS,X'FF'-LAST                                                  
         NI    BITS,X'FF'-NOTFIRST                                              
         BAS   RE,CLRSCR           CLEAR SCREEN                                 
         MVC   STDTOT,SPACES       CLEAR TOTAL                                  
         OI    STDTOTH+6,X'80'                                                  
         EJECT                                                                  
***********************************************************************         
*        DISPLAY  ELEMENTS                                                      
***********************************************************************         
*                                                                               
         USING CALLINED,R2         R2 = SCREEN LINE                             
         L     R6,AIO              GET FIRST ELEMENT                            
         MVI   ELCODE,SHRELQ                                                    
         BAS   RE,GETEL                                                         
         BNE   DR60                                                             
*                                                                               
         LA    R2,STDCOL1H         COLUMN 1 HEADER                              
DR10     BAS   RE,DISHOURS         DISPLAY  HOURS                               
         TM    BITS,LAST           ALL DONE                                     
         BO    DR60                                                             
         BAS   RE,BUMP6            NEXT IN COLUM                                
         LA    R1,STDEND1H                                                      
         CR    R2,R1                                                            
         BNH   DR10                                                             
*                                                                               
         LA    R2,STDCOL2H         COLUMN 2 HEADER                              
DR20     BAS   RE,DISHOURS         DISPLAY  HOURS                               
         TM    BITS,LAST           ALL DONE                                     
         BO    DR60                                                             
         BAS   RE,BUMP6            NEXT IN COLUM                                
         LA    R1,STDEND2H                                                      
         CR    R2,R1                                                            
         BNH   DR20                                                             
*                                                                               
         LA    R2,STDCOL3H         COLUMN 3 HEADER                              
DR30     BAS   RE,DISHOURS         DISPLAY  HOURS                               
         TM    BITS,LAST           ALL DONE                                     
         BO    DR60                                                             
         BAS   RE,BUMP6            NEXT IN COLUM                                
         LA    R1,STDEND3H                                                      
         CR    R2,R1                                                            
         BNH   DR30                                                             
         B     DR60                                                             
*                                                                               
DR60     L     R2,SVADDR           SAVED R2 IN DISPCAL-TOTAL LINE HERE          
         TM    CALFLAG,CALSPLIT    IS THERE A PAGE 2                            
         BNO   DR70                                                             
         TM    CALFLAG,CALPAGE2    ARE WE ON PAGE 2                             
         BO    DR65                                                             
         MVC   TOTWORD(4),=C'PF8='                                              
         MVC   TOTWORD+4(4),AC@DOWN                                             
         OI    TOTHEAD+6,X'80'     XMIT                                         
         ZAP   TOTPAGE1,YRTOTAL                                                 
         B     DRX                                                              
*                                                                               
DR65     MVC   STDTOT(4),=C'PF7='                                               
         MVC   STDTOT+4(2),AC@UP                                                
         OI    STDTOTH+6,X'80'                                                  
DR70     CURED (P5,YRTOTAL),(10,TOTHOURS),2                                     
         MVC   TOTWORD(5),AC@TOTU                                               
         MVI   TOTWORD+6,C'='                                                   
         OI    TOTHEAD+6,X'80'     XMIT                                         
         NI    GENSTAT2,X'FF'-RETEQSEL      TURN OFF PAGING                     
         B     DRX                                                              
*                                                                               
DR75     BAS   RE,CLRSCR           CLEAR SCREEN                                 
                                                                                
         NI    GENSTAT2,X'FF'-RETEQSEL   RETURN WITH NEXT RECORD                
         CLI   ACTEQU,ACTSEL       ARE WE ON SELECT SCREEN                      
         BNE   DR75B               NO                                           
         CLC   SVLSEL,AC@CHAU      WAS CHANGE SELECTED?                         
         BE    DR75B               YES - CARRY ON AS NORMAL                     
         TM    RUNINDS,RUNINEXT                                                 
         BZ    DR75A                                                            
         NI    RUNINDS,X'FF'-RUNINEXT                                           
         OI    GENSTAT2,NEXTSEL                                                 
         OI    GENSTAT2,RETEQSEL                                                
         B     DR75B                                                            
DR75A    OI    GENSTAT2,RETEQSEL   RETURN WITH SAME RECORD                      
         OI    RUNINDS,RUNINEXT    SET GET NEXT SELECTION                       
         USING FFRELD,R6           R6 = ELEMENT                                 
DR75B    L     R6,AIO              GET FIRST ELEMENT                            
         MVI   ELCODE,FFRELQ                                                    
         BAS   RE,GETEL            IS THERE A TARGET UTILISATION PCT?           
*        MVI   DLYPCT,0                                                         
         BNE   DR76                                                             
         BAS   RE,DISPCT           DISPLAY TARGET UTILISATION PCT               
*                                                                               
         DROP  R6                                                               
         USING DEDELD,R6           R2 = SCREEN LINE                             
         USING DAYLINED,R2         R6 = ELEMENT                                 
DR76     L     R6,AIO              GET FIRST ELEMENT                            
         MVI   ELCODE,DEDELQ                                                    
         BAS   RE,GETEL                                                         
         BNE   DRX                                                              
*                                                                               
         L     R3,ADAYTAB                                                       
         LA    R2,DLYSELH          COLUMN 1 HEADER                              
DR80     CLI   DEDLN,DEDLN1Q       IGNORE EXCEPTION ELEMENTS                    
         BNE   DR82                                                             
         CLC   DEDIND,0(R3)                                                     
         BE    DR84                                                             
DR82     MVI   ELCODE,DEDELQ                                                    
         BAS   RE,NEXTEL                                                        
         BE    DR80                                                             
         B     DR85                                                             
                                                                                
DR84     BAS   RE,DISDHRS          DISPLAY DAILY HOURS                          
DR85     LA    R2,DAYLINEL(R2)                                                  
         LA    R3,1(R3)                                                         
         CLI   0(R3),X'FF'         DISPLAY DAILY HOURS                          
         BE    DRX                                                              
         L     R6,AIO              RESET TO GET FIRST ELEMENT                   
         MVI   ELCODE,DEDELQ                                                    
         BAS   RE,GETEL                                                         
         BNE   DRX                                                              
         B     DR80                YES AVAILABLE                                
*                                                                               
DRX      XC    CONOPT,CONOPT       ALWAYS CLEAR OPTIONS                         
         OI    CONOPTH+6,X'80'                                                  
*                                                                               
         CLI   ACTEQU,ACTADD                                                    
         BE    XIT                                                              
         CLI   ACTEQU,ACTLIST      IS IT AN ACTION OF LIST                      
         BE    *+12                                                             
         LA    R2,DLYPCTH          MOVE CURSOR TO TARGET PCT FIELD              
         ST    R2,AFRSTREC                                                      
         XC    BIGKEY,BIGKEY       AVOID INVALID WRITE SEQUENCE                 
         MVC   BIGKEY(L'ACTKEY),SAVEKEY                                         
         MVI   RDUPDATE,C'Y'                                                    
         MVC   AIO,AIO2                                                         
         GOTO1 HIGH                                                             
         CLC   KEYSAVE(L'ACTKEY),BIGKEY                                         
         BE    *+6                 BETTER BE THERE                              
         DC    H'0'                                                             
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 GETREC                                                           
         MVC   AIO,AIO1                                                         
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*        DISPLAY HOURS                                                          
***********************************************************************         
         SPACE 1                                                                
         USING DAYLINED,R2         R2 = SCREEN LINE                             
         USING DEDELD,R6           R6 = ELEMENT                                 
DISDHRS  NTR1  ,                                                                
         CURED (P4,DEDHRS),(6,DAYHRS),2                                         
*                                                                               
         MVC   DAYEXC(L'AC@NO),AC@NO                                            
         TM    DEDSTAT,DEDSEXCP                                                 
         BZ    DISD02                                                           
         MVC   DAYEXC(L'AC@YES),AC@YES                                          
DISD02   OI    DAYEXCH+FLDOIND-FLDHDRD,FOUTTRN                                  
*                                                                               
DDX      XIT1  REGS=(R6)           PASS BACK R6                                 
         DROP  R2,R6                                                            
         EJECT                                                                  
***********************************************************************         
*        DISPLAY TARGET UTILISATION PERCENTAGE                                  
***********************************************************************         
         USING FFRELD,R6                                                        
DISPCT   NTR1  ,                                                                
DP01     CLI   FFRTYPE,FFRTPCT     CHECK TYPE                                   
         BE    DP02                                                             
         BAS   RE,NEXTEL                                                        
         BE    DP01                                                             
         B     DPX                                                              
DP02     CURED (P2,FFRPCT),(6,DLYPCT),0,ALIGN=LEFT                              
         OI    DLYPCTH+FLDOIND-FLDHDRD,FOUTTRN                                  
DPX      XIT1  REGS=(R6)                                                        
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*        DISPLAY HOURS                                                          
***********************************************************************         
*                                                                               
         USING CALLINED,R2         R2 = SCREEN LINE                             
         USING SHRELD,R6           R6 = ELEMENT                                 
DISHOURS NTR1                                                                   
         CLC   AC@TOTU,CALMON      TOTAL LINE = DONE                            
         BNE   *+12                                                             
         OI    BITS,LAST                                                        
         B     DHX                                                              
         CLC   CALSTDT,SPACES      ANY CALENDAR DATE ON LINE?                   
         BH    *+12                                                             
         OI    BITS,LAST                                                        
         B     DHX                                                              
*                                                                               
DH01     TM    BITS,NOTFIRST       FIRST PERIOD?                                
         BO    DH02                NO                                           
*                                                                               
         MVC   WORK(5),CALSTDT    FIRST PERIOD START DATE HAS YEAR              
         MVI   WORK+5,C'/'                                                      
         MVC   WORK+6(2),STYEAR   CHARACTER YEAR                                
         MVC   WORK+8(6),CALSTDT+5                                              
         B     DH03                                                             
*                                                                               
DH02     DS    0H                  MAKE SURE START DATE HAS YEAR                
         MVC   WORK(5),CALSTDT                                                  
         MVI   WORK+5,C'/'                                                      
         MVC   WORK+6(2),NXTYEAR   CHARACTER YEAR                               
         MVC   WORK+8(6),CALHYPH   END DATE                                     
*                                                                               
DH03     DS    0H                  VALIDATE                                     
         MVC   BYTE,LANGCODE                                                    
         LA    R1,14                                                            
         STC   R1,BYTE2                                                         
         OI    BYTE2,X'40'          ASSUME DDMM NOT MMYY                        
         GOTO1 PERVAL,DMCB,(BYTE2,WORK),(BYTE,BLOCK)                            
         LA    R3,BLOCK                                                         
         USING PERVALD,R3                                                       
         OI    BITS,NOTFIRST                                                    
*                                  DATES FROM CALENDAR                          
         MVC   PERSTART,PVALPSTA                                                
         MVC   PEREND,PVALPEND                                                  
*                                                                               
         XC    WORK,WORK                                                        
         GOTO1 DATCON,DMCB,(1,PVALPEND),(11,WORK)                               
         MVC   WORK+8(5),=C'-(1D)' ADD ONE DAY                                  
         CLI   LANGCODE,3                                                       
         BNE   *+10                                                             
         MVC   WORK+8(5),=C'-(1T)' ADD ONE DAY                                  
         XC    BLOCK(L'PVALOUTB),BLOCK                                          
         MVC   BYTE,LANGCODE                                                    
         LA    R1,14                                                            
         STC   R1,BYTE2                                                         
         OI    BYTE2,X'40'          ASSUME DDMM NOT MMYY                        
         GOTO1 PERVAL,DMCB,(BYTE2,WORK),(BYTE,BLOCK)                            
         LA    R3,BLOCK                                                         
*                                    CONVERT FA TO F0, FB TO F1, ETC.           
         GOTO1 DATCON,DMCB,(0,PVALEEND),(X'20',PVALEEND)                        
*        CLI   PVALEEND,C'9'                                                    
*        BNH   DH08                                                             
*        SR    R1,R1                                                            
*        IC    R1,PVALEEND                                                      
*        SH    R1,=H'10'                                                        
*        STC   R1,PVALEEND                                                      
DH08     MVC   NXTYEAR,PVALEEND    CHAR YEAR FOR NEXT PERIOD START DATE         
         DROP  R3                                                               
*                                                                               
DH10     DS    0H                                                               
         CLC   PEREND,SHREND       IS CALENDAR PERIOD WITHIN ELEM PER           
         BH    DH20                NO, THEN GET NEXT ELEM                       
         CLC   PERSTART,SHRSTART                                                
         BL    DH20                                                             
*                                                                               
DH15     DS    0H                  DISPLAY HOURS                                
         CURED (P4,SHRHOURS),(6,CALHOUR),2                                      
         AP    YRTOTAL,SHRHOURS                                                 
         B     DHX                                                              
*                                                                               
DH20     DS    0H                                                               
         BAS   RE,NEXTEL                                                        
         BE    DH10                                                             
         OI    BITS,LAST                                                        
*                                                                               
DHX      XIT1  REGS=(R6)           PASS BACK R6                                 
         DROP  R2,R6                                                            
         EJECT                                                                  
***********************************************************************         
*        BUMP6  -  BUMPS 6 FIELDS, TO GET TO NEXT IN COLUMN                     
*        R2 SHOULD BE POINTING TO FIRST HEADER IN COLUMN                        
***********************************************************************         
BUMP6    NTR1                                                                   
         LA    R4,6                                                             
BUMPLP   ZIC   R1,0(R2)                                                         
         AR    R2,R1                                                            
         BCT   R4,BUMPLP                                                        
         XIT1  REGS=(R2)           RETURN R2                                    
         EJECT                                                                  
***********************************************************************         
*        UPDATE STATUS IN DIR WITH FIRST AND LAST DATES ON REC                  
***********************************************************************         
*                                                                               
XP       XC    STDATE,STDATE                                                    
         XC    ENDATE,ENDATE                                                    
         TM    RUNINDS,RUNIDALY                                                 
         BZ    XP05                                                             
         BAS   RE,GETCAL                                                        
         MVC   STDATE,CALSTART                                                  
         MVC   ENDATE,CALEND                                                    
         B     XP20                                                             
*                                                                               
         USING SHRELD,R6                                                        
XP05     L     R6,AIO              RECORD JUST CHANGED/ADDED IS HERE            
         MVI   ELCODE,SHRELQ       GET FIRST AND LAST DATES                     
         BAS   RE,GETEL                                                         
         BNE   XP20                                                             
         MVC   STDATE,SHRSTART                                                  
         B     XP10                                                             
XP10NX   BAS   RE,NEXTEL                                                        
         BNE   XP20                                                             
XP10     MVC   ENDATE,SHREND                                                    
         B     XP10NX                                                           
*                                                                               
         USING STDRECD,R6                                                       
XP20     L     R6,AIO                                                           
         XC    BIGKEY,BIGKEY                                                    
         MVC   BIGKEY(L'ACTKEY),0(R6)                                           
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 READ                                                             
         LA    R6,BIGKEY                                                        
         MVC   STDKSTDT,STDATE                                                  
         MVC   STDKENDT,ENDATE                                                  
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         MVC   STDRSTDT,STDATE                                                  
         MVC   STDRENDT,ENDATE                                                  
         TM    OPTSTAT,OPTSREM     REMOVING HOURS?                              
         BZ    *+8                                                              
         BAS   RE,ANYELEMS         CHECK FOR ELEMENTS ON REC                    
         GOTO1 PUTREC                                                           
         GOTO1 WRITE                                                            
XPX      B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*        ON-SCREEN LIST                                                         
***********************************************************************         
*                                                                               
LR       TM    TRANSTAT,RACHANG               RECORD/ACTION CHANGE?             
         BZ    LRSTART                        NO - DON'T RESET                  
         XC    STLYEAR,STLYEAR                SWITCHING BETWEEN STD/EDT         
         XC    STLOFF,STLOFF                  SCREENS, WANT TO CLEAR            
         XC    STLDEPT,STLDEPT                FILTERS                           
         XC    STLSDPT,STLSDPT                                                  
         XC    STLPER,STLPER                                                    
         XC    STLDAY,STLDAY                                                    
         OI    STLYEARH+FLDOIND-FLDHDRD,FOUTTRN        TRANSMIT CHANGES         
         OI    STLOFFH+FLDOIND-FLDHDRD,FOUTTRN                                  
         OI    STLDEPTH+FLDOIND-FLDHDRD,FOUTTRN                                 
         OI    STLSDPTH+FLDOIND-FLDHDRD,FOUTTRN                                 
         OI    STLPERH+FLDOIND-FLDHDRD,FOUTTRN                                  
         MVI   FILTYEAR,X'FF'                          SET NO INPUT             
         MVI   BITS,0                                  INDICATORS               
         B     LR6                                     BUILD LIST               
*                                                                               
* NOT RESETING - VALIDATE FILTERS                                               
LRSTART  TM    STLYEARH+(FLDIIND-FLDHDRD),FINPTHIS   HAS FILTER CHANGED         
         BNO   *+10                                  NO - USE OLD KEY           
         XC    BIGKEY,BIGKEY                         BUILD NEW LIST             
         MVI   FILTYEAR,X'FF'                        SET NO INPUT               
         LA    R2,STLYEARH                                                      
         CLI   5(R2),0                               INPUT LENGTH = 0           
         BE    LR1                                   NO INPUT                   
         TM    STLYEARH+(FLDIIND-FLDHDRD),FINPNUM    IS VALID NUMERIC?          
         BZ    EINVYR                                INVALID YEAR               
         BAS   RE,VALYR                   CALDATE HOLDS CONVERTED DATE          
         MVC   FILTYEAR(1),CALDATE        MOVE YEAR INTO FILTYEAR               
*                                                                               
LR1      MVI   BITS,0                     INDICATES WHICH FILTERS USED          
         TM    STLOFFH+(FLDIIND-FLDHDRD),FINPTHIS   OFFICE FILTER CHG?          
         BNO   *+10                                 NO - NEXT FILTER            
         XC    BIGKEY,BIGKEY                        CLEAR KEY-BUILD NEW         
         LA    R2,STLOFFH          VALIDATE OFFICE FILTER                       
         CLI   5(R2),0             ANY DATA?                                    
         BE    LR2                 INPUT LENGTH > 0                             
         OI    BITS,YESOFF         SWITCH ON OFFICE USED BIT                    
*                                                                               
LR2      TM    STLDEPTH+(FLDIIND-FLDHDRD),FINPTHIS DEPT FILTER CHANGED?         
         BNO   *+10                                NO - NEXT FILTER             
         XC    BIGKEY,BIGKEY                       CLEAR KEY-BUILD NEW          
         LA    R2,STLDEPTH                                                      
         CLI   5(R2),0                             ANY DATA?                    
         BE    LR3                                 INPUT LENGTH > 0             
         TM    BITS,YESOFF         MUST HAVE ENTERED AN OFFICE                  
         BNO   EMISHIGH            MISSING HIGHER LEVELS                        
         OI    BITS,YESDPT         SWITCH ON DEPT USED BIT                      
*                                                                               
LR3      TM    STLSDPTH+(FLDIIND-FLDHDRD),FINPTHIS    SUB-DEPT                  
         BNO   *+10                                                             
         XC    BIGKEY,BIGKEY                                                    
         LA    R2,STLSDPTH         ANY SUB DEPT FILTER                          
         CLI   5(R2),0             CHECK INPUT LENGTH > 0                       
         BE    LR4                                                              
         TM    BITS,YESOFF+YESDPT  MUST HAVE HIGHER LEVELS                      
         BNO   EMISHIGH                                                         
         OI    BITS,YESSBDPT                                                    
*                                                                               
LR4      TM    STLPERH+(FLDIIND-FLDHDRD),FINPTHIS     PERSON FILTER             
         BNO   *+10                                                             
         XC    BIGKEY,BIGKEY                                                    
         LA    R2,STLPERH          ANY PERSON                                   
         CLI   5(R2),0             CHECK INPUT LENGTH > 0                       
         BE    LR5                                                              
         OI    BITS,YESPERSN                                                    
*                                                                               
LR5      DS    0H                                                               
         CLI   RECNUM,RTEDT                      EDIT HOURS SCREEN?             
         BNE   LR6                   STD HOURS - DAILY DOESN'T APPLY            
         TM    STLDAYH+(FLDIIND-FLDHDRD),FINPTHIS            CHANGE?            
         BNO   *+10                                                             
         XC    BIGKEY,BIGKEY                     REBUILD LIST                   
*                                                                               
         LA    R2,STLDAYH                                                       
         CLI   5(R2),0                           CHECK INPUT > 0                
         BE    LR6                               NO INPUT - BUILD LIST          
         ZIC   R1,5(R2)                          GET INPUT LENGTH               
         AR    R1,R2                             R1 = A(LAST CHAR)              
         BCTR  R1,0                                                             
LR5A     CLI   8(R1),170                         IS CHAR LOWER CASE             
         BNL   LR5B                              HIGH - UPPER CASE              
         CLI   8(R1),128                         IS SPECIAL CHAR                
         BNH   LR5B                              NOT LOWER CASE - NEXT          
         ZIC   R0,8(R1)                          GET CHAR                       
         A     R0,=XL4'40'                       CONVERT TO UPPER               
         STC   R0,8(R1)                          STORE CHAR                     
LR5B     BCTR  R1,R0                             PREVIOUS INPUT CHAR            
         CR    R1,R2                             REACHED START?                 
         BNL   LR5A                              CONVERT PREV CHAR              
         IC    R1,5(R2)                          GET LENGTH AGAIN               
LR5C     BCTR  R1,0                              DEC FOR EXECUTE                
         EX    R1,*+8                            CLC - YES                      
         BNE   *+20                              NOT YES                        
         CLC   8(0,R2),AC@YESU                   COMPARE TO "YES"               
         MVC   STLDAY(3),AC@YES                  MOVE YES TO SCREEN             
         B     LR6                               BUILD LIST                     
         OC    STLDAY,SPACES                                                    
         EX    R1,*+8                                                           
         BNE   EINVOPT                           NOT YES/NO - INVALID           
         CLC   8(0,R2),AC@NOU                    COMPARE TO "NO"                
         MVC   STLDAY(3),AC@NO                   MOVE NO TO SCREEN              
*                                                                               
         USING EDTRECD,R6                                                       
LR6      MVI   SEQNUM,X'00'                                                     
         LA    R6,BIGKEY                                                        
         XC    LISTTAB,LISTTAB                                                  
         LA    R3,LISTTAB          LIST TABLE                                   
         MVI   LSTCOUNT,0          START LIST TABLE AT ZERO                     
*                                                                               
         OC    BIGKEY,BIGKEY       FIRST TIME THROUGH?                          
         BNZ   LRHI                                                             
         MVC   EDTKEY,SPACES                                                    
         MVI   EDTKTYP,STDKTYPQ    STANDARD HOURS RECORD                        
         MVI   EDTKSUB,STDKSUBQ                                                 
*                                                                               
         CLI   RECNUM,RTEDT        EDIT HOURS                                   
         BNE   *+12                                                             
         MVI   EDTKTYP,EDTKTYPQ    EDIT HOURS RECORD                            
         MVI   EDTKSUB,EDTKSUBQ                                                 
*                                                                               
         MVC   EDTKCPY,CMPY        COMPANY                                      
         MVC   EDTKSEQ,SEQNUM                                                   
         MVC   KEY2,BIGKEY                                                      
*                                                                               
LRHI     GOTO1 HIGH                                                             
         CLI   DMCB+8,0                                                         
         BE    LR20                                                             
         DC    H'0'                                                             
LRSEQ    GOTO1 READ                                                             
         GOTO1 SEQ                                                              
         CLI   DMCB+8,0                                                         
         BE    LR20                                                             
         DC    H'0'                                                             
*                                                                               
         USING EDTRECD,R6                                                       
LR20     LA    R6,BIGKEY                                                        
         XC    STDENDYR,STDENDYR                                                
         MVC   OFFICE,EDTKOFC                                                   
         OC    OFFICE,SPACES                                                    
*                                                                               
         MVC   DEPT,EDTKDPT                                                     
         OC    DEPT,SPACES                                                      
*                                                                               
         MVC   SUBDPT,EDTKSBD                                                   
         OC    SUBDPT,SPACES                                                    
*                                                                               
         MVC   PERSON,EDTKPER                                                   
         OC    PERSON,SPACES                                                    
*                                                                               
         MVC   DAILYFLG(L'AC@NO),AC@NO                                          
         TM    EDTKKSTA,EDTKSDAY                                                
         JZ    *+10                                                             
         MVC   DAILYFLG(L'AC@YES),AC@YES                                        
         MVC   DATECAL(L'EDTKYR),EDTKYR                                         
         MVC   DATECAL+L'EDTKYR(2),=X'0101'                                     
*                                                                               
LR22     GOTO1 GETREC                                                           
         CLC   BIGKEY(3),SAVEKEY   SAME RECORD TYPE AND COMPANY?                
         BNE   LRX                 NO MORE TO LIST                              
*                                                                               
         CLC   OFFICE,SPACES                                                    
         BNH   LR24                                                             
         LA    R1,OFFICE                                                        
         BAS   RE,OFFACC                                                        
         BNE   LRSEQ                                                            
*                                                                               
LR24     MVC   LISTAR,SPACES       CLEAR LIST LINE                              
         TM    EDTKKSTA,EDTKSDAY   IS IT DAILY TIME                             
         BNZ   LR29                                                             
*                                                                               
         USING SHRELD,R6                                                        
         L     R6,AIO                                                           
         MVI   ELCODE,SHRELQ                                                    
         BAS   RE,GETEL                                                         
         B     LR25                                                             
*                                                                               
LRNX     MVI   ELCODE,SHRELQ                                                    
         BAS   RE,NEXTEL                                                        
LR25     BNE   LRSEQ                                                            
         CLC   SHREND(1),STDENDYR   SAME YEAR AS BEFORE?                        
         BNE   *+16                                                             
         TM    CALFLAG,NOCAL       NO CAL REC FOR THIS YEAR?                    
         BO    LRNX                KEEP ON LOOKING                              
         B     LR27                                                             
         MVC   STDENDYR,SHREND       SAVE ENDING YEAR                           
*                                                                               
         USING CASRECD,R4                                                       
         LA    R4,KEY2                                                          
         XC    KEY2,KEY2                                                        
         MVI   CASKTYP,CASKTYPQ                                                 
         MVI   CASKSUB,CASKSUBQ                                                 
         MVC   CASKCPY,CMPY                                                     
         MVC   CASKEMOA(1),STDENDYR    YEAR TO READ FOR                         
         MVC   KEYSAVE,KEY2                                                     
         GOTO1 DATAMGR,DMCB,DMRDHI,ACCDIR,KEY2,KEY2,0                           
         CLC   KEY2(CASKSMOA-CASRECD-1),KEYSAVE                                 
         BE    *+12                                                             
         OI    CALFLAG,NOCAL       CALENDAR DOES NOT EXIST                      
         B     LRNX                                                             
*                                                                               
         NI    CALFLAG,X'FF'-NOCAL                                              
         LA    R2,KEY2                                                          
         AH    R2,LKEY                                                          
         AH    R2,LSTATUS                                                       
         GOTO1 DATAMGR,DMCB,GETRECD,ACCMST,(R2),AIO2,WORK                       
         USING TMRELD,R4                                                        
         L     R4,AIO2                                                          
         AH    R4,DATADISP                                                      
LR26     CLI   0(R4),0                                                          
         BE    LR29                                                             
         CLI   0(R4),TMRELQ                                                     
         BE    *+16                                                             
         SR    R1,R1                                                            
         IC    R1,1(R4)                                                         
         AR    R4,R1                                                            
         B     LR26                                                             
*                                                                               
         MVC   DATECAL,TMREND      SAVE YEAREND DATE FROM CAL REC               
         DROP  R4                                                               
*                                                                               
LR27     CLC   SHREND,DATECAL      SAME DATE?                                   
         BNE   LRNX                MUST BE THE NEXT ELEM                        
*=*=*=*=*=*=*=*=* FILTER RECS                                                   
LR29     CLI   FILTYEAR,X'FF'      TEST YEAR FILTER USED                        
         BE    *+14                NO - DO OFFICE                               
         CLC   FILTYEAR(1),DATECAL COMPARE WITH FISCAL YEAR                     
         BNE   LRSEQ               NOT EQUAL - DO NEXT ELEMENT                  
*                                                                               
         TM    BITS,YESOFF         OFFICE CODE IN FILTER                        
         BNO   *+20                NO - CONTINUE                                
         OC    STLOFF(2),SPACES    GET RID OF NULLS                             
         CLC   STLOFF(2),OFFICE    COMPARE FILTER WITH FILE VALUE               
         BNE   LRSEQ               NOT EQUAL - DO NEXT ELEMENT                  
*                                                                               
         TM    BITS,YESDPT         DEPARTMENT CODE IN FILTER                    
         BNO   *+20                NO                                           
         OC    STLDEPT(2),SPACES                                                
         CLC   STLDEPT(2),DEPT                                                  
         BNE   LRSEQ               NEXT ELEMENT                                 
*                                                                               
         TM    BITS,YESSBDPT       SUB-DEPARTMENT FILTER                        
         BNO   *+20                                                             
         OC    STLSDPT(2),SPACES                                                
         CLC   STLSDPT(2),SUBDPT                                                
         BNE   LRSEQ                                                            
*                                                                               
         TM    BITS,YESPERSN       PERSON FILTER                                
         BNO   *+20                                                             
         OC    STLPER(7),SPACES                                                 
         CLC   STLPER(7),PERSON                                                 
         BNE   LRSEQ                                                            
*                                                                               
         CLI   RECNUM,RTEDT        EDIT HOURS RECORD?                           
         BNE   LR30                NO - DRAW SCREEN                             
         OC    STLDAY,STLDAY       FILTER VALUE ENTERED                         
         BZ    LR30                NO - DRAW SCREEN                             
         CLC   STLDAY(3),DAILYFLG  COMPARE FILTER WITH VALUE IN RECORD          
         BNE   LRSEQ               NOT EQUAL - NEXT ELEMENT                     
*=*=*=*=*=*=*=*=*                                                               
         USING LSTLINED,R4                                                      
LR30     LA    R4,LISTAR           LINE DSECT                                   
         CLC   OFFICE,SPACES                                                    
         BNE   LR40                                                             
         MVC   LSTOFF(3),AC@ALLU                                                
         B     LR45                                                             
LR40     MVC   LSTOFF,OFFICE                                                    
         MVC   LSTDEPT,DEPT                                                     
         MVC   LSTSDPT,SUBDPT                                                   
         MVC   LSTPER,PERSON                                                    
*                                                                               
LR45     CLI   RECNUM,RTEDT        EDIT HOURS                                   
         BNE   LR50                NO - STD HOURS                               
         MVC   LSTDLY,DAILYFLG                                                  
*                                                                               
LR50     GOTO1 DATCON,DMCB,(1,DATECAL),(11,WORK)                                
         CLC   WORK+6(2),=C'80'     MOST RECENT FISCAL YEAR ON REC              
         BL    *+14                                                             
         MVC   LSTFYR(2),=C'19'                                                 
         B     *+10                                                             
         MVC   LSTFYR(2),=C'20'                                                 
         MVC   LSTFYR+2(2),WORK+6   CHARACTER YEAR                              
*                                                                               
LR60     MVC   0(1,R3),LSTCOUNT    LIST # (0-15)                                
         MVC   1(4,R3),DATECAL     YEAR IN LIST                                 
         ZIC   R1,LSTCOUNT                                                      
         LA    R1,1(R1)                                                         
         STC   R1,LSTCOUNT         INCREMENT LIST # BY 1                        
         LA    R3,2(R3)            BUMP TABLE                                   
         ST    R3,LISTADDR         SAVE ADDR OF NEXT POSSIBLE ENTRY             
         GOTO1 LISTMON             SEND RECORD TO SCREEN                        
         B     LRSEQ               GET NEXT ELEM ON STD HRS REC                 
LRX      MVI   0(R3),X'FF'         MARK END OF TABLE                            
         B     XIT                                                              
         DROP  R6,R4                                                            
         EJECT                                                                  
***********************************************************************         
*        CLEAR SOME FIELDS                                                      
***********************************************************************         
*                                                                               
CLRSCR   NTR1                                                                   
         LA    R2,STDCOL1H         CLEAR ALL UNPROTECTED FIELDS                 
         LA    R3,STDTOTH                                                       
         TM    RUNINDS,RUNIDALY                                                 
         BZ    DRCLR                                                            
         LA    R2,DLYSELH          CLEAR ALL UNPROTECTED FIELDS                 
         LA    R3,DLYPFKYH                                                      
                                                                                
DRCLR    TM    1(R2),X'20'         PROTECTED?                                   
         BO    DRCLR5              THEN SKIP TO NEXT FIELD                      
         TM    6(R2),X'20'         PROTECTED?                                   
         BO    DRCLR5              THEN SKIP TO NEXT FIELD                      
         ZIC   R1,0(R2)            FIELD LENGTH                                 
         SH    R1,=H'9'            8 FOR HEADER, 1 FOR EX                       
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),SPACES                                                   
         OI    6(R2),X'80'         TRANSMIT                                     
DRCLR5   ZIC   R1,0(R2)                                                         
         AR    R2,R1                                                            
         CR    R2,R3               ARE WE DONE                                  
         BL    DRCLR               NO                                           
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*        RANDOM STUFF                                                           
***********************************************************************         
*                                  GENERAL MESSAGES                             
ERRMISS  MVI   GERROR1,MISSING                                                  
         B     ERRX                                                             
ERRINV   MVI   GERROR1,INVALID                                                  
         B     ERRX                                                             
ERRX     DS    0H                                                               
         MVI   GMSYS,X'FF'         GENERAL MESSAGE SYSTEM                       
         GOTO1 MYERR                                                            
*                                                                               
EINVACC  MVC   GERROR,=AL2(ACEACCT)                                             
         B     ACCERRX                                                          
EINVSECL MVC   GERROR,=AL2(ACSELOCK)                                            
         B     ACCERRX                                                          
EREMCH   MVC   GERROR,=AL2(ACEREMCH)                                            
         B     ACCERRX                                                          
ENOREM   MVC   GERROR,=AL2(ACEREMLW)                                            
         B     ACCERRX                                                          
EMISSDF  MVC   GERROR,=AL2(ACESTDEF)                                            
         B     ACCERRX                                                          
ETOOSHRT MVC   GERROR,=AL2(ACELSHO)                                             
         B     ACCERRX                                                          
ETOOLONG MVC   GERROR,=AL2(ACELLONG)                                            
         B     ACCERRX                                                          
EMISHIGH MVC   GERROR,=AL2(ACEHIGH)    MISSING HIGHER LEVELS                    
         B     ACCERRX                                                          
EINVYR   MVC   GERROR,=AL2(ACEINVYR)                                            
         B     ACCERRX                                                          
EINVDATE MVC   GERROR,=AL2(ACEIVDTE)                                            
         B     ACCERRX                                                          
EINVHRS  MVC   GERROR,=AL2(ACEIVHRS)                                            
         B     ACCERRX                                                          
EACTHRS  MVC   GERROR,=AL2(ACEACTHR)                                            
         B     ACCERRX                                                          
EQRTHRS  MVC   GERROR,=AL2(ACEQTRHR)                                            
         B     ACCERRX                                                          
EINVOPT  MVC   GERROR,=AL2(ACEINVOP)                                            
         B     ACCERRX                                                          
ENOCAL   MVC   GERROR,=AL2(ACENOCAL)                                            
         B     ACCERRX                                                          
EINVPCT  MVC   GERROR,=AL2(AE$INVPC)                                            
         B     ACCERRX                                                          
*                                                                               
ACCERRX  MVI   GMSYS,6             ACC MESSAGE SYSTEM                           
         GOTO1 MYERR                                                            
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
         EJECT                                                                  
***********************************************************************         
*        LTORG                                                                  
***********************************************************************         
*                                                                               
         LTORG                                                                  
DCLISTL  DS    0X                                                               
         DCDDL AC#PFKE9,L'AC@PFKE9                                              
DCLISTLX DC    X'00'                                                            
         EJECT                                                                  
***********************************************************************         
*        LITERALS                                                               
***********************************************************************         
         SPACE 1                                                                
LITVALS  DS    0X                                                               
         DC    C'ACCDIR'                                                        
         DC    C'ACCMST'                                                        
         DC    C'GETREC'                                                        
         DC    C'DMRSEQ'                                                        
         DC    C'DMREAD'                                                        
         DC    C'DMRDHI'                                                        
***********************************************************************         
* DAILY EDIT HOUR INDICATOR VALUES                                    *         
***********************************************************************         
DAYTAB   DS    0X                                                               
         DC    AL1(DEDIMON)                                                     
         DC    AL1(DEDITUE)                                                     
         DC    AL1(DEDIWED)                                                     
         DC    AL1(DEDITHU)                                                     
         DC    AL1(DEDIFRI)                                                     
         DC    AL1(DEDISAT)                                                     
         DC    AL1(DEDISUN)                                                     
DAYTABX  DC    X'FF'                                                            
*                                                                               
***********************************************************************         
*        LIST SCREEN PFKEY TABLE DEFINITIONS                                    
***********************************************************************         
*                                                                               
LPFTABLE DS    0C                                                               
*                                                                               
*        PERSON LIST                                                            
*                                                                               
         DC    AL1(LPF01X-*,01,PFTCPROG,0,0)                                    
         DCDD  AC#CPRSN,3                                                       
         DCDD  AC#CPRSN,8                                                       
         DCDD  AC#LIST,8                                                        
LPF01X   EQU   *                                                                
*                                                                               
*        STD CHANGE                                                             
*                                                                               
         DC    AL1(LPF02X-*,02,PFTCPROG,(LPF02X-LPF02)/KEYLNQ,0)                
         DCDD  AC#CHG,3                                                         
         DC    CL8' '                                                           
*        DCDD  AC#STDHR,8                                                       
         DCDD  AC#CHG,8                                                         
LPF02    DC    AL1(KEYTYCUR,L'LSTFYR-1),AL2(LSTFYR-LSTLINED)                    
         DC    AL1(KEYTYCUR,L'LSTOFF-1),AL2(LSTOFF-LSTLINED)                    
         DC    AL1(KEYTYCUR,L'LSTDEPT-1),AL2(LSTDEPT-LSTLINED)                  
         DC    AL1(KEYTYCUR,L'LSTSDPT-1),AL2(LSTSDPT-LSTLINED)                  
         DC    AL1(KEYTYCUR,L'LSTPER-1),AL2(LSTPER-LSTLINED)                    
         DC    AL1(KEYTYCUR,L'LSTDLY-1),AL2(LSTDLY-LSTLINED)                    
LPF02X   EQU   *                                                                
*                                                                               
*        STD DISP                                                               
*                                                                               
         DC    AL1(LPF03X-*,03,PFTCPROG,(LPF03X-LPF03)/KEYLNQ,0)                
         DCDD  AC#DSP,3                                                         
         DC    CL8' '                                                           
*        DCDD  AC#STDHR,8                                                       
         DCDD  AC#DSP,8                                                         
LPF03    DC    AL1(KEYTYCUR,L'LSTFYR-1),AL2(LSTFYR-LSTLINED)                    
         DC    AL1(KEYTYCUR,L'LSTOFF-1),AL2(LSTOFF-LSTLINED)                    
         DC    AL1(KEYTYCUR,L'LSTDEPT-1),AL2(LSTDEPT-LSTLINED)                  
         DC    AL1(KEYTYCUR,L'LSTSDPT-1),AL2(LSTSDPT-LSTLINED)                  
         DC    AL1(KEYTYCUR,L'LSTPER-1),AL2(LSTPER-LSTLINED)                    
         DC    AL1(KEYTYCUR,L'LSTDLY-1),AL2(LSTDLY-LSTLINED)                    
LPF03X   EQU   *                                                                
*                                                                               
*        LAST (BACK PAGE)                                                       
*                                                                               
         DC    AL1(LPF07X-*,07,PFTLIST,0,0)                                     
         DC    CL3'   ',CL8'         '                                          
         DCDD  AC#LAST,8                                                        
LPF07X   EQU   *                                                                
*                                                                               
*        NEXT (NEXT PAGE)                                                       
*                                                                               
         DC    AL1(LPF08X-*,08,PFTLIST,0,0)                                     
         DC    CL3'   ',CL8'         '                                          
         DCDD  AC#NXT,8                                                         
LPF08X   EQU   *                                                                
*                                                                               
*        RETURN TO CALLER                                                       
*                                                                               
         DC    AL1(LPF12X-*,12,PFTRPROG,0,0)                                    
         DC    CL3' ',CL8' ',CL8' '                                             
LPF12X   EQU   *                                                                
         DC    X'FF'                                                            
         EJECT                                                                  
***********************************************************************         
*        DAILY EDIT HRS SCREEN PF KEY DEFINITIONS                               
***********************************************************************         
*                                                                               
DPFTABLE DS    0C                                                               
*                                                                               
*        EDIT HRS LIST                                                          
*                                                                               
         DC    AL1(DPF01X-*,01,PFTCPROG,0,0)                                    
         DC    CL3' '                                                           
         DCDD  AC#REDIT,8                                                       
         DCDD  AC#LIST,8                                                        
DPF01X   EQU   *                                                                
*                                                                               
*        DAY HRS CHANGE                                                         
*                                                                               
         DC    AL1(DPF02X-*,02,PFTCPROG,(DPF02X-DPF02)/KEYLNQ,0)                
         DCDD  AC#CHG,3                                                         
         DCDD  AC#RDAY,8                                                        
         DCDD  AC#CHG,8                                                         
DPF02    DC    AL1(KEYTYTWA,L'STDYEAR-1),AL2(STDYEAR-T61DFFD)                   
         DC    AL1(KEYTYTWA,L'STDOFF-1),AL2(STDOFF-T61DFFD)                     
         DC    AL1(KEYTYTWA,L'STDDEPT-1),AL2(STDDEPT-T61DFFD)                   
         DC    AL1(KEYTYTWA,L'STDSDPT-1),AL2(STDSDPT-T61DFFD)                   
         DC    AL1(KEYTYTWA,L'STDPER-1),AL2(STDPER-T61DFFD)                     
*        DC    AL1(KEYTYTWA,L'STDDLY-1),AL2(STDDLY-T61DFFD)                     
         DC    AL1(KEYTYCUR,L'DAYDAY-1),AL2(0)                                  
DPF02X   EQU   *                                                                
*                                                                               
*        DAY HRS DISPLAY                                                        
*                                                                               
         DC    AL1(DPF03X-*,03,PFTCPROG,(DPF03X-DPF03)/KEYLNQ,0)                
         DCDD  AC#SEL,3                                                         
         DCDD  AC#RDAY,8                                                        
         DCDD  AC#DSP,8                                                         
DPF03    DC    AL1(KEYTYTWA,L'STDYEAR-1),AL2(STDYEAR-T61DFFD)                   
         DC    AL1(KEYTYTWA,L'STDOFF-1),AL2(STDOFF-T61DFFD)                     
         DC    AL1(KEYTYTWA,L'STDDEPT-1),AL2(STDDEPT-T61DFFD)                   
         DC    AL1(KEYTYTWA,L'STDSDPT-1),AL2(STDSDPT-T61DFFD)                   
         DC    AL1(KEYTYTWA,L'STDPER-1),AL2(STDPER-T61DFFD)                     
*        DC    AL1(KEYTYTWA,L'STDDLY-1),AL2(STDDLY-T61DFFD)                     
         DC    AL1(KEYTYCUR,L'DAYDAY-1),AL2(0)                                  
DPF03X   EQU   *                                                                
*                                                                               
*        RETURN TO CALLER                                                       
*                                                                               
         DC    AL1(DPF12X-*,12,PFTRPROG,0,0)                                    
         DC    CL3' ',CL8' ',CL8' '                                             
DPF12X   EQU   *                                                                
         DC    X'FF'                                                            
         EJECT                                                                  
***********************************************************************         
*        MAINT SCREEN PFKEY TABLE DEFINITIONS                                   
***********************************************************************         
*                                                                               
PPFTABLE DS    0C                                                               
*                                                                               
*        RETURN TO CALLER                                                       
*                                                                               
         DC    AL1(PPF12X-*,12,PFTRPROG,0,0)                                    
         DC    CL3' ',CL8' ',CL8' '                                             
PPF12X   EQU   *                                                                
         DC    X'FF'                                                            
         EJECT                                                                  
***********************************************************************         
*        INCLUDES                                                               
***********************************************************************         
*                                                                               
*        DDSPOOLD                                                               
*        DDSPLWORKD                                                             
*        ACOFFALD                                                               
*        DDPERVALD                                                              
*        DDSCANBLKD                                                             
*        ACCAPWORKD                                                             
*        ACCAPDSECT                                                             
*        ACGENFILE                                                              
*        ACDDEQUS                                                               
*        ACMSGEQUS                                                              
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE ACOFFALD                                                       
       ++INCLUDE DDPERVALD                                                      
       ++INCLUDE DDSCANBLKD                                                     
       ++INCLUDE ACCAPWORKD                                                     
       ++INCLUDE ACCAPDSECT                                                     
       ++INCLUDE ACGENFILE                                                      
       ++INCLUDE ACDDEQUS                                                       
       ++INCLUDE DDFLDHDR                                                       
       ++INCLUDE ACMSGEQUS                                                      
         PRINT ON                                                               
         EJECT                                                                  
***********************************************************************         
*        SCREENS                                                                
***********************************************************************         
*                                                                               
       ++INCLUDE ACCAPFFD                                                       
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE ACCAPE1D                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE ACCAPE2D                                                       
         ORG   STDTAGH                                                          
       ++INCLUDE ACCAPEBD                                                       
         ORG   STDTAGH                                                          
       ++INCLUDE ACCAPECD                                                       
         ORG   STDTOT+L'STDTOT+40                                               
         EJECT                                                                  
***********************************************************************         
*        REMAINING WORK AREA                                                    
***********************************************************************         
*                                                                               
         DS    0D                                                               
RELO     DS    A                                                                
ADPFTAB  DS    A                                                                
APPFTAB  DS    A                                                                
ALPFTAB  DS    A                                                                
ADAYTAB  DS    A                                                                
SVADDR   DS    F                                                                
HOURS    DS    PL4                 PERIOD HOURS PWS                             
SVHOURS  DS    PL4                 PERIOD HOURS PWS                             
BYTE2    DS    XL1                                                              
OFFICE   DS    CL2                 OFFICE CODE                                  
DEPT     DS    CL3                 DEPARTMENT CODE                              
SUBDPT   DS    CL3                 SUB DEPARTMENT CODE                          
PERSON   DS    CL8                 PERSON CODE                                  
DAILYFLG DS    CL4                 YES OR NO FOR DAILY EDIT HOURS REC           
PYEAR    DS    PL1                 PWOS                                         
CALDATE  DS    PL3                 DATE TO GET CALENDAR                         
STYEAR   DS    CL2                 CHAR START YEAR                              
NXTYEAR  DS    CL2                 NEXT PERIODS CHAR START YEAR                 
SVMONTH  DS    PL1                 SAVE MONTH                                   
EBDATE   DS    CL10                DATE IN EBCIDIC                              
YRTOTAL  DS    PL5                                                              
TOTPAGE1 DS    PL5                                                              
PERSTART DS    PL3                 PERIOD START PWOS                            
PEREND   DS    PL3                 PERIOD END PWOS                              
NEWSTART DS    PL3                 PERIOD START PWOS                            
NEWEND   DS    PL3                 PERIOD END PWOS                              
STDATE   DS    PL3                 PERIOD START PWOS                            
ENDATE   DS    PL3                 PERIOD END PWOS                              
CALSTART DS    PL3                 CALENDAR START PWOS                          
CALEND   DS    PL3                 CALENDAR END PWOS                            
CALYEAR  DS    PL1                 CALENDAR YEAR                                
SCRSTART DS    PL3                 SCREEN START PWOS                            
SCREND   DS    PL3                 SCREEN END PWOS                              
NXTSTART DS    PL3                 START DATE OF NEXT SCREEN                    
LSTEND   DS    PL3                 END DATE OF PREVIOUS SCREEN                  
DATECAL  DS    PL3                 CALENDAR END PWOS                            
PERCENT  DS    XL4                 PERCENTAGE                                   
STDENDYR DS    PL1                 END YEAR OF STD HRS FOR A                    
*                                  PARTICULAR CALENDAR                          
STPER    DS    XL1                 SCREEN START PERIOD                          
TWASCR2  DS    XL1                 BOTTOM SCREEN                                
SVLSEL   DS    CL1                                                              
*                                                                               
RUNINDS  DS    XL1                 RUN INDICATOR                                
RUNIDALY EQU   X'80'               RUNING UNDER DAILY MODE                      
RUNINEXT EQU   X'20'               GET NEXT SEL FOR DAILY EDIT HOURS            
*                                                                               
CALFLAG  DS    XL1                                                              
CALSPLIT EQU   X'80'               CALENDAR HAS TWO SCREENS                     
CALPAGE2 EQU   X'40'               NOW DEALING WITH PAGE 2 OF CAL               
CALDISP  EQU   X'10'               NEW PAGE SO DISPLAY FIRST                    
NOCAL    EQU   X'08'               NO CALENDAR FOR THIS YEAR                    
*                                                                               
OPTSTAT  DS    XL1                 OPTIONS                                      
OPTSREM  EQU   X'80'               REMOVE HOURS FOR YEAR                        
OPTSHRS  EQU   X'40'               FILL ALL HOURS WITH VALUE                    
*                                                                               
FILTYEAR DS    PL1                 YEAR ENTERED IN FILTER                       
*                                                                               
BITS2    DS    XL1                                                              
BITS22CO EQU   X'80'               2 CHARACTER OFFICES                          
*                                                                               
OLDOFLMX EQU   16                  MAX # OF OLD OFFICES ON A PAGE               
*                                                                               
BITS     DS    XL1                                                              
YESOFF   EQU   X'80'               OFFICE IS SPECIFIED                          
YESDPT   EQU   X'40'               DEPARTMENT IS SPECIFIED                      
YESSBDPT EQU   X'20'               SUB DEPT IS SPECIFIED                        
YESPERSN EQU   X'10'               PERSON IS SPECIFIED                          
LAST     EQU   X'08'               NO MORE                                      
NOTFIRST EQU   X'04'               NOT FIRST TIME THROUGH                       
NOELEMS  EQU   X'02'               THERE ARE NO ELEMS FOR YEAR                  
CALDISPD EQU   X'01'               CALENDAR WAS DISPLAYED PREVIOUSLY            
*                                                                               
SEQNUM   DS    XL1                                                              
OFFDISP  DS    XL1                                                              
LEVELLN  DS    CL4                 LENGTHS OF ALL LEVELS                        
ACCNT    DS    CL12                MY TEMP ACCOUNT FIELD                        
CALKEY   DS    XL56                KEY TO CALENDAR REC                          
SAVEKEY  DS    XL56                ACCFILE KEY                                  
KEY2     DS    XL56                ACCFILE KEY                                  
LSTCOUNT DS    XL1                 COUNT OF # OF ENTRIES ON LIST PAGE           
LISTADDR DS    F                   ADDR OF NEXT POSS NTRY IN LISTTAB            
*                                                                               
LISTTAB  DS    CL31                TABLE OF YEARS FOR LIST SELECT               
*                                  MAX 15 ENTRIES PER PAGE                      
*                                  1 BYTE FOR # IN LIST (0-14)                  
*                                  1 BYTE FOR PACKED YEAR IN LIST               
*                                  1 BYTE FOR END OF TABLE                      
DSLISTL  DS    0X                                                               
AC@PFKE9 DS    CL70                                                             
GLOBALS  DS    0X                                                               
ACCDIR   DS    CL6                                                              
ACCMST   DS    CL6                                                              
GETRECD  DS    CL6                                                              
DMRSEQ   DS    CL6                                                              
DMREAD   DS    CL6                                                              
DMRDHI   DS    CL6                                                              
GLOBALSL EQU   *-GLOBALS                                                        
TESTLEN  EQU   (L'STDTOT+40+(STDTOT-T61DFFD))-(*-T61DFFD)/2                     
                                                                                
         EJECT                                                                  
***********************************************************************         
*        DSECTS                                                                 
***********************************************************************         
*                                                                               
LSTLINED DSECT                     LIST LINE DSECT                              
         DS    CL3                                                              
LSTOFF   DS    CL2                 OFFICE                                       
         DS    CL10                                                             
LSTDEPT  DS    CL3                 DEPT                                         
         DS    CL9                                                              
LSTSDPT  DS    CL3                 SUB DEPT                                     
         DS    CL7                                                              
LSTPER   DS    CL8                 PERSON                                       
         DS    CL7                                                              
LSTFYR   DS    CL4                 FISCAL YEAR                                  
         DS    CL6                                                              
LSTDLY   DS    CL4                 DAILY                                        
*                                                                               
CALLINED DSECT                     CALENDAR LINE DSECT                          
CALHEAD  DS    CL8                                                              
CALMON   DS    CL3                 MONTH                                        
         DS    CL1                                                              
CALNUM   DS    CL2                 PERIOD NUMBER                                
         DS    CL1                                                              
CALSTDT  DS    CL5                 START DATE                                   
CALHYPH  DS    CL1                 HYPHEN                                       
CALENDT  DS    CL5                 END DATE                                     
         ORG   CALHEAD                                                          
TOTHEAD  DS    CL8                                                              
TOTWORD  DS    CL8                                                              
TOTHOURS DS    CL10                TOTAL HOURS                                  
*                                                                               
CALHOURH DS    CL8                 HOURS HEADER                                 
CALHOUR  DS    CL6                 HOURS                                        
*                                                                               
DAYLINED DSECT                                                                  
DAYSELH  DS    CL8                                                              
DAYSEL   DS    CL3                                                              
DAYSELX  DS    CL8                                                              
DAYDAYH  DS    CL8                                                              
DAYDAY   DS    CL14                                                             
DAYHRSH  DS    CL8                                                              
DAYHRS   DS    CL6                                                              
DAYHRSX  DS    CL8                                                              
DAYEXCH  DS    CL8                                                              
DAYEXC   DS    CL6                                                              
DAYLINEL EQU   *-DAYLINED                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'045ACCAP11   03/02/21'                                      
         END                                                                    
