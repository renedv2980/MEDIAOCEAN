*          DATA SET ACCAP14    AT LEVEL 109 AS OF 07/07/20                      
*PHASE T61D14B                                                                  
*INCLUDE BINSRCH2                                                               
*INCLUDE COVAIL                                                                 
*                                                                               
***********************************************************************         
*                                                                     *         
*  TITLE:        ACCAP14 -- ALLOCATION PROFILE MAINT/LIST             *         
*                                                                     *         
*  COMMENTS:     MAINTAINS ALLOCATION PROFILE RECORDS                 *         
*                                                                     *         
*  CALLED FROM:  CAP CONTROLLER (T61D00), WHICH CALLS                 *         
*                GEGENCON (T00A30), WHICH CALLS THIS.                 *         
*                                                                     *         
*  INPUTS:       SCREENS ACCAPE5 (MAINTENANCE)                        *         
*                        ACCAPE4 (LIST)                               *         
*                                                                     *         
*  OUTPUTS:      UPDATED PROFILE RECORDS, LIST                        *         
*                                                                     *         
*  REGISTERS:    R0 -- WORK                                           *         
*                R1 -- WORK                                           *         
*                R2 -- SCREEN FIELD HEADER                            *         
*                R3 -- WORK                                           *         
*                R4 -- WORK                                           *         
*                R5 -- SYSSPARE                                       *         
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
*   DCUR L075 -FIX SO REC IS DELETED AND NOT SHOWN ON LIST SCREEN     *         
*   DCUR L079 -FIX SO STDISP CLEARS WHEN REC DOES NOT EXIST           *         
*   DCUR L081 -FIX MIN COPARE IN TABLE AND COMPARE FOR PROFILE FILTER *         
*   DCUR L087 -ADDED DTM & ATM PROFILES, GROUPING OF PROFILES, CHANGED*         
*              CLIST SETTING TO COLIST, ADDED 'PAYROLL' AS A VALID    *         
*              OPTION, FIXED BUG FOR US/UK ONLY PROFILES.             *         
*   DCUR L088 -ADDED NO VALID OPTIONS FOR LIST SCREEN AND TOOK OUT    *         
*              ++INCLUDE OF GETCAP.  NOW USES CORE RESIDENT VERSION   *         
*   DCUR L089 -ALLOW FOR 2ND LINE OF PROFILE, NEW CLIST SETTING, ALLOW*         
*              FOR OFFICE LIST OR JUST AN OFFICE(FOR AGY ON 2 BYTE    *         
*              OFFICES AND ADDED PERSON SETTING FOR NB,PB AND HO PROFS*         
*   DCUR L091 -FIXED BUT WHEN TRYING TO CLEAR OUT AN OFFICE/OLIST FROM*         
*              ISL/ILI PROFILE AND RELINKED TO PICK UP NEW VERSION OF *         
*              ACCAPROTAB(OVH PROF NOW VALID DOWN TO LOWEST LEVEL)    *         
*              ALSO MOVED COPFTABD & COPDD DSECTS TO A GLOBAL BOOK    *         
*   NSHE L093  CHANGES REQUIRED FOR ONLINE UPDATE OF BUCKETS          *         
*   NSHE L094 130704 CHANGES REQUIRED FOR DAILY TIME                  *         
*   NSHE L095 261104 ENSURE START DATE OF DAILY TIME IS COMPLETED     *         
* JFOS 096 21FEB06 - MCS T/SHEET OPTIONS, SHRINK L'DISTABLE ENTRIES   *         
* JFOS 097 10MAY06 - FIX BUGS IN GDATMCST GDAEL MAINTENANCE           *         
* SMAN 098 12APR07 LO01-6081 ENSURE ALL TIMESHEET USERS HAVE PIDS     *         
* SMAN 099 04JUN07 BR12353L BUG FIX TO PROFILE/ADD SCROLLING (AND     *         
*                  FREE UP SOME SPACE)                                *         
* SMAN 100 29JUN07 BR10931D SMALL BUG FIX TO LVL 98 AND FILTER OUT    *         
*                  OVERHEAD RECORDS IN VALPID                         *         
* NSHE 101 11DEC07 LO01-6983 NEW PROFILE TO SEND REMINDER EMAIL       *         
* JFOS 102 03JAN08 BR15241L DON'T DIE IF NO GDATMCST AT LEDGER LEVEL  *         
* JFOS 103 29JAN07 BR15786L DON'T DIE IN VALPIDS IF PERRECS MISSING   *         
* TFRY 104 02JUL08 LO01-7858 ADD 1-30 TO VALIDATE NUMBER              *         
* TFRY 105 16JAN09 BR13269D  PREVENT A USER SPACING OUT BRO PROFILE   *         
*          16JAN09 UKCR20574 CLEAR BITS2 AT BEGINNING OF PROG         *         
* MPEN 106 29JUL09 LO01-8967 ADD NEW PROFILE FOR EXP MODULE IN COST   *         
* MPEN 108 23SEP09 LO01-9348 ADD PROFILE TO SET RECEIPT TICKED BY DEF *         
*          24SEP09 LO01-9351 TIME LOCK AND REMINDERS FOR GREY         *         
* MPEN 109 19APR10 UKCR00027663 FIX BUG NOT READING CALENDAR PROPERLY *         
* NSHE 110 06OCT10 UKCR00029092 ENHANCE - READ FOR TIMESHEETS         *         
* MPEN 111 05JAN11 <PR001317> PROFILE TO CHOOSE CLAIM APPROVERS       *         
* YNGX 112 26JAN11 PR001286 MAKE AGENCY LEVEL BRO PROFILE DDS ONLY    *         
* MPEN 113 23JAN12 <PR002517> THIRD PARTY USE PROFILE                 *         
* MPEN 114 14MAY12 <RMA-185> CHECK CURRENT LOCATION AT 1R LEVEL       *         
***********************************************************************         
         TITLE 'T61D14 - ALLOCATION PROFILE MAINT/LIST'                         
T61D14   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**1D14**,R7,RR=R3                                              
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING T61DFFD,RA          BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         LA    R5,SYSSPARE                                                      
         USING BLOCKSD,R5                                                       
         ST    R3,RELO                                                          
         BASR  R1,0                                                             
         AHI   R1,OPTSTAB-*                                                     
         ST    R1,AOPTSTAB                                                      
         BASR  R1,0                                                             
         AHI   R1,GRPTAB-*                                                      
         ST    R1,AGRPTAB                                                       
         BASR  R1,0                                                             
         AHI   R1,PROFTAB-*                                                     
         ST    R1,APROFTAB                                                      
         BASR  R1,0                                                             
         AHI   R1,COUPTAB-*                                                     
         ST    R1,ACOUPTAB                                                      
         BASR  R1,0                                                             
         AHI   R1,CITABLE-*                                                     
         ST    R1,ACITABLE                                                      
         BASR  R1,0                                                             
         AHI   R1,OFFLTAB-*                                                     
         ST    R1,AOFFLTAB                                                      
         BASR  R1,0                                                             
         AHI   R1,FROMTAB-*                                                     
         ST    R1,AFROMTAB                                                      
                                                                                
         ST    RC,SAVERC                                                        
*                                                                               
         GOTO1 =A(SETUP),DMCB,RR=RELO ANY SET UP, PFTABLE ETC                   
*                                                                               
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VK                                                               
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VR                                                               
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    DK                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DR                                                               
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         BE    LR                                                               
*                                                                               
XYES     SR    RC,RC                                                            
XNO      LTR   RC,RC                                                            
XIT      XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
*                           VALIDATE KEY                              *         
***********************************************************************         
*                                                                               
VK       DS    0H                                                               
         XC    SAVEKEY,SAVEKEY     INITIALIZE                                   
         XC    SVKEY2,SVKEY2                                                    
         XC    PROFILT,PROFILT                                                  
         XC    GRPFILT,GRPFILT                                                  
         MVC   METHNUM(L'METHNUM+L'METHCODE),SPACES                             
         MVC   OFFICE(L'OFFICE+L'DEPT+L'SUBDPT+L'PERSON),SPACES                 
*        MVC   METHNUM,SPACES                                                   
*        MVC   DEPT,SPACES                                                      
*        MVC   SUBDPT,SPACES                                                    
*        MVC   PERSON,SPACES                                                    
         MVC   ACCNT,SPACES                                                     
         MVI   BITS,0                                                           
         MVI   BITS2,0                                                          
         MVI   PIDV,0                                                           
         MVI   LEVBIT,LEVDFLT      DEFAULT LEVEL                                
*                                                                               
         GOTO1 GETLDG,DMCB,C'1R'         GET APPROPRIATE FIELD LENGTHS          
         MVC   LEVELLN,ABCDLEN                                                  
         TM    LDGROFP,LDGOKEY2    ON=2 CHAR OFFICE IN KEY                      
         BO    *+8                                                              
         OI    BITS,ONEBYTOF                                                    
         EJECT                                                                  
*----------------------------------------------------------------------         
*        VALIDATE OPTIONS                                                       
*----------------------------------------------------------------------         
*                                                                               
VK04     GOTO1 =A(VALOPTS),DMCB,RR=RELO                                         
*                                                                               
*----------------------------------------------------------------------         
*        VALIDATE METHOD                                                        
*----------------------------------------------------------------------         
*                                  SEE IF METHOD WAS SPECIFIED                  
VK05     LA    R2,PROMETHH                                                      
*                                                                               
         CLI   ACTEQU,ACTLIST      LIST DOESN'T NEED HIGHER LEVEL               
         BNE   VK06                USED AS FILTER FOR LIST                      
         CLI   5(R2),0             ANY DATA?                                    
         BE    VK40                IF NOT                                       
*                                                                               
VK06     CLI   5(R2),0             ANY DATA?                                    
         BNE   VK07                                                             
         OI    LEVBIT,LEVNOMTH     NO METHOD ENTERED                            
         B     VK40                REQUIRED                                     
*                                                                               
VK07     TM    4(R2),X'08'         VALID NUMERIC                                
         BNO   VK10                                                             
*                                                                               
*----------------------------------------------------------------------         
*                                  READ RECORD BY METHOD NUMBER                 
*                                                                               
         CLI   5(R2),1             ONLY 1-9                                     
         BNE   EINVMET                                                          
         MVC   METHNUM,8(R2)                                                    
*                                                                               
         XC    BIGKEY,BIGKEY                                                    
         LA    R6,BIGKEY           VALIDATE METHOD                              
         USING CAHRECD,R6                                                       
         MVC   CAHKEY,SPACES                                                    
         MVI   CAHKTYP,CAHKTYPQ    ALLOCATION METHOD RECORD TYPE BY NUM         
         MVI   CAHKSUB,CAHKSUBQ    METHOD BY CODE SUB TYPE                      
         MVC   CAHKCPY,CMPY        COMPANY                                      
         MVC   CAHKMTHD,METHNUM    METHOD NUMBER                                
         XC    CAHKOFC,CAHKOFC                                                  
         GOTO1 HIGH                                                             
         CLC   BIGKEY(L'ACTKEY),KEYSAVE                                         
         BNE   ENOMETH                                                          
         GOTO1 GETREC                                                           
         B     VK20                                                             
         DROP  R6                                                               
*                                                                               
*----------------------------------------------------------------------         
*                                  READ RECORD BY METHOD CODE                   
*                                                                               
VK10     MVC   METHCODE,PROMETH    SAVE METHOD CODE                             
         OC    METHCODE,SPACES                                                  
*                                                                               
         XC    BIGKEY,BIGKEY                                                    
         LA    R6,BIGKEY           VALIDATE METHOD                              
         USING CMTRECD,R6                                                       
         MVC   CMTKEY,SPACES                                                    
         MVI   CMTKTYP,CMTKTYPQ    ALLOCATION METHOD RECORD TYPE                
         MVI   CMTKSUB,CMTKSUBQ    METHOD BY CODE SUB TYPE                      
         MVC   CMTKCPY,CMPY        COMPANY                                      
         MVC   CMTKMTHD,METHCODE   METHOD CODE                                  
         GOTO1 HIGH                                                             
         CLC   BIGKEY(L'ACTKEY),KEYSAVE                                         
         BNE   ENOMETH                                                          
         GOTO1 GETREC                                                           
*        B     VK20                                                             
*                                                                               
*----------------------------------------------------------------------         
*                                  GET BOTH METHNUM AND METHCODE                
*                                                                               
VK20     DS    0H                                                               
         USING METELD,R6                                                        
         L     R6,AIO                                                           
         MVI   ELCODE,METELQ       METHOD OF ALLOCATION ELEMENT                 
         BAS   RE,GETEL                                                         
         BNE   ERRINV                                                           
*                                                                               
         MVC   METHNUM(L'METHNUM+L'METCODE),METNUM BOTH NUMBER AND CODE         
*        MVC   METHCODE,METCODE                                                 
         OI    LEVBIT,LEVMETH      METHOD LEVEL                                 
         MVC   PROMETH,METCODE                                                  
         OI    PROMETHH+6,X'80'    XMIT                                         
*        B     VK40                                                             
*                                                                               
         EJECT                                                                  
*----------------------------------------------------------------------         
*    VALIDATE OFFICE/DEPT/SUBDPT/PERSON - CAN ENTER UP TO ANY LEVEL             
*----------------------------------------------------------------------         
*                                                                               
VK40     GOTO1 =A(VALODSP),DMCB,RR=RELO                                         
         EJECT                                                                  
*----------------------------------------------------------------------         
*               IF LISTING, VALIDATE PROFILE FILTER                             
*----------------------------------------------------------------------         
VK60     CLI   ACTEQU,ACTLIST                                                   
         BNE   VK100                                                            
*                                                                               
         XC    CODISP1,CODISP1     DISPLACEMENT TO ENTRY IN COBLOCK             
*                                                                               
         LA    R2,PRLFLTH                                                       
         CLI   5(R2),0                                                          
         BE    VK100                                                            
*                                                                               
         L     R3,APROFTAB        CHECK FOR MATCH IN PROFILE TABLE              
         USING COPFTABD,R3                                                      
*                                                                               
VK65     L     R0,APDSLIST                                                      
         ZICM  R1,COSHORT,2        DISP TO SHORT NAME                           
         AR    R1,R0                                                            
         MVC   SHRTNAME,0(R1)                                                   
         ZICM  R1,COLONG,2         DISP TO LONG NAME                            
         AR    R1,R0                                                            
         MVC   LONGNAME,0(R1)                                                   
*                                                                               
         CLI   5(R2),2             IF NB,PB,HO                                  
         BNE   *+8                                                              
         MVI   10(R2),C' '         MUST PUT SPACE IN 3RD BYTE                   
*                                                                               
         CLI   5(R2),3             WAS LONGNAME ENTERED                         
         BH    *+14                OR THERE PREVIOUSLY?                         
*                                                                               
         CLC   SHRTNAME,8(R2)      MATCH ON SHORT NAME?                         
         BE    VK70                YEP                                          
*                                                                               
         ZIC   R1,COMIN            MIN COMPARE FOR LONG NAME                    
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   LONGNAME(0),8(R2)   MATCH ON LONG NAME                           
         BE    VK70                YEP                                          
*                                                                               
         LA    R3,COTBLEN(R3)      CHECK NEXT ENTRY                             
         CLI   0(R3),X'00'         END OF TABLE                                 
         BNE   VK65                                                             
         B     EINVPRO                                                          
*                                                                               
VK70     DS    0H                                                               
         MVC   PRLFLT,LONGNAME                                                  
         OI    6(R2),X'80'         XMIT LONG NAME                               
         MVC   PROFILT,SHRTNAME    SAVE FILTER                                  
         L     R1,APROFTAB                                                      
         SR    R3,R1               SAVE DISPLACEMENT INTO PROFILE TABLE         
         STH   R3,PROFDISP                                                      
*                                                                               
         DROP  R3                                                               
         EJECT                                                                  
*----------------------------------------------------------------------         
*                          BUILD KEY                                            
*----------------------------------------------------------------------         
*                                                                               
         USING CAPRECD,R6                                                       
VK100    LA    R6,BIGKEY                                                        
         XC    BIGKEY,BIGKEY                                                    
*                                                                               
         MVC   CAPKEY,SPACES                                                    
         MVI   CAPKTYP,CAPKTYPQ    PROFILE REC                                  
         MVI   CAPKSUB,CAPKSUBQ                                                 
         MVC   CAPKCPY,CMPY        COMPANY                                      
         MVC   CAPKMTHD,METHNUM                                                 
         MVC   CAPKOFC(L'CAPKOFC+L'CAPKDPT+L'CAPKSDT+L'CAPKPER),OFFICE          
*        MVC   CAPKDPT,DEPT                                                     
*        MVC   CAPKSDT,SUBDPT                                                   
*        MVC   CAPKPER,PERSON                                                   
         MVC   SAVEKEY,BIGKEY                                                   
*                                                                               
         CLI   ACTEQU,ACTLIST                                                   
         BE    XIT                                                              
*                                                                               
VK110    DS    0H                  I DO MY OWN IO                               
         OI    DMINBTS,X'08'       PASS BACK DELETED RECS                       
         GOTO1 HIGH                                                             
         CLC   BIGKEY(L'ACTKEY),KEYSAVE                                         
         BNE   VK112               RECORD DOESN'T EXISTS                        
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         USING CAPRECD,R6                                                       
         CLC   DSPKEY,0(R6)        DID KEY CHANGE?                              
         BNE   VK111               YES, DEFAULT TO BEGINNING                    
         CLC   SVOPTBIT,OPTBIT     DID DISPLAY OPTIONS CHANGE                   
         BE    *+16                NO                                           
*                                                                               
VK111    MVC   STDISP,=H'0'                                                     
         MVC   PRVSTDSP,=H'0'                                                   
         GOTO1 =A(DELOFF),DMCB,RR=RELO                                          
         B     XIT                 RECORD EXISTS                                
*                                                                               
*K112    TM    TRANSTAT,RACHANG    REC/ACT CHANGED?                             
*        BZ    *+16                                                             
VK112    MVC   STDISP,=H'0'                                                     
         MVC   PRVSTDSP,=H'0'                                                   
         L     RE,AIO              CLEAR IO                                     
         LA    RF,LIOS                                                          
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         L     R6,AIO                                                           
         MVC   0(42,R6),KEYSAVE    MOVE IN KEY                                  
*                                                                               
         OI    BITS,NEWREC         FLAG TO ADD REC                              
*                                                                               
         B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*                         VALIDATE RECORD                             *         
***********************************************************************         
*                                                                               
VR       DS    0H                                                               
         GOTO1 =A(VALOPTS),DMCB,RR=RELO                                         
*                                                                               
         CLI   ACTEQU,ACTDIS       BRANCH TO DISPLAY                            
         BE    DR                                                               
         OI    GENSTAT2,RETEQSEL   REDISPLAY BEFORE RETURN TO LIST              
*                                                                               
         NI    BITS,X'FF'-CHANGES  RESET                                        
*                                                                               
         XC    BIGKEY,BIGKEY                                                    
         MVC   BIGKEY(L'SAVEKEY),SAVEKEY   DOING MY OWN IO                      
         OI    DMINBTS,X'08'               PASS BACK DELETED RECS               
         GOTO1 HIGH                                                             
         CLC   BIGKEY(L'ACTKEY),KEYSAVE                                         
         BNE   VR03                        RECORD DOESN'T EXISTS                
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         USING CAPRECD,R6                                                       
         GOTO1 =A(DELOFF),DMCB,RR=RELO                                          
         NI    BITS,X'FF'-NEWREC           FLAG NOT TO ADD REC                  
         B     VR05                        RECORD EXISTS                        
*                                                                               
VR03     L     RE,AIO              CLEAR IO                                     
         LA    RF,LIOS                                                          
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         L     R6,AIO                                                           
         MVC   0(42,R6),KEYSAVE    MOVE IN KEY                                  
         OI    BITS,NEWREC         FLAG TO ADD REC                              
*                                                                               
VR05     GOTO1 =A(MKDISTAB),DMCB,RR=RELO  MAKE DISPLAY TABLE                    
         L     R6,AIO                                                           
         CLC   DSPKEY,0(R6)        DID KEY CHANGE                               
         BNE   VR06                                                             
         CLC   SVOPTBIT,OPTBIT     DID DISPLAY OPTIONS CHANGE                   
         BNE   VR06                                                             
         CLC   SVGRPOPT,GRPFILT    DID GROUP FILTER CHANGE                      
         BE    VR07                                                             
VR06     OI    BITS,KEYCHNGE       THERE WAS A KEY CHANGE                       
         B     DR                  DISPLAY FIRST                                
         EJECT                                                                  
*                                                                               
*----------------------------------------------------------------------         
*                 LOOP THROUGH SCREEN FOR CHANGES                               
*----------------------------------------------------------------------         
*                                                                               
VR07     LA    R2,PROLIN1H         FIRST DISPLAY LINE                           
         USING DSPLINED,R2                                                      
*                                                                               
         LA    R4,DISTABLE          R4 POINTER TO DISPLAY TABLE                 
         USING DISTABD,R4                                                       
*                                                                               
         AH    R4,STDISP           FIRST ENTRY DISPLAYED ON SCREEN              
         CLI   0(R4),0             ANYMORE ENTRIES                              
         BE    VR100                                                            
*                                                                               
VR10     CLC   DSPGRPNM(2),=C'**'  IS THIS LINE A PROFILE GROUP                 
         BE    VR12                TITLE LINE                                   
         CLC   DSPSHORT,SPACES     IS THIS THE 2ND LINE OF A PROFILE            
         BE    *+14                YES                                          
         OC    DSPSHORT,DSPSHORT                                                
         BNZ   VR15                                                             
VR12     LA    R2,DSPLINLN(R2)     THEN BUMP TO NEXT LINE ON SCREEN             
         LA    R1,PROPFKYH                                                      
         CR    R2,R1                                                            
         BNL   VR100               END OF PAGE                                  
         B     VR10                                                             
*                                                                               
VR15     CLC   DSPSHORT,DISSHORT                                                
         BNE   VR20NXX                                                          
         TM    DSPSETH+6,X'20'     IS FIELD PROTECTED                           
         BO    VR20NX              YES THEN SKIP                                
         OC    DSPSET,SPACES                                                    
         OC    DISSET,SPACES                                                    
         CLC   DSPSET,DISSET       SAME SETTING                                 
         BNE   VR30                NO, THEN MAKE CHANGES                        
*                                                                               
VR20NX   LA    R2,DSPLINLN(R2)     NEXT LINE                                    
         LA    R1,PROPFKYH                                                      
         CR    R2,R1                                                            
         BNL   VR100               END OF PAGE                                  
*                                                                               
VR20NXX  LA    R4,DISTABLN(R4)     NEXT TABLE ENTRY                             
         CLI   0(R4),0             ANYMORE ENTRIES                              
         BE    VR100                                                            
         B     VR10                                                             
*                                                                               
VR30     DS    0H                  VALIDATE INPUT                               
         CLI   DSPSETH+5,0         ANY INPUT                                    
         BNE   VR34                                                             
         XC    SETTING,SETTING                                                  
         B     VR55                                                             
                                                                                
VR34     SR    RF,RF               BRANCH TO VALIDATION ROUTINE                 
         IC    RF,DISVRTN                                                       
         SLL   RF,2                                                             
         B     *(RF)                                                            
                                                                                
         B     VRTNTBL                                                          
         B     VRTNNUM                                                          
         B     VRTNLED                                                          
         B     VRTNOFF                                                          
         B     VRTNCON                                                          
         B     VRTMUPL                                                          
         B     VRT1NAC                                                          
         B     VRTNUM2                                                          
         B     VRTMU2L                                                          
*                                                                               
VRTNTBL  DS    0H                  VALIDATE FROM OPTIONS IN TABLE               
***      GOTO1 =A(VALTBL),DMCB,RR=RELO                                          
         BAS   RE,VALTBL                                                        
         B     VR50                                                             
*                                                                               
VRTNLED  DS    0H                  VALIDATE LEDGER EXISTS                       
         BAS   RE,VALLED                                                        
         BAS   RE,VALLEDUP         CAN'T HAVE DUPLICATE LEDGER SETTINGS         
         B     VR50                                                             
*                                                                               
VRTNOFF  DS    0H                  VALIDATE OFFICE LIST                         
         BAS   RE,VALOFF                                                        
         B     VR50                                                             
*                                                                               
VRTNNUM  BAS   RE,VALNUM                                                        
         B     VR50                                                             
*                                                                               
VRTNCON  BAS   RE,VAL2OR3          CONTRA LEVEL OF 2 OR 3 ONLY                  
         B     VR50                                                             
*                                                                               
VRTMUPL  BAS   RE,VALMUPL          VALIDATE MULTIPLE SETTINGS                   
         B     VR50                                                             
*                                                                               
VRT1NAC  BAS   RE,VAL1NA           VALIDATE MULTIPLE SETTINGS                   
         B     VR50                                                             
*                                                                               
VRTNUM2  BAS   RE,VALNUM2                                                       
         B     VR50                                                             
*                                                                               
VRTMU2L  BAS   RE,VALMUPL                                                       
         B     VR50                                                             
*                                                                               
VR50     TM    BITS,NEWREC                                                      
         BO    VR60                                                             
*                                                                               
VR55     L     R6,AIO              LOOK FOR EXISTING ELEMENT TO DELETE          
         USING OPDELD,R6                                                        
         MVI   ELCODE,OPDELQ                                                    
         BAS   RE,GETEL                                                         
         B     *+8                                                              
VR52     BAS   RE,NEXTEL                                                        
         BNE   VR60                THEN ADD ELEM                                
         CLC   DISNUMB,OPDNUM                                                   
         BNE   VR52                                                             
*                                                                               
         CLI   DISNUMB,COMCT#      CAN'T CLEAR USE BRO TIMESHEETS               
         BNE   *+12                                                             
         CLI   DSPSETH+5,0                                                      
         BE    EINVSET                                                          
*                                                                               
         CLI   DISNUMB,COTHR#      CAN'T CLEAR THIRD PARTY USE                  
         BNE   *+12                                                             
         CLI   DSPSETH+5,0                                                      
         BE    EINVSET                                                          
*                                                                               
         CLI   DISNUMB,COIOFFL#    USE INDIRECT OFFICE LIST                     
         BE    *+12                                                             
         CLI   DISNUMB,COISOFL#    USE IND SAL OFFICE LIST                      
         BNE   VR56                                                             
                                                                                
*                                                                               
         CLI   DSPSETH+5,0                                                      
         BE    ENODEL              CAN'T CLEAR OFFICE LIST                      
*                                                                               
VR56     MVI   0(R6),X'FF'         REMOVE ELEMENT                               
         MVI   ELCODE,X'FF'                                                     
         GOTO1 REMELEM                                                          
         OI    BITS,CHANGES                                                     
*                                                                               
VR60     DS    0H                                                               
         BAS   RE,CHKMISC          MISCELLANEOUS PROFILE RULES                  
         GOTO1 =A(CHKCOUPL),DMCB,RR=RELO  COUPLED PROF CHANGE NEEDED            
         CLI   DSPSETH+5,0         NEW SETTING                                  
         BE    *+8                                                              
         BAS   RE,MKPROEL          MAKE AND ADD PROFILE ELEMENT                 
         B     VR20NX                                                           
*                                                                               
VR100    DS    0H                  NEED TO WRITE OR ADD REC?                    
         TM    BITS,CHANGES                                                     
         BNO   VRX                                                              
*                                                                               
         NI    BITS,X'FF'-MADETAB                                               
         TM    BITS,NEWREC                                                      
         BO    VR120                                                            
*                                                                               
         GOTO1 =A(ANYELEMS),DMCB,RR=RELO  ARE THERE ANY ELEMENTS ON REC         
         MVC   SVKEY2,BIGKEY       TO WRITE BACK REC WITH CHANGES               
*                                                                               
         XC    BIGKEY,BIGKEY                                                    
         MVC   BIGKEY(L'SAVEKEY),SAVEKEY      READ REC AGAIN FOR UPDATE         
         MVC   AIO,AIO2            BUT INTO AIO2 SO DON'T ERASE CHANGES         
         MVI   RDUPDATE,C'Y'                                                    
         OI    DMINBTS,X'08'       PASS BACK DELETED RECS                       
         GOTO1 HIGH                                                             
         GOTO1 GETREC                                                           
         MVC   AIO,AIO1                                                         
         MVC   BIGKEY,SVKEY2                                                    
         GOTO1 PUTREC                                                           
         GOTO1 WRITE               BUT WRITE OUT NEW REC IN AIO1                
         B     VR140                                                            
*                                                                               
VR120    DS    0H                  ADD NEW RECORD                               
         GOTO1 ADDREC                                                           
         NI    BITS,X'FF'-NEWREC                                                
*                                                                               
VR140    TM    BITS2,DTIMDATQ+MCSTDATQ  DATE ELEMENT UPDATE REQUIRED            
         BZ    VRX                                                              
         GOTO1 =A(DTIMDATE),DMCB,RR=RELO                                        
*                                                                               
VRX      MVI   IOOPT,C'Y'          ALREADY DID OWN IO                           
         B     DR                                                               
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*                MAKE PROFILE ELEMENT                                 *         
***********************************************************************         
*                                                                               
         USING DISTABD,R4                                                       
MKPROEL  NTR1                                                                   
         OC    SETTING,SETTING                                                  
         BZ    XIT                 SKIP IF NO SETTING                           
*                                                                               
         XC    ELEM,ELEM                                                        
         LA    R6,ELEM                                                          
         USING OPDELD,R6                                                        
         MVI   OPDEL,OPDELQ                                                     
         MVC   OPDNUM,DISNUMB                                                   
         L     R1,ASECBLK                                                       
         USING SECD,R1                                                          
         MVC   OPDPERS,SECPID                                                   
         DROP  R1                                                               
         GOTO1 DATCON,DMCB,(5,0),(1,OPDLAST)                                    
         LA    R0,OPDLN1Q                                                       
         ZIC   R1,SETLEN                                                        
         AR    R0,R1                                                            
         STC   R0,OPDLN                                                         
         BCTR  R1,0                                                             
         MVC   OPDDATA(0),SETTING                                               
         EX    R1,*-6                                                           
*                                                                               
         GOTO1 ADDELEM                                                          
         OI    BITS,CHANGES                                                     
         B     XIT                                                              
         DROP  R6,R4                                                            
         EJECT                                                                  
***********************************************************************         
*        ANY MISCELLANEOUS PROFILE RULES                              *         
***********************************************************************         
*                                                                               
         USING DISTABD,R4                                                       
CHKMISC  NTR1                                                                   
         CLI   DISNUMB,COVBASE#    #70 - AOY-INCLUDE OVERHEAD IN ALLOC          
         BNE   CHKM02                                                           
         CLI   SETTING,COOPTYQ     SET TO YES                                   
         BNE   CHKMYES                                                          
         CLI   CODBASE,COYTD       INVALID IF DIRECT ALLOC IS YTD               
         BNE   CHKMYES                                                          
         B     CHKMNO                                                           
*                                                                               
CHKM02   CLI   DISNUMB,COONUPD#    #28 - ON LINE UPDATE ON BUCKETS              
         BNE   CHKM04                                                           
         CLI   SETTING,COOPTYQ     SET TO YES                                   
         BNE   CHKMYES                                                          
         CLI   CODBASE,COMTHLY     INVALID IF DIRECT ALLOC IS YTD               
         BNE   CHKMNO                                                           
         CLI   COHRRAT,COOPTNQ     INVALID IF INCLUDE HOURLY RATES              
         BE    CHKMNO              IS NO                                        
         CLI   COINC,COOPTNQ       INVALID IF INCLUDE IN ALLOCATION             
         BE    CHKMNO              IS NO                                        
                                                                                
CHKM04   CLI   DISNUMB,CODBASE#    #22 - DIRECT TIME ALLOCATION                 
         BNE   CHKM06              NO                                           
         CLI   SETTING,COYTD       SET TO YEAR TO DATE                          
         BNE   CHKMYES                                                          
         CLI   COONUPD,COOPTNQ     INVALID IF ONLINE UPDATE IS YES              
         BNE   CHKMNO                                                           
*                                                                               
CHKM06   CLI   DISNUMB,COINC#      #8 - INCLUDE IN ALLOCATION                   
         BNE   CHKM08              NO                                           
         CLI   SETTING,COOPTNQ     SET TO NO                                    
         BNE   CHKMYES                                                          
         CLI   COONUPD,COOPTNQ     INVALID IF ONLINE UPDATE IS YES              
         BNE   CHKMNO                                                           
*                                                                               
CHKM08   CLI   DISNUMB,COHRRAT#    #13 - INCLUDE HOURLY RATES                   
         BNE   CHKM10              NO                                           
         CLI   SETTING,COOPTNQ     SET TO NO                                    
         BNE   CHKMYES                                                          
         CLI   COONUPD,COOPTNQ     INVALID IF ONLINE UPDATE IS YES              
         BNE   CHKMNO                                                           
*                                                                               
CHKM10   CLI   DISNUMB,CODTMS#     #29 - DAILY TIME INPUT                       
         BNE   CHKM12              NO                                           
         MVC   DTIMEST,SETTING                                                  
         OI    BITS2,DTIMDATQ      SET BIT                                      
         B     CHKMYES                                                          
*                                                                               
CHKM12   CLI   DISNUMB,COMCT#      #100- MCS TIMESHEETS IN USE                  
         BNE   CHKM14                                                           
         MVC   MCSTIME,SETTING                                                  
         OI    BITS2,MCSTDATQ      SET BIT                                      
         CLI   SETTING,COOPTNQ     SET TO NO                                    
         BE    CHKMYES                                                          
*&&US                                                                           
         TM    LEVBIT,LEVPER       IF SETTING TO YES AT THE PERSON LVL          
         BZ    CHKM13              AND THAT PERSON IS A FREELANCER              
         CLI   FREELNCR,C'Y'       GIVE AN ERROR                                
         BE    CHKMNO                                                           
*&&                                                                             
CHKM13   GOTO1 =A(VALPIDS),DMCB,RR=RELO   CHECK USERS HAVE PIDS                 
*                                                                               
CHKM14   CLI   DISNUMB,COTHR#      #THIRD PARTY AVAILABLE                       
         BNE   CHKMYES                                                          
         CLI   SETTING,COOPTNQ     CAN ONLY SWITCH ON NO SWITCH OFF             
         BE    CHKMNO                                                           
         GOTO1 =A(UPDPER),DMCB,RR=RELO                                          
*                                  UPDATE PERSON RECORD WITH SETTING            
CHKMYES  B     XIT                                                              
*                                                                               
CHKMNO   LA    R2,DSPSETH                                                       
         B     EINVSET                                                          
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
*                   VALIDATE SETTING WITH TABLE                       *         
***********************************************************************         
*                                                                               
         USING DSPLINED,R2                                                      
         USING DISTABD,R4                                                       
VALTBL   NTR1                                                                   
*                                                                               
         ICM   R3,15,DISVADR       ADDRESS OF VALIDATION TABLE                  
         USING COPDD,R3                                                         
*                                                                               
         ZIC   R0,COPDNUM          NUMBER OF OPTIONS                            
         LA    R3,COPDENTY         MINI ENTRIES                                 
         USING COPDENTY,R3                                                      
*                                                                               
VT20     L     R1,APUDSLST                                                      
         ZICM  R6,COPDOPT,2        DISP TO A VALID SETTING                      
         AR    R6,R1                                                            
*                                                                               
         ZIC   R1,COPDCLEN                                                      
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R6),DSPSET      VALID SETTING                                
         BE    VT30                                                             
         LA    R3,COPDMLEN(R3)     NEXT MINI ENTRY                              
         BCT   R0,VT20                                                          
         B     VTERR                                                            
*                                                                               
VT30     XC    SETTING,SETTING                                                  
         MVC   SETTING(L'COPDCOB),COPDCOB      EQUIVALENT SETTING               
         MVI   SETLEN,L'COPDCOB                LENGTH OF SETTING                
*                                                                               
         CLI   SETLEN,1            SETTING LEN=1 - MORE VALIDATION              
         BNE   VTX                                                              
*                                                                               
         L     R3,AOFFLTAB         TABLE OF WHERE TO CHECK FOR LIST             
VT31     CLC   DISNUMB,0(R3)       MATCH ON PROF NUMBER                         
         BE    VT32                                                             
         LA    R3,2(R3)                                                         
         CLI   0(R3),X'00'                                                      
         BNE   VT31                                                             
         B     VT40                                                             
*                                                                               
VT32     CLI   SETTING,COSET_L     OFFICE LIST VALIDATION  (OLIST)              
         BE    VT33                                                             
         CLI   SETTING,COSET_N     CLIENT OFFICE LIST VALIDAT (COLIST)          
         BE    VT33                                                             
         CLI   SETTING,COSET_K     CLIENT OFFICE VALIDATION (CLIST)             
         BNE   VT40                                                             
VT33     BAS   RE,VALOLIST                                                      
*                                                                               
         USING CITABD,R6           COST/INCOME TABLE                            
VT40     L     R6,ACITABLE                                                      
VT42     CLC   DISNUMB,CITCI       MATCH ON COST/INCOME PROFILE                 
         BNE   VT50                                                             
         CLI   SETTING,COINCOME    INCOME OR COST                               
         BNE   VTX                                                              
         BAS   RE,VALNODPT         INCOME = NO DEPT AT ANY LEVEL                
         BNE   VTERR                                                            
         BAS   RE,VALNOPER         INCOME = NO PERSON                           
         BE    VTX                                                              
         B     VTERR                                                            
*                                                                               
VT50     CLC   DISNUMB,CITDOA      DEPT/OFF/AGY PROFILE                         
         BNE   VT60                                                             
         CLI   SETTING,COSET_D     IF SET TO DEPT                               
         BNE   VTX                                                              
         BAS   RE,VALNOINC         THEN CAN'T BE SET AT INCOME                  
         BE    VTX                                                              
         B     VTERR                                                            
*                                                                               
VT60     CLC   DISNUMB,CITPC       PERSON/CLIENT PROFILE                        
         BNE   VT70                                                             
         CLI   SETTING,COSTAFF     IF SET TO PERSON                             
         BNE   VTX                                                              
         BAS   RE,VALNOINC         THEN CAN'T BE SET AT INCOME                  
         BE    VTX                                                              
         B     VTERR                                                            
*                                                                               
VT70     LA    R6,CITABLEN(R6)                                                  
         CLI   0(R6),0                                                          
         BNE   VT42                                                             
*                                                                               
VTX      B     XIT                                                              
*                                                                               
VTERR    LA    R2,DSPSETH                                                       
         B     EINVSET                                                          
         DROP  R2,R3,R4,R6                                                      
         EJECT                                                                  
***********************************************************************         
*                   VALIDATE OFFICE LIST EXISTS                       *         
***********************************************************************         
*                                                                               
VALOLIST NTR1                                                                   
         USING DSPLINED,R2                                                      
         USING DISTABD,R4                                                       
*                                                                               
         TM    LEVBIT,LEVOFF       MUST HAVE AT LEAST OFFICE LEVEL              
         BO    *+12                TO SET TO OLIST OR CLIST OR COLIST           
         LA    R2,DSPSETH                                                       
         B     EINVSTLV                                                         
*                                                                               
         L     R3,AOFFLTAB         TABLE OF WHERE TO CHECK FOR LIST             
VOL10    CLC   DISNUMB,0(R3)       MATCH ON PROF NUMBER                         
         BE    VOL20                                                            
         LA    R3,2(R3)                                                         
         CLI   0(R3),X'00'                                                      
         BNE   VOL10                                                            
         DC    H'0'                                                             
*                                                                               
         USING CAPRECD,R6                                                       
VOL20    LA    R6,BIGKEY           GET OFFICE LEVEL RECORD                      
         XC    BIGKEY,BIGKEY                                                    
         MVC   CAPKEY,SPACES                                                    
         MVI   CAPKTYP,CAPKTYPQ    PROFILE REC X'3E09'                          
         MVI   CAPKSUB,CAPKSUBQ                                                 
         MVC   CAPKCPY,CMPY        COMPANY                                      
         MVC   CAPKMTHD,METHNUM                                                 
         MVC   CAPKOFC,OFFICE                                                   
         MVC   KEY2,BIGKEY                                                      
         GOTO1 HIGH                                                             
         CLC   BIGKEY(L'CAPKEY),KEY2                                            
         BNE   VOLERR              NOTHING FOUND                                
         MVC   AIO,AIO2                                                         
         GOTO1 GETREC                                                           
         MVC   AIO,AIO1                                                         
*                                                                               
         USING OPDELD,R6                                                        
         L     R6,AIO2             LOOK FOR EXISTING OFFICE LIST ELEM           
         MVI   ELCODE,OPDELQ       X'A4'                                        
         BAS   RE,GETEL                                                         
         B     *+8                                                              
VOL30    BAS   RE,NEXTEL                                                        
         BNE   VOLERR                                                           
*                                                                               
VOL40    CLC   1(1,R3),OPDNUM        MATCH ON LIST PROF NUMBER                  
         BNE   VOL30                                                            
         B     XIT                                                              
*                                                                               
VOLERR   LA    R2,DSPSETH                                                       
         B     ENOOFL              MISSING OFFICE LIST                          
         DROP  R4,R6,R2                                                         
         EJECT                                                                  
***********************************************************************         
*        VALIDATE NO DEPT SETTING FOR CITDOA                                    
***********************************************************************         
*                                                                               
         USING CITABD,R6           COST/INCOME TABLE                            
         USING CAPRECD,R2                                                       
VALNODPT NTR1                      READ RECS FOR DEPT SETTING                   
         MVC   BYTE,CITDOA         BYTE HAS PROFILE NUMBER TO MATCH             
         DROP  R6                                                               
*                                                                               
         LA    R2,BIGKEY           MUST CHECK METHOD AND OFFICE LEVELS          
         XC    BIGKEY,BIGKEY                                                    
         MVC   CAPKEY,SPACES                                                    
         MVI   CAPKTYP,CAPKTYPQ    PROFILE REC                                  
         MVI   CAPKSUB,CAPKSUBQ                                                 
         MVC   CAPKCPY,CMPY        COMPANY                                      
         MVC   CAPKMTHD,METHNUM                                                 
         MVC   KEY2,BIGKEY                                                      
         GOTO1 HIGH                                                             
         B     VND10                                                            
VND10SEQ GOTO1 SEQ                                                              
VND10    CLC   BIGKEY(CAPKMIN),KEY2        SAME METHOD                          
         BNE   VNDYES              NOTHING FOUND                                
         CLC   CAPKDPT,SPACES      SKIP DPT AND LOWER LEVELS                    
         BNE   VND10SEQ                                                         
         MVC   AIO,AIO2                                                         
         GOTO1 GETREC                                                           
         MVC   AIO,AIO1                                                         
*                                                                               
         L     R6,AIO2             LOOK FOR EXISTING DEPT SETTING               
         USING OPDELD,R6                                                        
         MVI   ELCODE,OPDELQ                                                    
         BAS   RE,GETEL                                                         
         B     *+8                                                              
VND20    BAS   RE,NEXTEL                                                        
         BNE   VND10SEQ                                                         
         CLC   BYTE,OPDNUM         MATCH ON PROF NUMBER                         
         BNE   VND20                                                            
         CLI   OPDDATA,COSET_D     DEPT                                         
         BNE   VND10SEQ                                                         
VNDNO    B     XNO                                                              
VNDYES   B     XYES                                                             
         DROP  R2,R6                                                            
         EJECT                                                                  
***********************************************************************         
*        VALIDATE NO PERSON SETTING FOR CITPC                                   
***********************************************************************         
*                                                                               
         USING CITABD,R6           COST/INCOME TABLE                            
         USING CAPRECD,R2                                                       
VALNOPER NTR1                      READ RECS FOR DEPT SETTING                   
         MVC   BYTE,CITPC          BYTE HAS PROFILE NUMBER TO MATCH             
         DROP  R6                                                               
*                                                                               
         USING DISTABD,R4                                                       
         LA    R4,DISTABLE          R4 POINTER TO DISPLAY TABLE                 
VNP10    CLC   BYTE,DISNUMB                                                     
         BE    VNP20                                                            
         LA    R4,DISTABLN(R4)     NEXT TABLE ENTRY                             
         CLI   0(R4),0             ANYMORE ENTRIES                              
         BNE   VNP10                                                            
         B     VNPYES                                                           
*                                                                               
VNP20    CLI   DISSETQ,COSTAFF     PERSON                                       
         BNE   VNPYES                                                           
VNPNO    B     XNO                                                              
VNPYES   B     XYES                                                             
         DROP  R2,R4                                                            
         EJECT                                                                  
***********************************************************************         
*        VALIDATE NO INCOME SETTING FOR CITCI                                   
***********************************************************************         
*                                                                               
         USING CITABD,R6           COST/INCOME TABLE                            
         USING CAPRECD,R2                                                       
VALNOINC NTR1                      READ RECS FOR INCOME SETTING                 
         MVC   BYTE,CITCI          BYTE HAS PROFILE NUMBER TO MATCH             
         DROP  R6                                                               
*                                                                               
         USING DISTABD,R4                                                       
         LA    R4,DISTABLE          R4 POINTER TO DISPLAY TABLE                 
VNI10    CLC   BYTE,DISNUMB                                                     
         BE    VNI20                                                            
         LA    R4,DISTABLN(R4)     NEXT TABLE ENTRY                             
         CLI   0(R4),0             ANYMORE ENTRIES                              
         BNE   VNI10                                                            
         B     VNIYES                                                           
*                                                                               
VNI20    CLI   DISSETQ,COINCOME    INCOME                                       
         BNE   VNIYES                                                           
VNINO    B     XNO                                                              
VNIYES   B     XYES                                                             
         DROP  R2,R4                                                            
         EJECT                                                                  
***********************************************************************         
*              VALIDATE NUMBER UP TO 4 DECIMAL PLACES                 *         
***********************************************************************         
*                                                                               
VALNUM   NTR1                                                                   
         USING DISTABD,R4                                                       
         USING DSPLINED,R2                                                      
*                                                                               
VH05     ZIC   R0,DSPSETH+5                                                     
         GOTO1 CASHVAL,DMCB,(X'80',DSPSET),(R0)                                 
         ORG   *-2                                                              
         MVC   BYTE,DISVXTRA                                                    
         NI    BYTE,DISVNDPQ       CLEAR ALL BUT N'DECIMAL PLACES               
         BNZ   *+8                                                              
         MVI   DMCB+4,X'40'        ZERO, SO SET NO DECIMALS ALLOWED             
         OC    DMCB(1),BYTE                                                     
         BASR  RE,RF                                                            
         CLI   DMCB,X'FF'                                                       
         BNE   VH10                                                             
         LA    R2,DSPSETH                                                       
         B     EINVNUM                                                          
*                                                                               
VH10     XC    SETTING,SETTING                                                  
         ZAP   SETTING(4),DMCB+8(4)                                             
         TM    DISVXTRA,X'FF'-DISVNDPQ  TEST ANY RANGE VALIDATION REQ'D         
         BZ    VHOK                NO                                           
         LA    R2,DSPSETH          ELSE TEST RANGE                              
         CP    SETTING(4),PZERO                                                 
         BH    VH12                                                             
         BL    EINVIFL                                                          
         TM    DISVXTRA,DISVRNG3+DISVRNG4                                       
         BNZ   VH12                                                             
         B     EINVIFL             ZERO NOT VALID                               
VH12     CP    SETTING(4),P03      (DISVRNG1)                                   
         BNH   VHOK                                                             
         TM    DISVXTRA,DISVRNG4                                                
         BZ    VH13                                                             
         CP    SETTING(4),P04      (DISVRNG4)                                   
         BNH   VHOK                                                             
VH13     TM    DISVXTRA,DISVRNG5                                                
         BZ    VH14                                                             
         CP    SETTING(4),P50                                                   
         BNH   VHOK                                                             
VH14     TM    DISVXTRA,DISVRNG2+DISVRNG3                                       
         BZ    EINVIFL                                                          
         CP    SETTING(4),P99                                                   
         BH    EINVIFL                                                          
VHOK     MVI   SETLEN,4                                                         
         B     XIT                                                              
         DROP  R2,R4                                                            
                                                                                
PZERO    DC    P'0'                                                             
P03      DC    P'3'                                                             
P04      DC    P'4'                                                             
P50      DC    P'50'                                                            
P99      DC    P'99'                                                            
***********************************************************************         
*              VALIDATE NUMBER UP TO 4 DECIMAL PLACES                 *         
***********************************************************************         
*                                                                               
VALNUM2  NTR1                                                                   
         USING DISTABD,R4                                                       
         USING DSPLINED,R2                                                      
*                                                                               
VN2H05   ZIC   R0,DSPSETH+5                                                     
         GOTO1 CASHVAL,DMCB,(X'80',DSPSET),(R0)                                 
         ORG   *-2                                                              
         MVC   BYTE,DISVXTRA                                                    
         NI    BYTE,DISVNDPQ       CLEAR ALL BUT N'DECIMAL PLACES               
         BNZ   *+8                                                              
         MVI   DMCB+4,X'40'        ZERO, SO SET NO DECIMALS ALLOWED             
         OC    DMCB(1),BYTE                                                     
         BASR  RE,RF                                                            
         CLI   DMCB,X'FF'                                                       
         BNE   VN2H10                                                           
         LA    R2,DSPSETH                                                       
         B     EINVNUM                                                          
*                                                                               
VN2H10   XC    SETTING,SETTING                                                  
         ZAP   SETTING(4),DMCB+8(4)                                             
         TM    DISVXTRA,X'FF'-DISVNDPQ  TEST ANY RANGE VALIDATION REQ'D         
         BZ    VHOK                NO                                           
         LA    R2,DSPSETH          ELSE TEST RANGE                              
         TM    DISVXTRA,DISVR2G2+DISVR2G3   0-10,0-365                          
         BNZ   VN2H12                                                           
         CP    SETTING(4),PZERO                                                 
         BH    VN2H18                                                           
         BL    EINVIFL                                                          
         B     EINVIFL             ZERO NOT VALID                               
*                                                                               
VN2H12   CLC   SETTING,SPACES      BLANK IS OK                                  
         BE    VHOK                                                             
         OC    SETTING,SETTING     SO IS NULL                                   
         BZ    VHOK                                                             
         CP    SETTING(4),PZERO    -VE NOT ALLOWED                              
         BL    EINVIFL                                                          
         TM    DISVXTRA,DISVR2G2   0-10                                         
         BZ    VN2H14                                                           
         CP    SETTING(4),P10                                                   
         BNH   VHOK                                                             
         B     EINVIFL             ERROR NOT VALID                              
*                                                                               
VN2H14   TM    DISVXTRA,DISVR2G3   0-365                                        
         BZ    EINVIFL                                                          
         CP    SETTING(4),P365                                                  
         BNH   VHOK                                                             
         B     EINVIFL                                                          
*                                                                               
VN2H18   TM    DISVXTRA,DISVR2G1                                                
         BZ    EINVIFL                                                          
*                                                                               
VN2H20   CP    SETTING(4),P12      (DISVR2G1)                                   
         BNH   VHOK                                                             
         B     EINVIFL             ERROR NOT VALID                              
         DROP  R2,R4                                                            
                                                                                
P12      DC    P'12'                                                            
P10      DC    P'10'                                                            
P365     DC    P'365'                                                           
         EJECT                                                                  
***********************************************************************         
*              VALIDATE MULTIPLE SETTING PROFILES                     *         
***********************************************************************         
*                                                                               
         USING DSPLINED,R2                                                      
         USING DISTABD,R4                                                       
VALMUPL  NTR1                                                                   
         OI    BITS,FIRST                                                       
         XC    SETTING,SETTING                                                  
         LA    R6,SETTING                                                       
*                                                                               
         ST    R4,SAVER4                                                        
         CLI   DISNUMB,COFTA#      IS IT FTA?                                   
         BNE   VALMPL1                                                          
         CLI   DSPSETH+5,7         THEN OK TO OVERRIDE 5                        
         BH    VNMPLERR                                                         
         B     *+12                                                             
*                                                                               
VALMPL1  CLI   DSPSETH+5,5                                                      
         BH    VNMPLERR                                                         
         CLI   DSPSETH+5,1                                                      
         BNE   VALMPL2                                                          
         LA    R4,1                SET TO VALIDATE ONE CHARACTER                
         MVI   SETLEN,1            SET LENGTH OF SETTING                        
         B     VALMPL6                                                          
VALMPL2  CLI   DSPSETH+5,3                                                      
         BNE   VALMPL4                                                          
         LA    R4,2                SET TO VALIDATE TWO CHARACTERS               
         MVI   SETLEN,3            SET LENGTH OF SETTING                        
         B     VALMPL6                                                          
VALMPL4  CLI   DSPSETH+5,5                                                      
         BNE   VALMPL5                                                          
         LA    R4,3                SET TO VALIDATE THREE CHARACTERS             
         MVI   SETLEN,5            SET LENGTH OF SETTING                        
         B     VALMPL6                                                          
VALMPL5  CLI   DISNUMB,COFTA#      IS IT FTA?                                   
         BNE   VNMPLERR                                                         
         CLI   DSPSETH+5,7                                                      
         BNE   VNMPLERR                                                         
         LA    R4,4                SET TO VALIDATE FOUR  CHARACTERS             
         MVI   SETLEN,7            SET LENGTH OF SETTING                        
*                                                                               
VALMPL6  LA    R3,DSPSET           POINT TO INPUT SETTING                       
VALMPL8  CLI   0(R3),C'B'          B TIME                                       
         BNE   VALMPL8A                                                         
         TM    BITS,FIRST           FIRST TIME IN                               
         BO    VALMPL10            THEN DON'T CHECK FOR DUPS                    
         GOTO1 =A(CHKDUPS),DMCB,(3,(R3)),RR=RELO                                
         BNE   VNMPLERR                                                         
         B     VALMPL10                                                         
VALMPL8A CLI   0(R3),C'R'          R TIME                                       
         BNE   VALMPL8B                                                         
         TM    BITS,FIRST           FIRST TIME IN                               
         BO    VALMPL10            THEN DON'T CHECK FOR DUPS                    
         GOTO1 =A(CHKDUPS),DMCB,(3,(R3)),RR=RELO                                
         BNE   VNMPLERR                                                         
         B     VALMPL10                                                         
VALMPL8B CLI   0(R3),C'N'          N TIME                                       
         BNE   VALMPL8C                                                         
         TM    BITS,FIRST           FIRST TIME IN                               
         BO    VALMPL10            THEN DON'T CHECK FOR DUPS                    
         GOTO1 =A(CHKDUPS),DMCB,(3,(R3)),RR=RELO                                
         BNE   VNMPLERR                                                         
         B     VALMPL10                                                         
VALMPL8C ST    R4,SAVER3                                                        
         L     R4,SAVER4                                                        
         CLI   DISNUMB,COFTA#      IS IT FTA?                                   
         BNE   VNMPLERR                                                         
         L     R4,SAVER3                                                        
         CLI   0(R3),C'C'           C TIME                                      
         BNE   VNMPLERR                                                         
         TM    BITS,FIRST           FIRST TIME IN                               
         BO    VALMPL10            THEN DON'T CHECK FOR DUPS                    
         GOTO1 =A(CHKDUPS),DMCB,(3,(R3)),RR=RELO                                
         BNE   VNMPLERR                                                         
*                                                                               
VALMPL10 NI    BITS,X'FF'-FIRST                                                 
         MVC   0(1,R6),0(R3)       MOVE TIME INTO SETTING                       
         LA    R6,1(R6)            BUMP SETTING FIELD                           
         C     R4,=F'1'                                                         
         BE    XIT                                                              
         MVI   0(R6),C','                                                       
         LA    R6,1(R6)                                                         
         LA    R3,1(R3)                                                         
         CLI   0(R3),C','          MUST HAVE COMMAS IN BETWEEN                  
         BNE   VNMPLERR                                                         
         LA    R3,1(R3)            BUMP PAST COMMA AND INTO NXT SET FLD         
VALMPL12 BCT   R4,VALMPL8                                                       
*                                                                               
*ALMPLX  B     XIT                                                              
*                                                                               
VNMPLERR LA    R2,DSPSETH                                                       
         B     EINVSET                                                          
         DROP  R2,R4                                                            
         EJECT                                                                  
***********************************************************************         
*              VALIDATE CONTRA LEVEL OF 2 OR 3                        *         
***********************************************************************         
*                                                                               
         USING DSPLINED,R2                                                      
VAL2OR3  NTR1                                                                   
         CLI   DSPSETH+5,1                                                      
         BNE   V23ERR                                                           
         CLI   DSPSET,C'2'                                                      
         BE    V23A                                                             
         CLI   DSPSET,C'3'                                                      
         BNE   V23ERR                                                           
V23A     XC    SETTING,SETTING                                                  
         MVC   SETTING(1),DSPSET                                                
         MVI   SETLEN,1                                                         
         B     XIT                                                              
V23ERR   LA    R2,DSPSETH                                                       
         B     EINVSET                                                          
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
*                   VALIDATE LEDGER SETTING                           *         
***********************************************************************         
*                                                                               
VALLED   NTR1                                                                   
*                                                                               
         USING DSPLINED,R2                                                      
         USING DISTABD,R4                                                       
*                                                                               
         ICM   R3,15,DISVADR       ADDRESS OF VALIDATION TABLE                  
         USING COPDD,R3                                                         
*                                                                               
         ZIC   R0,COPDNUM          NUMBER OF OPTIONS                            
         LA    R3,COPDENTY         MINI ENTRIES                                 
         USING COPDENTY,R3                                                      
*                                                                               
VL20     CLC   COPDPRO,DISNUMB     FIND SAME PROFILE NUMBER                     
         BE    VL30                                                             
         LA    R3,COPDLLEN(R3)     NEXT MINI ENTRY                              
         BCT   R0,VL20                                                          
         B     VL38                                                             
*                                                                               
VL30     DS    0H                  MATCH                                        
         LA    R6,KEY2             BUILD KEY                                    
         USING ACTRECD,R6                                                       
*                                                                               
         MVC   LEDNUM,COPDLED      SAVE LEDGER NUMBER FOR VALLEDUP              
*                                                                               
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,CMPY        COMPANY                                      
         MVC   ACTKUNT(L'COPDLED),COPDLED                                       
         ZIC   R1,DSPSETH+5                                                     
         BCTR  R1,0                                                             
         MVC   ACTKACT(0),DSPSET                                                
         EX    R1,*-6                                                           
*                                                                               
         L     R6,AIO2                                                          
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'ACCOUNT',KEY2,AIO2                    
         CLC   KEY2,0(R6)                                                       
         BE    VL40                INVALID ACCOUNT                              
VL38     LA    R2,DSPSETH                                                       
         B     EINVACC             INVALID ACCOUNT                              
*                                                                               
VL40     XC    SETTING,SETTING                                                  
         ZIC   R1,DSPSETH+5                                                     
         STC   R1,SETLEN                                                        
         BCTR  R1,0                                                             
         MVC   SETTING(0),DSPSET                                                
         EX    R1,*-6                                                           
*                                                                               
         B     XIT                                                              
                                                                                
         DROP  R2,R3,R4,R6                                                      
         EJECT                                                                  
***********************************************************************         
*          VALIDATE LEDGER SETTING NOT DUPLICATED                     *         
***********************************************************************         
*                                                                               
VALLEDUP NTR1                                                                   
*                                                                               
         USING DSPLINED,R2                                                      
         USING DISTABD,R4                                                       
*                                                                               
         ICM   R3,15,DISVADR       ADDRESS OF VALIDATION TABLE                  
         USING COPDD,R3                                                         
*                                                                               
         ZIC   R0,COPDNUM          NUMBER OF OPTIONS                            
         LA    R3,COPDENTY         MINI ENTRIES                                 
         USING COPDENTY,R3                                                      
*                                                                               
VD20     CLC   COPDLED,LEDNUM      FIND SAME LEDGER                             
         BE    VD30                                                             
VD25     LA    R3,COPDLLEN(R3)     NEXT MINI ENTRY                              
         BCT   R0,VD20                                                          
         B     VD50                                                             
*                                                                               
VD30     DS    0H                  MATCH                                        
         LA    R1,COPTIONS                                                      
         MVC   HALFWD,COPSDISP                                                  
         AH    R1,HALFWD           DISPLACEMENT TO SETTING                      
         CLC   DSPSET(1),0(R1)                                                  
         BNE   VD25                                                             
         LA    R2,DSPSETH                                                       
         B     EDUP15              DUPLICATE LEDGER SETTING                     
*                                                                               
*     GO BACK AND FILL IN NEW SETTING IN CASE MORE THAN 1 CHANGED               
*                                                                               
VD50     ICM   R3,15,DISVADR       ADDRESS OF VALIDATION TABLE                  
         USING COPDD,R3                                                         
*                                                                               
         ZIC   R0,COPDNUM          NUMBER OF OPTIONS                            
         LA    R3,COPDENTY         MINI ENTRIES                                 
         USING COPDENTY,R3                                                      
*                                                                               
VD55     CLC   COPDPRO,DISNUMB     FIND SAME PROFILE NUMBER                     
         BE    VD60                                                             
         LA    R3,COPDLLEN(R3)     NEXT MINI ENTRY                              
         BCT   R0,VD55                                                          
         LA    R2,DSPSETH                                                       
         B     EINVACC                                                          
*                                                                               
VD60     DS    0H                  MATCH                                        
         LA    R1,COPTIONS                                                      
         MVC   HALFWD,COPSDISP                                                  
         AH    R1,HALFWD           DISPLACEMENT TO SETTING                      
         MVC   0(1,R1),DSPSET                                                   
*                                                                               
VDX      B     XIT                                                              
         DROP  R2,R3,R4                                                         
         EJECT                                                                  
*                                                                               
***********************************************************************         
*                   VALIDATE 1N ACCOUNT                               *         
***********************************************************************         
*                                                                               
VAL1NA   NTR1                                                                   
*                                                                               
         USING DSPLINED,R2                                                      
         USING DISTABD,R4                                                       
*                                                                               
         LA    R6,KEY2             BUILD KEY                                    
         USING ACTRECD,R6                                                       
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,CMPY        COMPANY                                      
         MVC   ACTKUNT(2),=C'1N'                                                
         SR    R1,R1                                                            
         IC    R1,DSPSETH+5                                                     
         AHI   R1,-1                                                            
         MVC   ACTKACT(0),DSPSET                                                
         EX    R1,*-6                                                           
*                                                                               
         L     R6,AIO2                                                          
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'ACCOUNT',KEY2,AIO2                    
         CLC   KEY2,0(R6)                                                       
         BNE   V1NERR                                                           
         L     R6,AIO2                                                          
         SR    R0,R0                                                            
         LA    RF,ACCORFST(R6)                                                  
         USING ABLELD,RF                                                        
V1N02    CLI   ABLEL,0                                                          
         BE    V1NERR                                                           
         CLI   ABLEL,ABLELQ        LOOK FOR BALANCE EL = LOW-LVL A/C            
         BE    V1NX                                                             
         IC    R0,ABLLN                                                         
         AR    RF,R0                                                            
         B     V1N02                                                            
                                                                                
V1NERR   LA    R2,DSPSETH                                                       
         B     EINVACC                                                          
*                                                                               
V1NX     XC    SETTING,SETTING                                                  
         SR    R1,R1                                                            
         IC    R1,DSPSETH+5                                                     
         STC   R1,SETLEN                                                        
         AHI   R1,-1                                                            
         MVC   SETTING(0),DSPSET                                                
         EX    R1,*-6                                                           
         B     XIT                                                              
         DROP  R2,R4,R6,RF                                                      
         EJECT                                                                  
*                                                                               
***********************************************************************         
*                   VALIDATE OFFICE LIST                              *         
***********************************************************************         
*                                                                               
VALOFF   NTR1                                                                   
*                                                                               
         USING DSPLINED,R2                                                      
         USING DISTABD,R4                                                       
*                                                                               
         TM    BITS,ONEBYTOF       AGY ON 1 BYTE OFFICES                        
         BO    VO30                                                             
*                                                                               
VO10     CLI   DSPSETH+5,2         MUST BE 2 OR LESS                            
         BH    VO50                                                             
         LA    R6,KEY2             BUILD KEY                                    
         USING OFFRECD,R6                                                       
         MVC   OFFKEY,SPACES                                                    
         MVI   OFFKTYP,OFFKTYPQ    OFFICE RECORDS                               
         MVC   OFFKCPY,CMPY        COMPANY                                      
         ZIC   R1,DSPSETH+5                                                     
         BCTR  R1,0                                                             
         MVC   OFFKOFF(0),DSPSET                                                
         EX    R1,*-6                                                           
*                                                                               
         L     R6,AIO2                                                          
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'ACCOUNT',KEY2,AIO2                    
         CLC   KEY2,0(R6)                                                       
         BE    VO20                                                             
VO15     LA    R2,DSPSETH                                                       
         B     EINVOFFL                                                         
*                                                                               
VO20     DS    0H                                                               
*        TM    OFFRSTAT,OFFSLIST   IS THIS A LIST CODE                          
*        BNO   VO15                                                             
*                                                                               
         XC    SETTING,SETTING                                                  
         MVI   SETLEN,X'02'                                                     
         MVC   SETTING(2),OFFKOFF                                               
         B     VOX                                                              
*                                                                               
VO30     CLI   DSPSETH+5,2         MUST BE 2 OR LESS                            
         BH    VO50                                                             
*                                                                               
VO40     XC    DUB(3),DUB                                                       
         MVI   DUB+1,C'$'       FIRST BYTE IS ZERO                              
         MVC   DUB+2(1),DSPSET                                                  
         LA    R6,KEY2                                                          
         USING CTUREC,R6                                                        
         XC    KEY2,KEY2                                                        
         MVI   CTUKTYP,C'U'                                                     
         MVI   CTUKSYS,C'A'                                                     
         MVC   CTUKPROG,DUB          OFFICE LIST                                
         MVC   CTUKAGY,ALPHA       2 BYTE ALPHA ID                              
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'CTFILE ',KEY2,AIO2                    
         L     R6,AIO2                                                          
         CLC   KEY2(CTUKMED-CTUKEY),0(R6)   CLC UP TO AGY                       
         BE    *+12                                                             
VO50     LA    R2,DSPSETH                                                       
         B     EINVOFL                                                          
*                                                                               
         XC    SETTING,SETTING                                                  
         MVI   SETLEN,1                                                         
         MVC   SETTING(1),DSPSET                                                
*                                                                               
VOX      B     XIT                                                              
         DROP  R2,R4,R6                                                         
         EJECT                                                                  
*                                                                               
***********************************************************************         
*                       DISPLAY KEY                                   *         
***********************************************************************         
*                                                                               
DK       DS    0H                                                               
*                                                                               
         GOTO1 =A(VALOPTS),DMCB,RR=RELO                                         
*                                                                               
         MVC   SAVEKEY,BIGKEY                                                   
*                                                                               
         LA    R6,BIGKEY                                                        
         USING CAPRECD,R6                                                       
         CLC   CAPKOFC,SPACES      ANY OFFICE CODE                              
         BNH   *+20                                                             
         MVC   PROOFF(L'CAPKOFC),CAPKOFC                                        
         OI    PROOFFH+6,X'80'                                                  
         MVC   PROOFFH+5(1),LEVELLN                                             
         CLC   CAPKDPT,SPACES      ANY DEPARTMENT CODE                          
         BNH   *+20                                                             
         MVC   PRODEPT(L'CAPKDPT),CAPKDPT                                       
         OI    PRODEPTH+6,X'80'                                                 
         MVC   PRODEPTH+5(1),LEVELLN+1                                          
         CLC   CAPKSDT,SPACES      ANY SUB-DEPARTMENT CODE                      
         BNH   *+20                                                             
         MVC   PROSDPT(L'CAPKSDT),CAPKSDT                                       
         OI    PROSDPTH+6,X'80'                                                 
         MVC   PROSDPTH+5(1),LEVELLN+2                                          
         CLC   CAPKPER,SPACES      ANY PERSON CODE                              
         BNH   *+20                                                             
         MVC   PROPER,CAPKPER                                                   
         OI    PROPERH+6,X'80'                                                  
         MVC   PROPERH+5(1),LEVELLN+3                                           
         GOTO1 =A(VALODSP),DMCB,RR=RELO                                         
*                                  VALIDATE OFFICE DEPT SUB-DEPT PERSON         
         MVC   SVMETHNM,CAPKMTHD   SAVE NUMBER                                  
         MVC   PROMETH,SPACES                                                   
         CLC   SVMETHNM,SPACES     NO METHOD                                    
         BE    DK20                                                             
*                                                                               
         USING CAHRECD,R6                                                       
         LA    R6,BIGKEY           GET METHOD CODE TO DISPLAY                   
         XC    BIGKEY,BIGKEY                                                    
         MVC   CAHKEY,SPACES                                                    
         MVI   CAHKTYP,CAHKTYPQ    ALLOCATION METHOD RECORD TYPE BY NUM         
         MVI   CAHKSUB,CAHKSUBQ    METHOD BY CODE SUB TYPE                      
         MVC   CAHKCPY,CMPY        COMPANY                                      
         MVC   CAHKMTHD,SVMETHNM   METHOD NUMBER                                
         XC    CAHKOFC,CAHKOFC                                                  
         GOTO1 HIGH                                                             
         CLC   BIGKEY(L'ACTKEY),KEYSAVE                                         
         BE    *+6                                                              
         DC    H'0'                HAS TO BE THERE                              
         MVC   AIO,AIO2                                                         
         GOTO1 GETREC                                                           
         MVC   AIO,AIO1                                                         
*                                                                               
         USING METELD,R6                                                        
         L     R6,AIO2                                                          
         MVI   ELCODE,METELQ       METHOD OF ALLOCATION ELEMENT                 
         BAS   RE,GETEL                                                         
*                                                                               
         MVC   PROMETH,METCODE                                                  
DK20     OI    PROMETHH+6,X'80'                                                 
*                                                                               
         NI    BITS,X'FF'-MADETAB                                               
*                                                                               
DKX      B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*                       DISPLAY RECORD                                *         
***********************************************************************         
*                                                                               
DR       DS    0H                                                               
         MVC   SVGRPOPT,GRPFILT                                                 
         XC    SVGRPFLT,SVGRPFLT                                                
         TM    BITS,KEYCHNGE       DID WE COME FROM VR                          
         BNO   DR02                                                             
*                                                                               
DR02     GOTO1 =A(CLRSCR),DMCB,RR=RELO    CLEAR SCREEN                          
*                                                                               
         TM    BITS,MADETAB        IS DISPLAY TABLE ALREADY MADE                
         BO    DR10                                                             
         GOTO1 =A(MKDISTAB),DMCB,RR=RELO  MAKE DISPLAY TABLE                    
*                                                                               
DR10     DS    0H                                                               
         L     R6,AIO                                                           
         MVC   DSPKEY,0(R6)       LAST KEY TO BE DISPLAYED                      
         EJECT                                                                  
*                                                                               
*---------------------------------------------------------------------          
*              OK  NOW DISPLAY PAGE FROM TABLE                                  
*---------------------------------------------------------------------          
*                                                                               
         LA    R4,DISTABLE                                                      
         USING DISTABD,R4                                                       
         LA    R2,PROLIN1H         FIRST DISPLAY LINE                           
         USING DSPLINED,R2                                                      
*                                  WHERE TO START DISPLAYING                    
         TM    BITS,REDISP                                                      
         BO    DR26                                                             
*                                                                               
         CLI   PFKEY,0             NEW PAGE FOR ENTER IF ACTION SELECT          
         BNE   DR20                                                             
         CLI   ACTEQU,ACTSEL                                                    
         BE    DR24                                                             
         B     DR28                                                             
*                                                                               
DR20     CLI   PFKEY,7             UP                                           
         BNE   DR22                                                             
         MVC   STDISP,PRVSTDSP                                                  
         LA    R1,DISTABLN         LENGTH OF EACH ENTRY                         
         MH    R1,=H'16'           16 LINES                                     
         LH    R0,PRVSTDSP                                                      
         SR    R0,R1                                                            
         CH    R0,=H'0'                                                         
         BNL   *+6                                                              
         SR    R0,R0               DEFAULT TO BEGINNING                         
         STH   R0,PRVSTDSP                                                      
         B     DR28                                                             
*                                                                               
DR22     CLI   PFKEY,8             DOWN                                         
         BNE   DR28                                                             
DR24     MVC   PRVSTDSP,STDISP     DISP OF LAST DISP'D ENTRY                    
         MVC   STDISP,LSTDISP                                                   
         B     DR28                                                             
*                                                                               
DR26     MVC   STDISP,=H'0'       DEFAULT TO BEGINNING                          
         MVC   PRVSTDSP,=H'0'    DEFAULT TO BEGINNING                           
         NI    BITS,X'FF'-REDISP                                                
*                                  WHERE TO START DISPLAYING                    
DR28     DS    0H                                                               
         LA    R0,DISTABLN         LENGTH OF ONE ENTRY                          
         MH    R0,TABCOUNT         NUMBER OF ENTRIES                            
         LH    R1,STDISP                                                        
         CR    R0,R1               MAKE SURE THERE'S AN ENTRY                   
         BH    DR29                                                             
         LA    R1,0                                                             
DR29     AR    R4,R1               ADD NEW START DISP TO TABLE ADDR             
*                                                                               
DR30NX   DS    0H                                                               
         LA    R1,DISTABLE                                                      
         LR    R0,R4               R4 POINTS TO ENTRY BEING DISPLAYED           
         SR    R0,R1                                                            
         STH   R0,LSTDISP          SAVE DISPLACEMENT INTO TABLE                 
*                                                                               
         OC    DISNUMB(DISTABLN),DISNUMB      ANY MORE ENTRIES?                 
         BNZ   DR32                                                             
         NI    GENSTAT2,X'FF'-RETEQSEL     GET NEXT SELECT                      
         SR    R0,R0               START FROM TOP NEXT ENTER                    
         STH   R0,LSTDISP                                                       
         B     DR100                                                            
*                                                                               
DR32     CLC   SVGRPFLT,DISGRP     SAME GROUP FILTER AS PREVIOUS                
         BE    DR33                                                             
         USING GRPTBD,R3                                                        
         L     R3,AGRPTAB                                                       
DR32C    CLC   GRPSET,DISGRP       MATCH ON GROUP SETTING?                      
         BE    DR32D                                                            
         LA    R3,GRPLNQ(R3)                                                    
         CLI   0(R3),X'00'         END OF TABLE?                                
         BNE   DR32C                                                            
         DC    H'0'                                                             
*                                                                               
DR32D    L     RF,APUDSLST                                                      
         ZICM  R1,GRPDNME,2        DISP TO GROUP FILTER NAME                    
         AR    RF,R1                                                            
         MVC   DSPGRPNM,0(RF)                                                   
         OI    DSPDESCH+6,X'80'    XMIT                                         
         OI    DSPSETH+6,X'20'     PROTECT THE SETTING                          
         MVC   SVGRPFLT,DISGRP     SAVE THE GROUP SYMBOL                        
         LA    R2,DSPLINLN(R2)     BUMP TO NEXT DISPLAY LINE                    
         LA    R1,PROPFKYH                                                      
         CR    R2,R1                                                            
         BNL   DR100               END OF PAGE                                  
         B     DR30NX                                                           
         DROP  R3                                                               
*                                                                               
DR33     MVC   DSPSHORT,DISSHORT   PROFILE SHORT NAME                           
         MVI   DSPHYPH,C'-'                                                     
         L     R0,APDSLIST                                                      
         SR    RF,RF                                                            
         ICM   RF,3,DISDESC                                                     
         AR    RF,R0                                                            
         MVC   DSPDESC,0(RF)       DISPLAY DECRIPTION                           
         OI    DSPDESCH+6,X'80'                                                 
*                                                                               
         L     R3,AFROMTAB         FORMAT FROM CODE                             
DR34     CLC   DISFROM,0(R3)                                                    
         BE    DR36                                                             
         LA    R3,5(R3)                                                         
         CLI   0(R3),0                                                          
         BNE   DR34                                                             
*                                                                               
DR36     MVC   DSPFROM,1(R3)       DISPLAY FROM CODE                            
         CLI   LANGCODE,3          GERMANY                                      
         BNE   *+10                                                             
         MVC   DSPFROM,3(R3)                                                    
         OI    DSPFROMH+6,X'80'                                                 
*                                                                               
         TM    OPTBIT,OPTACT       ACTIVITY OPTION ON?                          
         BNO   DR38                                                             
*                                                                               
         MVC   PROHEAD,SPACES                                                   
         MVC   PROHEAD+8(L'AC@LAST),AC@LAST                                     
         MVC   PROHEAD+20(L'AC@ACTY),AC@ACTY                                    
         OI    PROHEADH+6,X'80'                                                 
         MVC   DSPWHO,DISWHO                 WHO LAST CHANGED                   
         GOTO1 DATCON,DMCB,(1,DISDATE),(11,DSPDATE)                             
         OI    DSPVALH+6,X'80'                                                  
         B     DR40                                                             
*                                                                               
DR38     L     R0,APDSLIST                                                      
         SR    RF,RF                                                            
         ICM   RF,3,DISOPTS                                                     
         AR    RF,R0                                                            
         MVC   DSPVAL,0(RF)        DISPLAY VALID OPTIONS                        
         OI    DSPVALH+6,X'80'                                                  
         MVC   PROHEAD,SPACES                                                   
         MVC   PROHEAD(13),AC@VALOP                                             
         OI    PROHEADH+6,X'80'                                                 
*                                                                               
DR40     SR    RF,RF               BRANCH TO DISPLAY ROUTINE                    
         IC    RF,DISDRTN                                                       
         SLL   RF,2                                                             
         B     *(RF)                                                            
                                                                                
         B     DRTNMVC             ONLY ONE IN USE...                           
         B     DRTNMVC                                                          
         B     DRTNMVC                                                          
         B     DRTNMVC                                                          
         B     DRTNMVC                                                          
         B     DRTNMVC                                                          
         B     DRTNMVC                                                          
         B     DRTNMVC                                                          
         B     DRTNMVC                                                          
         B     DRTNMVC                                                          
*                                                                               
DRTNMVC  DS    0H                  JUST MVC                                     
         MVC   DSPSET,DISSET                                                    
         OI    DSPSETH+6,X'80'                                                  
         B     DR50                                                             
*                                                                               
DRTNEDIT DS    0H                  EDIT TO 4 DECIMAL PLACES                     
         CURED (P4,DISSET),(8,DSPSET),4                                         
         OI    DSPSETH+6,X'80'                                                  
*                                                                               
DR50     BAS   RE,PROTPROF         PROTECT PROFILE SETTING                      
         BNE   DR55                                                             
         OI    DSPSETH+6,X'20'                                                  
*                                                                               
DR55     LA    R2,DSPLINLN(R2)     NEXT LINE                                    
         LA    R1,PROPFKYH                                                      
         CR    R2,R1                                                            
         BNL   DR100               END OF PAGE                                  
         TM    DISFLAG,DIS2ND      DOES THIS PROFILE HAVE A 2ND LINE?           
         BZ    DR60                                                             
         TM    OPTBIT,OPTACT       IS THE ACTIVITY OPTION ON?                   
         BO    DR60                YES-DON'T SHOW 2ND LINE                      
         MVC   DSPSHORT,SPACES                                                  
         OI    DSPSETH+6,X'20'     PROTECT THE SETTING                          
         ICM   R1,15,DISVADR       ADDR OF PROFILE VALIDATION ENTRY             
         USING COPDD,R1                                                         
*                                                                               
         L     R6,APDSLIST                                                      
         ZICM  R3,COPDLIT2,2                                                    
         AR    R3,R6                                                            
         MVC   DSPVAL,0(R3)       JUST FILL IN 2ND LINE OF SETTINGS             
*        OC    DSPVAL,SPACES                                                    
         OI    DSPVALH+6,X'80'                                                  
DR57     LA    R2,DSPLINLN(R2)     BUMP TO NEXT DISPLAY LINE                    
         LA    R1,PROPFKYH                                                      
         CR    R2,R1                                                            
         BNL   DR100               END OF PAGE                                  
*                                                                               
DR60     LA    R4,DISTABLN(R4)     NEXT TABLE ENTRY                             
         B     DR30NX                                                           
*                                                                               
DR100    DS    0H                                                               
         TM    BITS,KEYCHNGE       DID WE COME FROM VR                          
         BNO   DRX                                                              
         NI    BITS,X'FF'-KEYCHNGE                                              
         MVC   SVOPTBIT,OPTBIT                                                  
         LA    R2,PROLIN1H                                                      
         LA    R2,DSPSETH                                                       
         B     ENTCHNG             TELL TO NOW ENTER CHANGES                    
*                                                                               
DRX      DS    0H                                                               
         MVC   SVOPTBIT,OPTBIT                                                  
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
***********************************************************************         
*                       ON-SCREEN LIST                                *         
***********************************************************************         
*                                                                               
LR       DS    0H                                                               
*                                                                               
         XC    LSTDISP,LSTDISP                                                  
         MVC   SVMETHNM,SPACES                                                  
         MVC   SVMETHCD,SPACES                                                  
*                                                                               
         LA    R4,LISTAR           LINE DSECT                                   
         USING LSTLINED,R4                                                      
*                                                                               
         OC    BIGKEY,BIGKEY       FIRST TIME THROUGH?                          
         BNZ   LRHI                                                             
*                                                                               
         XC    BIGKEY,BIGKEY                                                    
         MVC   BIGKEY(3),SAVEKEY   KEY FROM VALKEY - UP TO COMPANY              
*                                                                               
LRHI     DS    0H                                                               
         GOTO1 HIGH                                                             
         CLI   DMCB+8,0                                                         
         BE    LR10                                                             
         DC    H'0'                                                             
*                                                                               
LRSEQ    DS    0H                                                               
         GOTO1 SEQ                                                              
         CLI   DMCB+8,0                                                         
         BE    LR10                                                             
         DC    H'0'                                                             
*                                                                               
LR10     GOTO1 GETREC                                                           
         CLC   BIGKEY(3),SAVEKEY   SAME RECORD TYPE AND COMPANY?                
         BNE   LRX                 NO MORE TO LIST                              
*                                                                               
         L     R6,AIO              CHECK FOR FILTERS                            
         USING CAPRECD,R6                                                       
         TM    LEVBIT,LEVMETH      ANY METHOD SPECIFIED                         
         BNO   LR20                NO                                           
         CLC   CAPKMTHD,METHNUM    MATCH                                        
         BNE   LRSEQ                                                            
*                                                                               
LR20     TM    LEVBIT,LEVOFF       ANY OFFICE SPECIFIED                         
         BNO   LR22                NO                                           
         CLC   CAPKOFC,OFFICE      MATCH                                        
         BNE   LRSEQ                                                            
*                                                                               
LR22     TM    LEVBIT,LEVDPT       ANY DEPT SPECIFIED                           
         BNO   LR24                NO                                           
         CLC   CAPKDPT,DEPT        MATCH                                        
         BNE   LRSEQ                                                            
*                                                                               
LR24     TM    LEVBIT,LEVSDPT      ANY SUB DEPT SPECIFIED                       
         BNO   LR30                NO                                           
         CLC   CAPKSDT,SUBDPT      MATCH                                        
         BNE   LRSEQ                                                            
*                                                                               
LR30     DS    0H                                                               
         OC    PROFILT,PROFILT     ANY PROFILE FILTER?                          
         BZ    LR40                                                             
*                                                                               
         L     R3,APROFTAB                                                      
         AH    R3,PROFDISP         DISP INTO TABLE OF ENTRY                     
         GOTO1 =A(SHOWPROF),DMCB,RR=RELO  DO WE SHOW THIS PROFILE               
         BNE   LRSEQ                                                            
         GOTO1 =A(PROFSET),DMCB,RR=RELO AND IS THE PROFILE SET                  
         BNE   LRSEQ                                                            
*                                                                               
LR40     MVC   LISTAR,SPACES       CLEAR LIST LINE                              
*                                                                               
         CLC   SVMETHNM,CAPKMTHD   SAME AS LAST LINE                            
         BNE   LR42                                                             
         MVC   LSTMETH,SVMETHCD                                                 
         B     LR52                THEN SKIP READ                               
*                                                                               
LR42     MVC   KEY2,BIGKEY         SAVE KEY TO RESTORE SEQUENCE                 
         MVC   SVMETHNM,CAPKMTHD   SAVE NUMBER                                  
         CLC   SVMETHNM,SPACES     ANY METHOD                                   
         BE    LR52                                                             
*                                                                               
         LA    R6,BIGKEY           GET METHOD CODE TO DISPLAY                   
         XC    BIGKEY,BIGKEY                                                    
         USING CAHRECD,R6                                                       
         MVC   CAHKEY,SPACES                                                    
         MVI   CAHKTYP,CAHKTYPQ    ALLOCATION METHOD RECORD TYPE BY NUM         
         MVI   CAHKSUB,CAHKSUBQ    METHOD BY CODE SUB TYPE                      
         MVC   CAHKCPY,CMPY        COMPANY                                      
         MVC   CAHKMTHD,SVMETHNM   METHOD NUMBER                                
         XC    CAHKOFC,CAHKOFC                                                  
         GOTO1 HIGH                                                             
         CLC   BIGKEY(L'ACTKEY),KEYSAVE                                         
         BE    *+6                                                              
         DC    H'0'                HAS TO BE THERE                              
         MVC   AIO,AIO2                                                         
         GOTO1 GETREC                                                           
         MVC   AIO,AIO1                                                         
*                                                                               
         USING METELD,R6                                                        
         L     R6,AIO2                                                          
         MVI   ELCODE,METELQ       METHOD OF ALLOCATION ELEMENT                 
         BAS   RE,GETEL                                                         
*                                                                               
         MVC   LSTMETH,METCODE                                                  
         MVC   SVMETHCD,METCODE    SAVE CODE                                    
         XC    BIGKEY,BIGKEY                                                    
         MVC   BIGKEY(L'ACTKEY),KEY2                                            
         GOTO1 HIGH                RESTORE SEQUENCE                             
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         USING CAPRECD,R6                                                       
*                                                                               
LR52     MVC   LSTOFF,CAPKOFC                                                   
         MVC   LSTDEPT,CAPKDPT                                                  
         MVC   LSTSDPT,CAPKSDT                                                  
         MVC   LSTPER,CAPKPER                                                   
*                                                                               
         GOTO1 LISTMON             SEND RECORD TO SCREEN                        
         B     LRSEQ               NEXT RECORD                                  
*                                                                               
LRX      B     XIT                                                              
         DROP  R4,R6                                                            
         EJECT                                                                  
***********************************************************************         
*    CHECKS IF PROFILE SHOULD BE PROTECTED                                      
*    GETS LEVEL FROM KEY IN AIO                                                 
***********************************************************************         
*                                                                               
         USING DISTABD,R4          TABLE ENTRY                                  
PROTPROF NTR1                                                                   
         L     R6,AIO                                                           
         USING CAPRECD,R6                                                       
*                                                                               
*&&UK                                                                           
         CLI   DISNUMB,COMCT#      BRO PROFILE                                  
         BNE   PP04                                                             
         TM    LEVBIT,LEVMETH+LEVGRP+LEVOFF+LEVDPT+LEVSDPT+LEVPER               
         BNZ   PP04                OK IF NOT HIGHEST LEVEL                      
         CLI   TWAOFFC,C'*'        IF NOT DDS PROTECT THIS FIELD                
         BNE   XYES                                                             
*&&                                                                             
*                                                                               
PP04     CLC   CAPKMTHD,SPACES     FOR NO METHOD                                
         BNE   PP08                                                             
         TM    DISSHOW,COLNOMTH    VALID FOR NO METHOD                          
         BNO   XYES                                                             
         B     PP10                                                             
*                                                                               
PP08     TM    DISSHOW,COLNOMTH    VALID ONLY FOR NO METHOD                     
         BO    XYES                                                             
*                                                                               
         CLC   CAPKOFC,SPACES      ANY OFFICE                                   
         BNE   PP10                                                             
         TM    DISSHOW,COLMETH     SHOW AT METHOD LEVEL?                        
         BNO   XYES                                                             
         TM    DISPROT,COLMETH      NO, THEN MUST BE AT METHOD LEVEL            
         BNO   XNO                                                              
         GOTO1 =A(PROTEX),DMCB,RR=RELO                                          
         BNE   XNO                 MISC CIRCUMSTANCES TO PROTECT                
         BO    XYES                                                             
*                                                                               
PP10     CLC   CAPKDPT,SPACES      ANY DEPT                                     
         BNE   PP20                                                             
         TM    DISPROT,COLOFFC     NO, THEN OFFICE LEVEL                        
         BO    XYES                                                             
         GOTO1 =A(PROTEX),DMCB,RR=RELO                                          
         BNE   XNO                 MISC CIRCUMSTANCES TO PROTECT                
         B     XYES                                                             
*                                                                               
PP20     CLC   CAPKSDT,SPACES      ANY SUB DEPT                                 
         BNE   PP30                                                             
         TM    DISPROT,COLDPT      NO, THEN DEPT LEVEL                          
         B     PP50                                                             
*        BNO   XNO                                                              
*        BO    XYES                                                             
*                                                                               
PP30     CLC   CAPKPER,SPACES      ANY PERSON                                   
         BNE   PP40                                                             
         TM    DISPROT,COLSDT      NO, THEN SUB DEPT LEVEL                      
         B     PP50                                                             
*        BNO   XNO                                                              
*        BO    XYES                                                             
*                                                                               
PP40     TM    DISPROT,COLPER      NO, THEN PERSON LEVEL                        
         B     PP50                                                             
*        BNO   XNO                                                              
*        BO    XYES                                                             
*                                                                               
PP50     BNO   XNO                                                              
         BO    XYES                                                             
         DROP  R4,R6                                                            
         EJECT                                                                  
***********************************************************************         
*                        RANDOM STUFF                                 *         
***********************************************************************         
*                                  GENERAL MESSAGES                             
ENTCHNG  DS    0H                                                               
         MVI   GERROR1,X'04'       REC DISP - ENTER CHANGES                     
         MVI   GMSGTYPE,C'I'       INFO                                         
         B     ERRX                                                             
ERRMISS  DS    0H                                                               
         MVI   GERROR1,MISSING                                                  
         B     ERRX                                                             
ERRPLS   MVI   GERROR1,2                                                        
         MVI   GMSGTYPE,C'I'                                                    
         B     ERRX                                                             
ERRINV   DS    0H                                                               
         MVI   GERROR1,INVALID                                                  
         B     ERRX                                                             
*                                                                               
ERRX     DS    0H                                                               
         MVI   GMSYS,X'FF'         GENERAL MESSAGE SYSTEM                       
         GOTO1 MYERR                                                            
*                                                                               
EDUP15   LHI   RF,ACEDUP15                                                      
         B     ACCERRX                                                          
EINVACC  LHI   RF,ACEACCT                                                       
         B     ACCERRX                                                          
EDEFDISP LHI   RF,ACEDEFOP                                                      
         B     ACCERRX                                                          
EINVNUM  LHI   RF,ACEINVNM                                                      
         B     ACCERRX                                                          
ETOOSHRT LHI   RF,ACELSHO                                                       
         B     ACCERRX                                                          
ETOOLONG LHI   RF,ACELLONG                                                      
         B     ACCERRX                                                          
EMISHIGH LHI   RF,ACEHIGH                                                       
         B     ACCERRX                                                          
*INVYR   LHI   RF,ACEINVYR                                                      
*        B     ACCERRX                                                          
*INVDATE LHI   RF,ACEIVDTE                                                      
*        B     ACCERRX                                                          
EINVMET  LHI   RF,ACEIVMET                                                      
         B     ACCERRX                                                          
ENOMETH  LHI   RF,ACENOMET                                                      
         B     ACCERRX                                                          
EINVPRO  LHI   RF,ACEIVPRO                                                      
         B     ACCERRX                                                          
ENODEL   LHI   RF,ACENODOL                                                      
         B     ACCERRX                                                          
EINVSET  LHI   RF,ACEINVST                                                      
         B     ACCERRX                                                          
EMISPID  LHI   RF,ACEPMFPC                                                      
EGENER   MVI   GLTXT,L'PCODE                                                    
         LA    R1,PCODE                                                         
         STCM  R1,7,GATXT                                                       
         LA    R2,PROMETHH                                                      
         B     ACCERRX                                                          
EINVLOC  LHI   RF,ACEPLOC                                                       
         B     EGENER                                                           
ETIMEXS  LHI   RF,ACETIMEX                                                      
         B     EGENER                                                           
EINVOFL  LHI   RF,ACEIVOFL                                                      
         B     ACCERRX                                                          
ENOOFL   LHI   RF,ACENOOFL                                                      
         B     ACCERRX                                                          
EINVOPT  LHI   RF,ACEINVOP                                                      
         B     ACCERRX                                                          
*NOCAL   LHI   RF,ACENOCAL                                                      
*        B     ACCERRX                                                          
EINVSTLV LHI   RF,ACESETLV                                                      
         B     ACCERRX                                                          
EINVOF   LHI   RF,ACEIVOF                                                       
         B     ACCERRX                                                          
EINVOFFL LHI   RF,ACEOFFL                                                       
         B     ACCERRX                                                          
EINVIFL  LHI   RF,ACEINV                                                        
         B     ACCERRX                                                          
*                                                                               
ACCERRX  DS    0H                                                               
         STCM  RF,3,GERROR                                                      
         MVI   GMSYS,6             ACC MESSAGE SYSTEM                           
         GOTO1 MYERR                                                            
*                                                                               
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
         EJECT                                                                  
***********************************************************************         
*                           LTORG                                     *         
***********************************************************************         
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
*        MAINT PFKEY TABLE                                                      
**********************************************************************          
*                                                                               
PFTABLE  DS    0C                                                               
*                                                                               
* RETURN TO LIST                                                                
*                                                                               
         DC    AL1(PPF09X-*,09,PFTCPROG,0,0)                                    
         DC    CL3' '                                                           
         DCDD  AC#PRFL,8                                                        
         DCDD  AC#LIST,8                                                        
PPF09X   EQU   *                                                                
*                                                                               
* PERSON LIST                                                                   
*                                                                               
         DC    AL1(PPF01X-*,01,PFTCPROG,(PPF01X-PPF01)/KEYLNQ,0)                
         DC    CL3' '                                                           
         DCDD  AC#CPRSN,8                                                       
         DCDD  AC#LIST,8                                                        
PPF01    DC    AL1(KEYTYTWA,L'PROPER-1),AL2(PROPER-T61DFFD)                     
PPF01X   EQU   *                                                                
*                                                                               
* METHOD LIST                                                                   
*                                                                               
         DC    AL1(PPF10X-*,10,PFTCPROG,0,0)                                    
         DC    CL3' '                                                           
         DCDD  AC#METH,8                                                        
         DCDD  AC#LIST,8                                                        
PPF10X   EQU   *                                                                
*                                                                               
* RETURN TO CALLER                                                              
*                                                                               
         DC    AL1(PPF12X-*,12,PFTRPROG,0,0)                                    
         DC    CL3' ',CL8' ',CL8' '                                             
PPF12X   EQU   *                                                                
*                                                                               
         DC    X'FF'                                                            
         EJECT                                                                  
**********************************************************************          
*        LIST PFKEY TABLE                                                       
**********************************************************************          
*                                                                               
LPFTABLE DS    0C                                                               
*                                                                               
* PROFILE LAST (BACK PAGE)                                                      
*                                                                               
         DC    AL1(LPF07X-*,07,PFTLIST,0,0)                                     
         DC    CL3'   '                                                         
         DCDD  AC#PRFL,8                                                        
         DCDD  AC#LAST,8                                                        
LPF07X   EQU   *                                                                
*                                                                               
* PROFILE NEXT (NEXT PAGE)                                                      
*                                                                               
         DC    AL1(LPF08X-*,08,PFTLIST,0,0)                                     
         DC    CL3'   '                                                         
         DCDD  AC#PRFL,8                                                        
         DCDD  AC#NXT,8                                                         
LPF08X   EQU   *                                                                
*                                                                               
* TO DISPLAY                                                                    
*                                                                               
         DC    AL1(LPF09X-*,09,PFTCPROG,(LPF09X-LPF09)/KEYLNQ,0)                
         DCDD  AC#DSP,3                                                         
         DCDD  AC#PRFL,8                                                        
         DCDD  AC#DSP,8                                                         
LPF09    DC    AL1(KEYTYCUR,L'LSTMETH-1),AL2(LSTMETH-LSTSTRT)                   
         DC    AL1(KEYTYCUR,L'LSTOFF-1),AL2(LSTOFF-LSTSTRT)                     
         DC    AL1(KEYTYCUR,L'LSTDEPT-1),AL2(LSTDEPT-LSTSTRT)                   
         DC    AL1(KEYTYCUR,L'LSTSDPT-1),AL2(LSTSDPT-LSTSTRT)                   
         DC    AL1(KEYTYCUR,L'LSTPER-1),AL2(LSTPER-LSTSTRT)                     
LPF09X   EQU   *                                                                
*                                                                               
* PERSON LIST                                                                   
*                                                                               
         DC    AL1(LPF01X-*,01,PFTCPROG,(LPF01X-LPF01)/KEYLNQ,0)                
         DCDD  AC#CPRSN,3                                                       
         DCDD  AC#CPRSN,8                                                       
         DCDD  AC#LIST,8                                                        
LPF01    DC    AL1(KEYTYCUR,L'LSTPER-1),AL2(LSTPER-LSTSTRT)                     
LPF01X   EQU   *                                                                
*                                                                               
* METHOD LIST                                                                   
*                                                                               
         DC    AL1(LPF10X-*,10,PFTCPROG,0,0)                                    
         DCDD  AC#METH,3                                                        
         DCDD  AC#METH,8                                                        
         DCDD  AC#LIST,8                                                        
LPF10X   EQU   *                                                                
*                                                                               
* RETURN TO CALLER                                                              
*                                                                               
         DC    AL1(LPF12X-*,12,PFTRPROG,0,0)                                    
         DC    CL3' ',CL8' ',CL8' '                                             
LPF12X   EQU   *                                                                
*                                                                               
         DC    X'FF'                                                            
         EJECT                                                                  
***********************************************************************         
*                   TABLE DSECTS AND STUFF                            *         
***********************************************************************         
*                                                                               
*------- FROM EQUIVALENTS TABLE                                                 
*                                                                               
         DS    0F                                                               
FROMTAB  DC    C'F',C'DF',C'VO'                                                 
         DC    C'A',C'AG',C'AG'                                                 
         DC    C'M',C'MT',C'ME'                                                 
         DC    C'O',C'OF',C'FA'                                                 
         DC    C'D',C'DP',C'AB'                                                 
         DC    C'S',C'SD',C'KS'                                                 
         DC    C'P',C'PE',C'MA'                                                 
         DC    X'00'                                                            
*                                                                               
*------- OFFICE LIST VALIDATION TABLE                                           
*        BYTE1 = PROFILE NUM REQRING VALIDATION                                 
*        BYTE2 = PROFILE NUM THAT MUST EXIST                                    
*                                                                               
OFFLTAB  DC    AL1(COIALLD#,COIOFFL#)                                           
         DC    AL1(COIALLO#,COIOFFL#)                                           
         DC    AL1(COIALLC#,COIOFFL#)                                           
         DC    AL1(COVALLD#,COIOFFL#)                                           
         DC    AL1(COVALLO#,COIOFFL#)                                           
         DC    AL1(COISALL#,COISOFL#)                                           
         DC    X'00'                                                            
*                                                                               
*------- COST/INCOME VALIDATION - IF INCOME THEN NO DEPT OR PERSON              
*        BYTE1 = C/I PROFILE                                                    
*        BYTE2 = CORRESPONDING DEPT/OFF/AGY PROFILE                             
*        BYTE3 = CORRESPONDING PERSON/CLIENT PROFILE                            
*                                                                               
CITABLE  DC    AL1(CONBAB#,CONBAL#,CONBOFF#)                                    
         DC    AL1(COPBAB#,COPBAL#,COPBOFF#)                                    
         DC    AL1(COHOAB#,COHOAL#,COHOOFF#)                                    
         DC    X'00'                                                            
*                                                                               
*------- COUPLED PROFILE TABLE -------------------------                        
*        BYTE1 = CONTROLLING PROFILE                                            
*        BYTE2 = SETTING OF CONTROLLING PROFILE                                 
*        BYTE3 = DEPENDANT PROFILE                                              
*        BYTE4 = SETTING TO SET TO                                              
*                                                                               
COUPTAB  DS    0H                                                               
         DC    AL1(CONBAB#,CODCOST),AL1(CONBOFF#),X'FF'                         
         DC    AL1(CONBAB#,COINCOME),AL1(CONBOFF#,COCLIENT)                     
         DC    AL1(COPBAB#,CODCOST),AL1(COPBOFF#),X'FF'                         
         DC    AL1(COPBAB#,COINCOME),AL1(COPBOFF#,COCLIENT)                     
         DC    AL1(COHOAB#,CODCOST),AL1(COHOOFF#),X'FF'                         
         DC    AL1(COHOAB#,COINCOME),AL1(COHOOFF#,COCLIENT)                     
         DC    AL1(CODBASE#,COYTD),AL1(COVBASE#),X'FF'                          
         DC    X'00'                                                            
*                                                                               
*------- DISPLAY ROUTINE TABLE                                                  
*                                                                               
*        DS    0F                                                               
*DRTNTAB  DC    AL1(01,0,0,0),A(DRTNMVC)                                        
*        DC    AL1(02,0,0,0),A(DRTNMVC)                                         
*        DC    AL1(03,0,0,0),A(DRTNMVC)                                         
*        DC    AL1(04,0,0,0),A(DRTNMVC)                                         
*        DC    AL1(05,0,0,0),A(DRTNMVC)                                         
*        DC    AL1(06,0,0,0),A(DRTNMVC)                                         
*        DC    X'00'                                                            
*                                                                               
*------- VALIDATION ROUTINE TABLE                                               
*                                                                               
*        DS    0F                                                               
*VRTNTAB  DC    AL1(01,0,0,0),A(VRTNTBL)                                        
*        DC    AL1(02,0,0,0),A(VRTNNUM)                                         
*        DC    AL1(03,0,0,0),A(VRTNLED)                                         
*        DC    AL1(04,0,0,0),A(VRTNOFF)                                         
*        DC    AL1(05,0,0,0),A(VRTNCON)                                         
*        DC    AL1(06,0,0,0),A(VRTMUPL)                                         
*        DC    X'00'                                                            
*                                                                               
*------- VALID OPTIONS TABLE                                                    
*                                                                               
OPTSTAB  DS    0C                                                               
         DCDD  AC#ACTY,8                                                        
         DC    AL1(OPTACT),AL1(0)                                               
         DCDD  AC#SALL,8                                                        
         DC    AL1(OPTSHOW),AL1(0)                                              
         DCDD  AC#RUN,8                                                         
         DC    AL1(OPTGRP),AL1(CORUNF)                                          
         DCDD  AC#DIR,8                                                         
         DC    AL1(OPTGRP),AL1(CODIRECT)                                        
         DCDD  AC#IDR,8                                                         
         DC    AL1(OPTGRP),AL1(COINDIR)                                         
         DCDD  AC#COVER,8                                                       
         DC    AL1(OPTGRP),AL1(COOVHED)                                         
         DCDD  AC#SLRY,8                                                        
         DC    AL1(OPTGRP),AL1(COINDTYP)                                        
         DCDD  AC#PAYRL,8                                                       
         DC    AL1(OPTGRP),AL1(COINDTYP)                                        
         DCDD  AC#NEWBZ,8                                                       
         DC    AL1(OPTGRP),AL1(CONEWBIZ)                                        
         DCDD  AC#PBONO,8                                                       
         DC    AL1(OPTGRP),AL1(COPROBON)                                        
         DCDD  AC#HOUSE,8                                                       
         DC    AL1(OPTGRP),AL1(COHOUSEC)                                        
         DCDD  AC#TIME,8                                                        
         DC    AL1(OPTGRP),AL1(COTMESHT)                                        
         DCDD  AC#BRTSH,8                                                       
         DC    AL1(OPTGRP),AL1(COMCSTSP)                                        
         DCDD  AC#BRTSC,8                                                       
         DC    AL1(OPTGRP),AL1(COMCSTSC)                                        
         DCDD  AC#BREXP,8                                                       
         DC    AL1(OPTGRP),AL1(COMCSEXP)                                        
         DCDD  AC#DEF,8                                                         
         DC    AL1(OPTDFLT),AL1(0)                                              
         DC    X'00'                                                            
*                                                                               
*------- GROUP FILTER NAMES TABLE                                               
*                                                                               
GRPTAB   DS    0C                                                               
         DC    AL1(CORUNF),AL2(AC@CRUN-PUDSLIST)                                
         DC    AL1(CODIRECT),AL2(AC@CDIR-PUDSLIST)                              
         DC    AL1(COINDIR),AL2(AC@CINDR-PUDSLIST)                              
         DC    AL1(COOVHED),AL2(AC@COVHD-PUDSLIST)                              
         DC    AL1(COINDTYP),AL2(AC@CPAY-PUDSLIST)                              
         DC    AL1(CONEWBIZ),AL2(AC@CNBIZ-PUDSLIST)                             
         DC    AL1(COPROBON),AL2(AC@CPBON-PUDSLIST)                             
         DC    AL1(COHOUSEC),AL2(AC@CHOUS-PUDSLIST)                             
         DC    AL1(COTMESHT),AL2(AC@CTIME-PUDSLIST)                             
         DC    AL1(COMCSTSP),AL2(AC@CMCST-PUDSLIST)                             
         DC    AL1(COMCSTSC),AL2(AC@CMCSX-PUDSLIST)                             
         DC    AL1(COMCSEXP),AL2(AC@CMCXP-PUDSLIST)                             
         DC    X'00'                                                            
*                                                                               
         EJECT                                                                  
       ++INCLUDE ACCAPROTAB                                                     
         EJECT                                                                  
       ++INCLUDE ACCAPDDICT                                                     
         EJECT                                                                  
***********************************************************************         
* UPDATE PERSON RECORD WITH THIRD PARTY SETTING                       *         
***********************************************************************         
         SPACE 1                                                                
UPDPER   NTR1  BASE=*,LABEL=*                                                   
         MVC   SVKEY1,BIGKEY                                                    
         MVC   PCODE,SPACES                                                     
         MVC   AIO,AIO2                                                         
         MVI   CNINES,C'9'     FILL CNINES WITH 9'S                             
         MVC   CNINES+1(L'CNINES-1),CNINES                                      
*                                                                               
         MVI   LDGFLG,0                                                         
         XC    WAREA1(3),WAREA1                                                 
         GOTO1 DATCON,DMCB,(5,0),(1,WAREA1)                                     
*                                                                               
         LA    R0,3                                                             
         XR    R1,R1                                                            
         XR    RE,RE                                                            
         LA    RF,LEVELLN      RF CONTAINS LEVEL LENGTHS                        
UPDPER02 IC    R1,0(,RF)                                                        
         AR    RE,R1                                                            
         LA    RF,1(RF)                                                         
         BCT   R0,UPDPER02                                                      
         STC   RE,LENSTOR      LENSTOR=L'OFFICE+L'DEPT+L'SUBDEPT                
*                                                                               
         LA    R3,BIGKEY                                                        
         USING ACTRECD,R3      READ ACCOUNT RECORD                              
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,CMPY                                                     
         MVC   ACTKUNT(2),=C'1R'                                                
         MVC   ACTKACT,ACCNT                                                    
         MVC   SVKEY3,BIGKEY                                                    
         CLC   PERSON,SPACES   ANY PERSON CODE?                                 
         BNH   UPDPER03                                                         
         GOTO1 READ            READ 1R ACCOUNT TO CHECK IF CURRENT              
         B     UPDPER20        LOCATION                                         
*                                                                               
UPDPER03 LA    RE,L'ACTKACT                                                     
         LA    R6,ACTKACT+L'ACTKACT-1                                           
UPDPER04 CLI   0(R6),C' '                                                       
         BH    UPDPER06                                                         
         AHI   R6,-1                                                            
         BCT   RE,UPDPER04                                                      
         MVI   LDGFLG,1        CHECK PIDS OVER WHOLE COMPANY 1R LEDGER          
         GOTO1 HIGH                                                             
         B     UPDPER11                                                         
*                                                                               
UPDPER06 LA    RF,ACTRECD                                                       
         SR    R6,RF                                                            
         STC   R6,MYCHAR                                                        
         GOTO1 HIGH                                                             
         B     UPDPER11                                                         
*                                                                               
UPDPER08 MVC   BIGKEY,SVKEY4   RESTORE KEY                                      
         GOTO1 READ                                                             
UPDPER10 GOTO1 SEQ                                                              
*                                                                               
UPDPER11 CLI   LDGFLG,1                                                         
         BNE   UPDPER12                                                         
         CLC   SVKEY3(ACTKACT-ACTRECD),BIGKEY SAME COMPANY U&L?                 
         BE    UPDPER14                                                         
         B     UPDPERX         NO (FURTHER) PIDS TO CHECK                       
UPDPER12 LLC   R1,MYCHAR                                                        
         EXCLC R1,SVKEY3,BIGKEY CHECK WE ARE NOT AT A HIGHER LEVEL              
         BNE   UPDPERX                                                          
*                                                                               
UPDPER14 LA    RF,ACTKACT      CHECK FOR A PERSON CODE                          
         ZIC   R1,LENSTOR                                                       
         AR    RF,R1                                                            
         ZIC   RE,LEVELLN+3                                                     
         BCTR  RE,0                                                             
         EXCLC RE,0(RF),SPACES                                                  
         BE    UPDPER10        READ NEXT RECORD                                 
                                                                                
         LA    RF,ACTKACT      SKIP OVERHEAD ACCOUNT RECORDS                    
         ZIC   RE,LEVELLN      (SEE ACCAP16, VALOVER ROUTINE)                   
         BCTR  RE,0                                                             
         EXCLC RE,ACTKACT,CNINES                                                
         BE    UPDPER10                                                         
         LA    RF,1(RE,RF)                                                      
*                                                                               
         ZIC   RE,LEVELLN+1                                                     
         BCTR  RE,0                                                             
         EXCLC RE,0(RF),CNINES                                                  
         BE    UPDPER10                                                         
         LA    RF,1(RE,RF)                                                      
*                                                                               
         ZIC   RE,LEVELLN+2                                                     
         AR    RF,RE                                                            
*                                                                               
         ZIC   RE,LEVELLN+3                                                     
         CH    RE,=H'3'        ONLY NEED 3 NINES IN LOW LEVEL TO                
         BL    *+8             BE AN OVERHEAD ACCOUNT                           
         LA    RE,3                                                             
         BCTR  RE,0                                                             
         EXCLC RE,0(RF),CNINES                                                  
         BE    UPDPER10                                                         
*                                                                               
*   ** RETRIEVE PERSON CODE **                                                  
*                                                                               
         MVC   PCODE,SPACES                                                     
         LR    R1,RF           R1=A(PERSON CODE)                                
         SR    RE,RE                                                            
         IC    RE,LEVELLN+3    LENGTH OF PERSON LEVEL                           
         LTR   RE,RE                                                            
         BNZ   *+6                                                              
         DC    H'0'            L'PERSON LEVEL SHOULD BE GREATER THAN 0          
         AHI   RE,-1           RE=LENGTH OF PERSON LEVEL-1                      
         AR    RF,RE           RF=A(PERSON_CODE+L'PERSON_CODE-1)                
UPDPER16 CLI   0(RF),C' '                                                       
         BH    UPDPER18                                                         
         AHI   RF,-1                                                            
         BCT   RE,UPDPER16                                                      
UPDPER18 EXMVC RE,PCODE,0(R1)  SAVE PERSON CODE                                 
*                                                                               
         USING PERRECD,R6                                                       
UPDPER20 MVC   SVKEY4,BIGKEY                                                    
         LA    R6,BIGKEY                                                        
         MVC   PERKEY,SPACES                                                    
         MVI   PERKTYP,PERKTYPQ                                                 
         MVC   PERKCPY,CMPY                                                     
         MVC   PERKCODE,PCODE                                                   
         CLC   PERSON,SPACES                                                    
         BE    UPDPER22                                                         
         MVC   PERKCODE,PERSON                                                  
         MVC   PCODE,PERSON                                                     
*                                                                               
UPDPER22 GOTO1 READ                                                             
         BE    *+6                                                              
         DC    H'0'            YES - DIDN'T FIND IT                             
         GOTO1 GETREC                                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R6,AIO                                                           
*                                                                               
         XR    R0,R0                                                            
         XR    R1,R1                                                            
         LA    R2,PERRFST      GET LOCELD                                       
         USING LOCELD,R2                                                        
UPDPER24 CLI   LOCEL,0                                                          
         BE    UPDPER08                                                         
         CLI   LOCEL,LOCELQ                                                     
         BE    UPDPER28                                                         
UPDPER26 LLC   R0,LOCLN                                                         
         AR    R2,R0                                                            
         B     UPDPER24                                                         
*                                                                               
UPDPER28 OC    LOCEND,LOCEND                                                    
         BZ    *+14                                                             
         CLC   LOCEND,WAREA1   FIND A LOCATION THAT IS VALID                    
         BNH   UPDPER26        END DATE MUST BE AFTER TODAY                     
         CLI   LOCSTAT,LOCSACT                                                  
         BE    *+12                                                             
         CLI   LOCSTAT,LOCSLOA                                                  
         BNE   UPDPER08                                                         
*                              CHECK MATCH ON LOCATION                          
         LA    RF,SVKEY4+ACTKACT-ACTRECD                                        
         LLC   R1,LEVELLN      BIGKEY OVERWRITTEN USE SVKEY4                    
         BCTR  R1,0                                                             
         EXCLC R1,0(RF),LOCOFF OFFICE                                           
         BNE   UPDPER26                                                         
         AHI   R1,1                                                             
         AR    RF,R1                                                            
         LLC   R1,LEVELLN+1                                                     
         BCTR  R1,0                                                             
         EXCLC R1,0(RF),LOCDEPT DEPARTMENT                                      
         BNE   UPDPER26                                                         
         AHI   R1,1                                                             
         AR    RF,R1                                                            
         LLC   R1,LEVELLN+2                                                     
         BCTR  R1,0                                                             
         EXCLC R1,0(RF),LOCSUB  SUBDEPARTMENT                                   
         BNE   UPDPER26                                                         
         DROP  R2                                                               
*                                                                               
         XR    R1,R1                                                            
         LA    RF,PERRFST      GET EMPELD                                       
         USING EMPELD,RF                                                        
UPDPER30 CLI   EMPEL,0                                                          
         BE    UPDPER36                                                         
         CLI   EMPEL,EMPELQ    EMPLOYEE HISTORY ELEMENT? X'56'                  
         BE    UPDPER34                                                         
UPDPER32 IC    R0,EMPLN                                                         
         AR    RF,R0                                                            
         B     UPDPER30                                                         
*                                                                               
UPDPER34 OI    EMPSTAT,EMPSTHRD                                                 
         GOTO1 PUTREC                                                           
UPDPER36 CLC   PERSON,SPACES   CHECKING ONE PERSON ONLY?                        
         BNH   UPDPER08                                                         
         DROP  RF                                                               
*                                                                               
UPDPERX  MVC   AIO,AIO1        RE-ESTABLISH AIO                                 
         MVC   BIGKEY,SVKEY1   RE-ESTABLISH BIGKEY                              
         XIT1                                                                   
         DROP  R1                                                               
         EJECT                                                                  
***********************************************************************         
*                              SETUP                                            
***********************************************************************         
*                                                                               
SETUP    NTR1  BASE=*,LABEL=*                                                   
         NI    GENSTAT3,X'FF'-OKVALSEL     CHANGE FROM LIST INVALID             
*                                                                               
         OI    GENSTAT2,RETEQSEL   RETURN SAME SELECTION (FOR PAGING)           
*                                                                               
         BASR  R1,0                                                             
         AHI   R1,PUDCLIST-*                                                    
         ST    R1,APUDCLST                                                      
         BASR  R2,0                                                             
         AHI   R2,PDCLIST-*                                                     
         ST    R2,APDCLIST                                                      
         BASR  R1,0                                                             
         AHI   R1,PUDSLIST-*                                                    
         ST    R1,APUDSLST                                                      
         BASR  R3,0                                                             
         AHI   R3,PDSLIST-*                                                     
         ST    R3,APDSLIST                                                      
*                                                                               
         GOTO1 DICTATE,DMCB,C'LL  ',(R2),(R3)                                   
         L     R2,APUDCLST                                                      
         L     R3,APUDSLST                                                      
         GOTO1 DICTATE,DMCB,C'LU  ',(R2),(R3)                                   
*                                                                               
         SR    R2,R2               NO PFKEY AT TABLE FIRST                      
         CLI   ACTEQU,ACTLIST                                                   
         BE    S30                                                              
         LA    R3,PROPFKYH                                                      
         CLI   PFKEY,7             UP                                           
         BE    S40                                                              
         CLI   PFKEY,8             DOWN                                         
         BE    S40                                                              
         CLI   PFKEY,11            DOWN                                         
         BE    S40                                                              
         BASR  R2,0                                                             
         AHI   R2,PFTABLE-*                                                     
         ST    R2,APFTABLE                                                      
         B     S40                                                              
S30      BASR  R2,0                                                             
         AHI   R2,LPFTABLE-*                                                    
         ST    R2,ALPFTBLE                                                      
         LA    R3,PRLPFKYH                                                      
S40      GOTO1 INITIAL,DMCB,(X'40',(R2)),(R3)   INITIALIZE THE PFKEYS           
*                                                                               
         USING VALOPTD,R3                                                       
         L     R3,AOPTSTAB         TRANSLATE TABLE                              
         CLI   VOPTNAME,C' '       ALREADY DONE                                 
         BNL   SX                                                               
S50      GOTO1 DICTATE,DMCB,C'SU  ',VOPTNAME,0                                  
         LA    R3,VOPTLEN(R3)                                                   
         CLI   0(R3),0                                                          
         BNE   S50                                                              
         DROP  R3                                                               
*                                                                               
SX       XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*    CHECKS IF PROFILE SHOULD BE PROTECTED                                      
***********************************************************************         
*                                                                               
         USING DISTABD,R4          TABLE ENTRY                                  
         USING CITABD,R6           COST/INCOME TABLE                            
PROTEX   NTR1  BASE=*,LABEL=*                                                   
         L     R6,ACITABLE         FOR ALLOCATION BASIS VALIDATION              
PX10     CLC   DISNUMB,CITDOA      MATCH ON DEPT/OFF/AGY PROFILE                
         BE    PX50                                                             
         LA    R6,CITABLEN(R6)                                                  
         CLI   0(R6),0                                                          
         BNE   PX10                                                             
*                                                                               
PX50     MVC   BYTE,CITCI                                                       
         DROP  R6                                                               
*                                                                               
         USING CAPRECD,R6                                                       
         LA    R6,BIGKEY           MUST CHECK METHOD LEVEL FOR INCOME           
         XC    BIGKEY,BIGKEY                                                    
         MVC   CAPKEY,SPACES                                                    
         MVI   CAPKTYP,CAPKTYPQ    PROFILE REC                                  
         MVI   CAPKSUB,CAPKSUBQ                                                 
         MVC   CAPKCPY,CMPY        COMPANY                                      
         MVC   CAPKMTHD,METHNUM                                                 
         MVC   KEY2,BIGKEY                                                      
         GOTO1 HIGH                                                             
         CLC   BIGKEY(CAPKMIN),KEY2        SAME METHOD                          
         BNE   PXNO                NOTHING FOUND                                
         MVC   AIO,AIO2                                                         
         GOTO1 GETREC                                                           
         MVC   AIO,AIO1                                                         
*                                                                               
         L     R6,AIO2             LOOK FOR EXISTING INCOME SETTING             
         USING OPDELD,R6                                                        
         MVI   ELCODE,OPDELQ                                                    
         BAS   RE,GETEL                                                         
         B     *+8                                                              
PX60     BAS   RE,NEXTEL                                                        
         BNE   PXNO                                                             
         CLC   BYTE,OPDNUM         MATCH ON PROF NUMBER                         
         BNE   PX60                                                             
         CLI   OPDDATA,COINCOME    INCOME = PROTECT                             
         BNE   PXNO                                                             
PXYES    SR    RC,RC               PROTECT AT OFFICE LEVEL                      
PXNO     LTR   RC,RC                                                            
         XIT1                                                                   
         DROP  R4,R6                                                            
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*    VALIDATE OFFICE/DEPT/SUBDPT/PERSON - CAN ENTER UP TO ANY LEVEL   *         
***********************************************************************         
         SPACE 1                                                                
         USING GEND,RC                                                          
         USING SPOOLD,R8                                                        
         USING T61DFFD,RA          BASE SCREEN FOR SYSTEM + THIS PROG           
         USING SYSD,R9                                                          
         USING BLOCKSD,R5                                                       
*                                                                               
VALODSP  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R4,ACCNT            BUILD ACCOUNT IN ACCNT TO READ 1R            
         LA    R2,PROOFFH          OFFICE                                       
         CLI   5(R2),0             ANY DATA?                                    
         BE    VODSP04                                                          
*                                                                               
VODSP02  CLC   PROOFFH+5(1),LEVELLN         SHOULD = LEN LEVEL A ACC            
         BNE   VODSP16                                                          
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         MVC   OFFICE(0),PROOFF    SAVE OFFICE                                  
         EX    R1,*-6                                                           
         OI    LEVBIT,LEVOFF       OFFICE LEVEL                                 
         OI    LEVBIT,LEVGRP       OFFICE GROUP LEVEL                           
         MVC   0(0,R4),PROOFF     BUILD ACCOUNT TO READ 1R                      
         EX    R1,*-6                                                           
         AR    R4,R1                                                            
         LA    R4,1(R4)                                                         
*                                                                               
VODSP04  LA    R2,PRODEPTH         ANY DEPARTMENT                               
         CLI   5(R2),0                                                          
         BE    VODSP08                                                          
*                                                                               
         CLI   ACTEQU,ACTLIST      LIST DOESN'T NEED HIGHER LEVEL               
         BE    VODSP06             USED AS FILTER FOR LIST                      
         TM    LEVBIT,LEVOFF       MUST HAVE OFFICE                             
         BZ    EMISHIGH                                                         
*                                                                               
VODSP06  CLC   5(1,R2),LEVELLN+1                                                
         BNE   VODSP16                                                          
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         MVC   DEPT(0),8(R2)                                                    
         EX    R1,*-6                                                           
         OI    LEVBIT,LEVDPT       DEPT LEVEL                                   
         MVC   0(0,R4),8(R2)       R4 POINTS TO RIGHT PLACE IN ACCNT            
         EX    R1,*-6              BUILD ACCOUNT TO READ 1R                     
         AR    R4,R1                                                            
         LA    R4,1(R4)                                                         
*                                                                               
VODSP08  LA    R2,PROSDPTH         ANY SUB DEPT FILTER                          
         CLI   5(R2),0                                                          
         BE    VODSP12                                                          
*                                                                               
         CLI   ACTEQU,ACTLIST      LIST DOESN'T NEED HIGHER LEVEL               
         BE    VODSP10             USED AS FILTER FOR LIST                      
         TM    LEVBIT,LEVOFF+LEVDPT     MUST HAVE HIGHER LEVELS                 
         BNO   EMISHIGH                                                         
*                                                                               
VODSP10  CLC   5(1,R2),LEVELLN+2                                                
         BNE   VODSP16                                                          
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         MVC   SUBDPT(0),8(R2)                                                  
         EX    R1,*-6                                                           
         OI    LEVBIT,LEVSDPT      SUB DEPT LEVEL                               
         MVC   0(0,R4),8(R2)       R4 POINTS TO RIGHT PLACE IN ACCNT            
         EX    R1,*-6              BUILD ACCOUNT TO READ 1R                     
         AR    R4,R1                                                            
         LA    R4,1(R4)                                                         
*                                                                               
VODSP12  DS    0H                                                               
         CLI   ACTEQU,ACTLIST                                                   
         BE    VODSP18             NO PERSON FILTER FOR LIST                    
*                                                                               
         LA    R2,PROPERH          ANY PERSON                                   
         CLI   5(R2),0                                                          
         BE    VODSP14                                                          
*                                                                               
         TM    LEVBIT,LEVOFF+LEVDPT+LEVSDPT                                     
         BNO   EMISHIGH                                                         
         CLC   5(1,R2),LEVELLN+3                                                
         BH    ETOOLONG                                                         
*        BL    ETOOSHRT                                                         
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         MVC   PERSON(0),8(R2)                                                  
         EX    R1,*-6                                                           
         OI    LEVBIT,LEVPER       PERSON LEVEL                                 
         MVC   0(0,R4),8(R2)       R4 POINTS TO RIGHT PLACE IN ACCNT            
         EX    R1,*-6              BUILD ACCOUNT TO READ 1R                     
*                                                                               
VODSP14  DS    0H                  VALIDATE THAT 1R EXISTS                      
         LA    R2,PROOFFH          CURSOR AT OFFICE ON ERROR                    
         GOTO1 =A(VALACCNT),DMCB,RR=RELO                                        
*                                                                               
VODSP16  BH    ETOOLONG                                                         
         BL    ETOOSHRT                                                         
*&&US                                                                           
         TM    LEVBIT,LEVPER                                                    
         BZ    VODSP18                                                          
*                                                                               
         MVI   CNINES,C'9'     FILL CNINES WITH 9'S                             
         MVC   CNINES+1(L'CNINES-1),CNINES                                      
         LA    RF,OFFICE       SKIP OVERHEAD ACCOUNT RECORDS                    
         ZIC   RE,LEVELLN      (SEE ACCAP16, VALOVER ROUTINE)                   
         BCTR  RE,0                                                             
         EXCLC RE,0(RF),CNINES                                                  
         BE    VODSP18                                                          
*                                                                               
         LA    RF,DEPT                                                          
         ZIC   RE,LEVELLN+1                                                     
         BCTR  RE,0                                                             
         EXCLC RE,0(RF),CNINES                                                  
         BE    VODSP18                                                          
*                                                                               
         LA    RF,SUBDPT                                                        
         ZIC   RE,LEVELLN+2                                                     
         BCTR  RE,0                                                             
         EXCLC RE,0(RF),CNINES                                                  
         BE    VODSP18                                                          
*                                                                               
         LA    RF,PERSON                                                        
         ZIC   RE,LEVELLN+3                                                     
         CH    RE,=H'3'        ONLY NEED 3 NINES IN LOW LEVEL TO                
         BL    *+8             BE AN OVERHEAD ACCOUNT                           
         LA    RE,3                                                             
         BCTR  RE,0                                                             
         EXCLC RE,0(RF),CNINES                                                  
         BE    VODSP18                                                          
*                                                                               
         GOTO1 =A(RDPER),DMCB,RR=RELO                                           
*&&                                                                             
*                                                                               
VODSP18  XIT1                                                                   
         EJECT                                                                  
                                                                                
***********************************************************************         
*         VALIDATE OFFICE,DEPT,SUBDPT,PERSON IN 1R                   *          
***********************************************************************         
*                                                                               
VALACCNT NTR1  BASE=*,LABEL=*      VALIDATE LOCATION ENTERED                    
         LA    R6,KEY2             BUILD KEY                                    
         USING ACTRECD,R6                                                       
*                                                                               
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,CMPY        COMPANY                                      
         MVC   ACTKUNT(2),=C'1R'   UNIT 1, LEDGER R                             
         MVC   ACTKACT,ACCNT                                                    
*                                                                               
         L     R6,AIO2                                                          
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'ACCOUNT',KEY2,AIO2                    
         CLC   KEY2,0(R6)                                                       
         BNE   EINVACC             INVALID ACCOUNT                              
*                                                                               
VAX      DS    0H                                                               
         XIT1                                                                   
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*                           LTORG                                     *         
***********************************************************************         
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*         READ PERSON RECORD AND CHECK IF FREELANCER                 *          
***********************************************************************         
*                                                                               
*&&US                                                                           
RDPER    NTR1  BASE=*,LABEL=*      VALIDATE LOCATION ENTERED                    
         LA    R6,KEY2             BUILD KEY                                    
         USING PERRECD,R6                                                       
*                                                                               
         MVI   FREELNCR,C'N'                                                    
         MVC   PERKEY,SPACES                                                    
         MVI   PERKTYP,PERKTYPQ                                                 
         MVC   PERKCPY,CMPY                                                     
         MVC   PERKCODE,PERSON                                                  
         GOTO1 DATCON,DMCB,(5,0),(1,WAREA1)                                     
*                                                                               
         L     R6,AIO2                                                          
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'ACCOUNT',KEY2,AIO2                    
         CLC   KEY2,0(R6)                                                       
         BNE   EINVACC             INVALID ACCOUNT                              
*                                                                               
         SR    R0,R0                                                            
         LA    RF,ACCORFST(R6)                                                  
         USING LOCELD,RF                                                        
RDPER10  CLI   LOCEL,0                                                          
         BE    RDPERX          CHECK IF PID NEEDED                              
         CLI   LOCEL,LOCELQ    LOCATION ELEMENT? X'83'                          
         BE    RDPER20                                                          
RDPER15  IC    R0,LOCLN                                                         
         AR    RF,R0                                                            
         B     RDPER10                                                          
*                                                                               
RDPER20  CLC   LOCOFF,OFFICE                                                    
         BNE   RDPER15                                                          
         CLC   LOCDEPT,DEPT                                                     
         BNE   RDPER15                                                          
         CLC   LOCSUB,SUBDPT                                                    
         BNE   RDPER15                                                          
         CLC   LOCSTART,WAREA1     RIGHT LOCATION?                              
         BH    RDPER15                                                          
         OC    LOCEND,LOCEND                                                    
         BZ    *+14                                                             
         CLC   LOCEND,WAREA1                                                    
         BL    RDPER15                                                          
         TM    LOCATTR,LOCYFRL     FREELANCER?                                  
         BNO   RDPERX                                                           
         MVI   FREELNCR,C'Y'                                                    
*                                                                               
RDPERX   XIT1                                                                   
         DROP  R6,RF                                                            
***********************************************************************         
*                           LTORG                                     *         
***********************************************************************         
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*&&                                                                             
***********************************************************************         
* DAILY TIME/MCS TIME DATE CHANGE RECORD                              *         
*  - ENSURE VARIOUS LEVELS OF 1R HOLD DATE OF CHANGES                 *         
*  - TO DAILY TIME/MCS TIME PROFILE                                   *         
***********************************************************************         
         SPACE 1                                                                
         USING GEND,RC                                                          
         USING SPOOLD,R8                                                        
         USING T61DFFD,RA          BASE SCREEN FOR SYSTEM + THIS PROG           
         USING SYSD,R9                                                          
         USING BLOCKSD,R5                                                       
MYGDTYP  EQU   BYTE1               GDATYPE                                      
MYOPTYN  EQU   BYTE2               OPTION SETTING (Y/N)                         
*                                                                               
DTIMDATE NTR1  BASE=*,LABEL=*                                                   
                                                                                
         CLI   PIDV,1       CALLED DTIMDATE FROM VALPIDS?                       
         BE    DTIM01                                                           
*                                                                               
DTINI    MVI   MYGDTYP,GDATDTIM                                                 
         MVC   MYOPTYN,DTIMEST                                                  
         TM    BITS2,DTIMDATQ      TEST DAILY TIME CHANGE                       
         BZ    *+12                                                             
         NI    BITS2,X'FF'-DTIMDATQ YES - CLEAR BIT AND CONTINUE                
         B     DTIM01                                                           
DTIM00   TM    BITS2,MCSTDATQ      NO - MUST BE MCS TIME CHANGE                 
         BNZ   *+6                                                              
         DC    H'0'                WHAT ELSE?                                   
         MVI   MYGDTYP,GDATMCST                                                 
         MVC   MYOPTYN,MCSTIME                                                  
         NI    BITS2,X'FF'-MCSTDATQ CLEAR BIT                                   
DTIM01   GOTO1 DATCON,DMCB,(5,0),(1,WAREA1)                                     
         GOTO1 =A(GETCAL),DMCB,RR=RELO                                          
         CLI   PIDV,1     CALLED DTIMDATE FROM VALPIDS?                         
         BNE   DTIM01A    NO                                                    
         GOTO1 DATCON,DMCB,(5,0),(1,WAREA1)                                     
         GOTO1 =A(GETCAL),DMCB,RR=RELO                                          
         MVC   SWDAY(3),WAREA1+3                                                
         MVI   PIDV,0                                                           
         B     DTIMX                                                            
                                                                                
DTIM01A  XC    WAREA1+6(3),WAREA1+6                                             
         LA    R6,BIGKEY           BUILD KEY                                    
         USING ACTRECD,R6                                                       
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,CMPY        COMPANY                                      
         MVC   ACTKUNT(2),=C'1R'   UNIT 1, LEDGER R                             
         MVC   ACTKACT,ACCNT                                                    
*                                                                               
         MVC   KEY2,BIGKEY                                                      
         MVC   SVKEY2,BIGKEY                                                    
         GOTO1 HIGH                                                             
         CLC   BIGKEY(L'ACTKEY),KEY2                                            
         BE    *+6                 NOTHING FOUND                                
         DC    H'0'                DIE                                          
         MVC   AIO,AIO2                                                         
         GOTO1 GETREC                                                           
*                                                                               
         USING GDAELD,R6                                                        
         L     R6,AIO2             LOOK FOR EXISTING GENERAL DATE ELEM          
         MVI   ELCODE,GDAELQ       X'E5'                                        
         BAS   RE,GETEL                                                         
         B     *+8                                                              
DTIM02   BAS   RE,NEXTEL                                                        
         BE    DTIM50                                                           
         XC    ELEM,ELEM                                                        
         LA    R6,ELEM                                                          
         MVI   GDAEL,GDAELQ                                                     
         MVI   GDALN,GDALN2Q                                                    
         MVC   GDATYPE,MYGDTYP     SET ELEMENT TYPE                             
         CLI   MYOPTYN,COOPTYQ     SET TO YES                                   
         BNE   DTIM04                                                           
         MVC   GDADATE,WAREA1+3                                                 
         B     DTIM32                                                           
DTIM04   MVC   GDADATE2,WAREA1+3                                                
*                                                                               
* READ FOR HIGHER LEVELS                                                        
*                                                                               
         CLI   MYGDTYP,GDATMCST    IF BRO GDAEL, EL MAY NOT EXIST IF...         
         BE    DTIM06              ...VALPIDS PREVENTED 'Y' BEING SET           
         CLC   ACCNT,SPACES        LEDGER LEVEL SHOULD HAVE GDADATE             
         BNE   *+6                                                              
         DC    H'0'                DIE AS WE HAVEN'T GOT A DATE                 
DTIM06   MVC   AIO,AIO3                                                         
         GOTO1 GETLDG,DMCB,C'1R'   GET LEDGER RECORD AND INFO                   
         LA    R4,LDGLN1           R4=LENGTH OF THIS LEVEL                      
*                                                                               
         USING GDAELD,R6                                                        
DTIM10   L     R6,AIO3             LOOK FOR EXISTING GENERAL DATE ELEM          
         MVI   ELCODE,GDAELQ       X'E5'                                        
         BAS   RE,GETEL                                                         
         B     *+8                                                              
DTIM12   BAS   RE,NEXTEL           HAVE WE FOUND ELEMENT                        
         BNE   DTIM20                                                           
                                                                                
DTIM14   CLC   GDATYPE,MYGDTYP     IS ELEMENT CORRECT TYPE                      
         BNE   DTIM12              NO - GET NEXT ELEMENT                        
         OC    GDADATE2,GDADATE2   HAS DAILY TIME BEEN SWITCHED OFF             
         BNZ   DTIM12              YES - GET NEXT ELEMENT                       
         CLC   GDADATE,WAREA1+3   CHECK ON DATE ISN'T HIGHER THAN START         
         BH    DTIM12              HIGHER - GET NEXT ELEMENT                    
         OC    WAREA1+6(3),WAREA1+6 HAVE WE GOT A DATE FROM PREV LEVELS         
         BZ    DTIM16              NO                                           
         CLC   GDADATE,WAREA1+6    YES - CHECK DATE IS AFTER THIS ONE           
         BNH   DTIM12              NO - GET NEXT ELEMENT                        
DTIM16   MVC   WAREA1+6(L'GDADATE),GDADATE    SAVE ON DATE                      
         B     DTIM12                                                           
                                                                                
DTIM20   XR    RF,RF                                                            
         IC    RF,0(R4)            LENGTH OF THIS LEVEL                         
         SHI   RF,1                SUBTRACT ONE FOR EX                          
         LA    R6,BIGKEY           BUILD KEY                                    
         USING ACTRECD,R6                                                       
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,CMPY        COMPANY                                      
         MVC   ACTKUNT(2),=C'1R'   UNIT 1, LEDGER R                             
         EXMVC RF,ACTKACT,ACCNT                                                 
*                                                                               
         MVC   KEY2,BIGKEY         READ HIGHER LEVEL RECORD                     
         GOTO1 HIGH                                                             
         CLC   BIGKEY(L'ACTKEY),KEY2                                            
         BE    *+6                 NOTHING FOUND                                
         DC    H'0'                DIE                                          
         GOTO1 GETREC                                                           
         CLC   SVKEY2(L'ACTKEY),BIGKEY HAVE WE REACHED LEVEL OF PROFILE         
         BE    DTIM30              YES - EXIT                                   
         LA    R4,L'ACLVALS(R4)    NO - MOVE TO NEXT LEVEL                      
         B     DTIM10              READ FOR DAILY TIME START DATE               
*                                                                               
         USING GDAELD,R6                                                        
DTIM30   LA    R6,ELEM                                                          
         MVC   GDADATE,WAREA1+6                                                 
         MVC   AIO,AIO2                                                         
DTIM32   GOTO1 ADDELEM                                                          
         B     DTIM60                                                           
*                                                                               
DTIM50   CLC   GDATYPE,MYGDTYP     IS ELEMENT CORRECT TYPE                      
         BNE   DTIM02                                                           
         CLI   MYOPTYN,COOPTYQ     SET TO YES                                   
         BNE   DTIM58                                                           
*&&US                                                                           
         OC    GDADATE,GDADATE     ANY START DATE?                              
         BNZ   DTIM51                                                           
         MVC   GDADATE,WAREA1+3                                                 
         XC    GDADATE2,GDADATE2                                                
         B     DTIM60                                                           
                                                                                
DTIM51   DS    0H                                                               
*&&                                                                             
         CLC   GDADATE,WAREA1+3                                                 
         BNE   DTIM52                                                           
         XC    GDADATE2,GDADATE2                                                
         B     DTIM60                                                           
*                                                                               
DTIM52   CLC   GDADATE2,WAREA1+3 IS NEW START DATE SAME AS OLD END DATE         
         BNE   DTIM02              NO                                           
         XC    GDADATE2,GDADATE2   YES - CLEAR END DATE                         
         B     DTIM60                                                           
*                                                                               
DTIM58   OC    GDADATE2,GDADATE2                                                
         BNZ   DTIM02                                                           
         MVC   GDADATE2,WAREA1+3                                                
         CLC   GDADATE,WAREA1+3   CHECK END AND START DATE AREN'T EQUAL         
         BNE   DTIM60              NO - PUT RECORDS BACK                        
         MVI   GDAEL,X'FF'         YES - DELETE ELEMENT                         
         MVI   ELCODE,X'FF'                                                     
         GOTO1 REMELEM                                                          
*                                                                               
DTIM60   GOTO1 PUTREC                                                           
         TM    BITS2,MCSTDATQ      TEST MCS TIME CHANGE AS WELL                 
         BNZ   DTIM00              YES - GO BACK AND PROCESS THAT               
         MVC   AIO,AIO1            ELSE FINISHED                                
         XC    SVKEY2,SVKEY2                                                    
DTIMX    XIT1                                                                   
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*                           LTORG                                     *         
***********************************************************************         
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        GET CALENDAR RECORD                                                    
***********************************************************************         
*                                                                               
GETCAL   NTR1  BASE=*,LABEL=*                                                   
         USING CASRECD,R6          READ FOR CALENDAR                            
         LA    R6,BIGKEY                                                        
         XC    WAREA1+3(3),WAREA1+3                                             
         XC    BIGKEY,BIGKEY                                                    
         MVI   CASKTYP,CASKTYPQ    X'3E'                                        
         MVI   CASKSUB,CASKSUBQ    X'0B'                                        
         MVC   CASKCPY,CMPY        COMPANY                                      
         MVC   CASKEMOA,WAREA1     DATE TO READ HIGH FOR                        
         GOTO1 HIGH                                                             
         CLC   BIGKEY(CASKEMOA-CASKEY),KEYSAVE     SAME COMP                    
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
GCAL02   LA    R6,BIGKEY                                                        
         CLC   CASKEMOA,WAREA1     IF END DATE IS LOWER THAN TODAY              
         BL    GCSEQ               GET NEXT RECORD                              
         CLC   CASKSMOA,WAREA1     IF START DATE HIGHER THAN TODAY              
         BH    GCALX               END                                          
GCAL04   OC    OFFICE,OFFICE       HAVE WE GOT AN OFFICE                        
         BNZ   GCAL08              YES                                          
         CLC   CASKOFC,SPACES      NO - HAS KEY GOT AN OFFICE                   
         BNE   GCSEQ               NOT INTERESTED IN THIS RECORD                
         B     GCAL10                                                           
GCAL08   CLC   CASKOFC,SPACES                                                   
         BE    GCAL10                                                           
         CLC   OFFICE,CASKOFC                                                   
         BE    GCAL10                                                           
                                                                                
GCSEQ    GOTO1 SEQ                                                              
         CLC   BIGKEY(CASKEMOA-CASKEY),KEYSAVE                                  
         BE    GCAL02                                                           
         B     GCALX                                                            
                                                                                
GCAL10   MVC   AIO,AIO3                                                         
         GOTO1 READ                                                             
         GOTO1 GETREC                                                           
                                                                                
         L     R6,AIO3                                                          
         USING TMPELD,R6           SAVE CALENDAR START AND END DATES            
GCAL20   L     R6,AIO3                                                          
         MVI   ELCODE,TMPELQ       TIMESHEET PERIOD ELEMS                       
         BAS   RE,GETEL            ARE THERE MORE THAN 53 PERIODS               
         B     GCAL30                                                           
                                                                                
GCALNX   BAS   RE,NEXTEL                                                        
GCAL30   BNE   GCAL40                                                           
         CLC   TMPSTART,WAREA1                                                  
         BH    GCAL40                                                           
         CLC   TMPEND,WAREA1                                                    
         BL    GCALNX                                                           
         GOTO1 DATCON,DMCB,(1,TMPEND),(0,WORK)                                  
         GOTO1 ADDAY,DMCB,WORK,WORK+10,F'1'                                     
         GOTO1 DATCON,DMCB,(0,WORK+10),(1,WAREA1+3)                             
GCAL40   B     GCSEQ                                                            
                                                                                
GCALX    OC    WAREA1+3(3),WAREA1+3                                             
         BNZ   *+6                                                              
         DC    H'0'                                                             
         XIT1                                                                   
***********************************************************************         
*                           LTORG                                     *         
***********************************************************************         
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*          DELETE RECORD IF NO ELEMENTS ARE ON IT                     *         
***********************************************************************         
*                                                                               
ANYELEMS NTR1  BASE=*,LABEL=*                                                   
         L     R6,AIO              LOOK FOR EXISTING OFFICE LIST ELEM           
         USING OPDELD,R6                                                        
         MVI   ELCODE,OPDELQ                                                    
         BAS   RE,GETEL                                                         
         BE    ANYELEMX                                                         
*                                                                               
         L     R6,AIO              LOOK FOR EXISTING OFFICE LIST ELEM           
         USING CAPRECD,R6                                                       
         OI    CAPRSTAT,X'80'      MARK FOR DELETION                            
         LA    R6,BIGKEY                                                        
         OI    CAPKSTAT,X'80'                                                   
ANYELEMX XIT1                                                                   
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*                           LTORG                                     *         
***********************************************************************         
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*                   VALIDATE PIDS                                     *         
***********************************************************************         
*                                                                               
         USING DSPLINED,R2                                                      
VALPIDS  NMOD1 0,*VALPID*                                                       
         L     RC,SAVERC                                                        
         MVC   SVKEY1,BIGKEY                                                    
         MVC   PCODE,SPACES                                                     
         MVI   CNINES,C'9'     FILL CNINES WITH 9'S                             
         MVC   CNINES+1(L'CNINES-1),CNINES                                      
*                              GET SWITCH DATE                                  
         MVI   PIDV,1                                                           
         GOTO1 =A(DTIMDATE),DMCB,RR=RELO                                        
*                                                                               
         CLC   PERSON,SPACES   CHECKING PID FOR A PARTICULAR PERSON?            
         BNE   VPID20          YES                                              
         MVI   LDGFLG,0                                                         
                                                                                
         LA    R0,3                                                             
         SR    R1,R1                                                            
         SR    RE,RE                                                            
         LA    RF,LEVELLN      RF CONTAINS LEVEL LENGTHS                        
VPID02   IC    R1,0(,RF)                                                        
         AR    RE,R1                                                            
         LA    RF,1(RF)                                                         
         BCT   R0,VPID02                                                        
         STC   RE,LENSTOR      LENSTOR=L'OFFICE+L'DEPT+L'SUBDEPT                
*                                                                               
         LA    R3,KEY3                                                          
         USING ACTRECD,R3      READ ACCOUNT RECORD                              
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,CMPY                                                     
         MVC   ACTKUNT(2),=C'1R'                                                
         MVC   ACTKACT,ACCNT                                                    
         MVC   SVKEY3,KEY3                                                      
*                                                                               
         LA    RE,L'ACTKACT                                                     
         LA    R6,ACTKACT+L'ACTKACT-1                                           
VPID04   CLI   0(R6),C' '                                                       
         BH    VPID06                                                           
         AHI   R6,-1                                                            
         BCT   RE,VPID04                                                        
         MVI   LDGFLG,1        CHECK PIDS OVER WHOLE COMPANY 1R LEDGER          
         B     VPID08                                                           
*                                                                               
VPID06   LA    RF,ACTRECD                                                       
         SR    R6,RF                                                            
         STC   R6,MYCHAR                                                        
*                                                                               
VPID07   SR    RE,RE                                                            
         IC    RE,ACTKACT+L'ACTKACT-1                                           
         LA    RE,1(RE)                                                         
         STC   RE,ACTKACT+L'ACTKACT-1                                           
VPID08   GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'ACCDIR',KEY3,KEY3                     
         CLI   LDGFLG,1                                                         
         BNE   VPID12                                                           
         CLC   SVKEY3(ACTKACT-ACTRECD),KEY3 SAME COMPANY U&L?                   
         BE    VPID14                                                           
         B     VPIXIT          NO (FURTHER) PIDS TO CHECK                       
VPID12   SR    R1,R1                                                            
         IC    R1,MYCHAR                                                        
         EXCLC R1,SVKEY3,KEY3  CHECK WE ARE NOT AT A HIGHER LEVEL               
         BNE   VPIXIT                                                           
*                                                                               
VPID14   LA    RF,ACTKACT      CHECK FOR A PERSON CODE                          
         ZIC   R1,LENSTOR                                                       
         AR    RF,R1                                                            
         ZIC   RE,LEVELLN+3                                                     
         BCTR  RE,0                                                             
         EXCLC RE,0(RF),SPACES                                                  
         BE    VPID07          READ NEXT RECORD                                 
                                                                                
         LA    RF,ACTKACT      SKIP OVERHEAD ACCOUNT RECORDS                    
         ZIC   RE,LEVELLN      (SEE ACCAP16, VALOVER ROUTINE)                   
         BCTR  RE,0                                                             
         EXCLC RE,ACTKACT,CNINES                                                
         BE    VPID07                                                           
         LA    RF,1(RE,RF)                                                      
                                                                                
         ZIC   RE,LEVELLN+1                                                     
         BCTR  RE,0                                                             
         EXCLC RE,0(RF),CNINES                                                  
         BE    VPID07                                                           
         LA    RF,1(RE,RF)                                                      
                                                                                
         ZIC   RE,LEVELLN+2                                                     
         AR    RF,RE                                                            
                                                                                
         ZIC   RE,LEVELLN+3                                                     
         CH    RE,=H'3'        ONLY NEED 3 NINES IN LOW LEVEL TO                
         BL    *+8             BE AN OVERHEAD ACCOUNT                           
         LA    RE,3                                                             
         BCTR  RE,0                                                             
         EXCLC RE,0(RF),CNINES                                                  
         BE    VPID07                                                           
*                                                                               
*   ** RETRIEVE PERSON CODE **                                                  
*                                                                               
         MVC   PCODE,SPACES                                                     
         LR    R1,RF           R1=A(PERSON CODE)                                
         SR    RE,RE                                                            
         IC    RE,LEVELLN+3    LENGTH OF PERSON LEVEL                           
         LTR   RE,RE                                                            
         BNZ   *+6                                                              
         DC    H'0'            L'PERSON LEVEL SHOULD BE GREATER THAN 0?         
         AHI   RE,-1           RE=LENGTH OF PERSON LEVEL-1                      
         AR    RF,RE           RF=A(PERSON_CODE+L'PERSON_CODE-1)                
VPID16   CLI   0(RF),C' '                                                       
         BH    VPID18                                                           
         AHI   RF,-1                                                            
         BCT   RE,VPID16                                                        
VPID18   EXMVC RE,PCODE,0(R1)  SAVE PERSON CODE                                 
*                                                                               
*   ** READ PERSON RECORD TO CHECK PID **                                       
*                                                                               
VPID20   NI    PILOBYT,X'FF'-(PIDIBYT+LDAYBYT+PIDTERM)                          
*&&US*&& NI    PILOBYT,X'FF'-PIDFRL                                             
         XC    SVSTRT(9),SVSTRT                                                 
                                                                                
         LA    R6,KEY2                                                          
         USING PERRECD,R6      READ PERSON RECORD                               
         MVC   PERKEY,SPACES                                                    
         MVI   PERKTYP,PERKTYPQ                                                 
         MVC   PERKCPY,CMPY                                                     
         MVC   PERKCODE,PCODE                                                   
         CLC   PERSON,SPACES                                                    
         BE    VPID21                                                           
         MVC   PERKCODE,PERSON                                                  
         MVC   PCODE,PERSON    FOR THE SAKE OF EMISPID AND EINVLOC              
*                                                                               
VPID21   L     R6,AIO2                                                          
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'ACCOUNT',KEY2,AIO2                    
         BE    VPID22                                                           
         CLC   PERSON,SPACES       N/F: TEST SPECIFIC PERSON WANTED             
         BE    VPID07              NO - ALLOW MISSING PERRECS                   
         DC    H'0'                YES - DIDN'T FIND IT                         
*                                                                               
VPID22   SR    R0,R0                                                            
         LA    RF,ACCORFST(R6) GET LOCELD AND PIDELD                            
         USING LOCELD,RF                                                        
VPID23   CLI   LOCEL,0                                                          
         BE    VPID40          CHECK IF PID NEEDED                              
         CLI   LOCEL,LOCELQ    LOCATION ELEMENT? X'83'                          
         BE    VPID26                                                           
         CLI   LOCEL,EMPELQ    EMPLOYEE HISTORY ELEMEN? X'56'                   
         BE    VPID37                                                           
         CLI   LOCEL,PIDELQ    PERSONAL ID ELEMENT? X'D8'                       
         BE    VPID38                                                           
VPID24   IC    R0,LOCLN                                                         
         AR    RF,R0                                                            
         B     VPID23                                                           
*                                                                               
VPID26   CLC   SVSTRT(3),LOCSTART                                               
         BH    VPID30                                                           
         MVC   SVSTRT(3),LOCSTART                                               
VPID30   CLI   LDGFLG,1        CHECKING ACROSS WHOLE AGENCY/1R LEDGER?          
         BE    VPID32          YES                                              
         CLC   OFFICE,LOCOFF   CHECK OFFICE                                     
         BNE   VPID24                                                           
         CLI   DEPT,C' '                                                        
         BE    VPID32                                                           
         CLC   DEPT,LOCDEPT    CHECK DEPT                                       
         BNE   VPID24                                                           
         CLI   SUBDPT,C' '                                                      
         BE    VPID32                                                           
         CLC   SUBDPT,LOCSUB   CHECK SUB-DEPT                                   
         BNE   VPID24                                                           
VPID32   CLC   SWDAY,LOCSTART  IS START<SW?                                     
         BNL   VPID34          YES                                              
         OI    PILOBYT,LDAYBYT MARK 'NEED PID IN FUTURE' SW<START               
         MVC   SVSTRT+6(3),LOCSTART                                             
         B     VPID24                                                           
VPID34   CLI   LOCSTAT,LOCSACT (NO LOCEND DATE)                                 
         BE    VPID36                                                           
         CLC   SWDAY,LOCEND    IS START<SW<END?                                 
         BL    VPID36          YES                                              
         CLI   LOCSTAT,LOCSTRM START<END<SW                                     
         BE    VPID35                                                           
         CLC   SVSTRT+3(3),LOCSTART                                             
         BH    VPID24                                                           
         MVC   SVSTRT+3(3),LOCSTART                                             
         B     VPID24                                                           
VPID35   CLC   SVSTRT+6(3),LOCSTART                                             
         BH    *+8                                                              
         NI    PILOBYT,X'FF'-LDAYBYT TURN OFF 'NEED A PID'                      
         B     VPID24                                                           
VPID36   OI    PILOBYT,LDAYBYT MARK 'NEED PID NOW'                              
         MVC   SVSTRT+6(3),LOCSTART                                             
*&&US                                                                           
         TM    LOCATTR,LOCYFRL     ARE THEY A NON-BRANDO FREELANCE?             
         BNO   VPID24                                                           
         OI    PILOBYT,PIDFRL      MARK PID AS FREELANCER                       
*&&                                                                             
         B     VPID24                                                           
*                                                                               
         USING EMPELD,RF                                                        
VPID37   OC    EMPTRM,EMPTRM       IS PID TERMED?                               
         BZ    VPID24                                                           
         OI    PILOBYT,PIDTERM     MARK PID AS TERMED                           
         B     VPID24                                                           
*                                                                               
         USING PIDELD,RF                                                        
VPID38   OC    PIDNO,PIDNO                                                      
         BZ    VPID40                                                           
         OI    PILOBYT,PIDIBYT PID FOUND                                        
         B     VPID42                                                           
VPID40   DS    0H                                                               
*&&US                                                                           
         TM    PILOBYT,PIDFRL      IS THE PID FREELANCER?                       
         BO    VPID54                                                           
*&&                                                                             
         TM    PILOBYT,PIDTERM     IS THE PID TERMINATED?                       
         BO    VPID42                                                           
         TM    PILOBYT,LDAYBYT     NEED A PID NOW OR IN FUTURE?                 
         BO    VPID44                                                           
         OC    SVSTRT,SVSTRT                                                    
         BZ    VPID42                                                           
         CLC   SVSTRT(3),SVSTRT+3                                               
         BE    VPID44                                                           
                                                                                
VPID42   CLC   PERSON,SPACES   CHECKING PID FOR ONE PERSON ONLY?                
         BE    VPID46                                                           
*&&US                                                                           
         TM    PILOBYT,PIDFRL                                                   
         BO    VPID54                                                           
*&&                                                                             
         TM    PILOBYT,PIDIBYT+PIDTERM                                          
         BNZ   VPID46                                                           
VPID44   LA    R2,DSPSETH                                                       
         B     EMISPID         PID MISSING FOR PERSON &T                        
*                                                                               
VPID46   DS    0H                                                               
*&&US                                                                           
         TM    PILOBYT,PIDFRL                                                   
         BO    VPID54                                                           
*&&                                                                             
         LA    R6,SVKEY2                                                        
         USING TSWRECD,R6      READ TIMESHEET WEEKLY POINTER                    
         XC    TSWKEY,TSWKEY                                                    
         MVI   TSWKTYP,TSWKTYPQ                                                 
         MVI   TSWKSUB,TSWKSUBQ                                                 
         MVC   TSWKCPY,CMPY                                                     
         MVC   TSWKPER,PCODE                                                    
*                                                                               
         SR    R1,R1                                                            
         ICM   R1,7,SWDAY                                                       
         LNR   R1,R1                                                            
         STCM  R1,7,WORK                                                        
         MVC   KEY2,SVKEY2                                                      
         LA    R2,DSPSETH                                                       
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'ACCDIR',SVKEY2,SVKEY2                 
         B     VPID50                                                           
VPID48   GOTO1 DATAMGR,DMCB,=C'DMRSEQ',=C'ACCDIR',SVKEY2,SVKEY2                 
VPID50   CLC   TSWKEY(TSWKEND-TSWKEY),KEY2                                      
         BNE   VPID52                                                           
         CLC   TSWKEND,WORK                                                     
         BNL   VPID52                                                           
*                                                                               
         CLC   OFFICE,SPACES   DO WE HAVE AN OFFICE?                            
         JNH   VPID51                                                           
         LA    RF,TSWKODS                                                       
         ZIC   RE,LEVELLN                                                       
         BCTR  RE,0                                                             
         EXCLC RE,0(RF),OFFICE                                                  
         BNE   VPID48                                                           
         LA    RF,1(RE,RF)                                                      
*                                                                               
         CLC   DEPT,SPACES     DO WE HAVE A DEPARTMENT?                         
         JNH   VPID51                                                           
         ZIC   RE,LEVELLN+1                                                     
         BCTR  RE,0                                                             
         EXCLC RE,0(RF),DEPT                                                    
         BNE   VPID48                                                           
         LA    RF,1(RE,RF)                                                      
*                                                                               
         CLC   SUBDPT,SPACES   DO WE HAVE A SUB-DEPARTMENT?                     
         JNH   VPID51                                                           
         ZIC   RE,LEVELLN+2                                                     
         EXCLC RE,0(RF),SUBDPT                                                  
         BNE   VPID48                                                           
*                                                                               
VPID51   TM    TSWKSTAT,TIMSDELT IGNORE DELETED TIMESHEETS                      
         BNZ   VPID48                                                           
         B     ETIMEXS         ERROR TIME EXISTS                                
VPID52   CLC   PERSON,SPACES   CHECKING PID FOR ONE PERSON ONLY?                
         BE    VPID07                                                           
*                                                                               
*&&US                                                                           
VPID54   CLC   PERSON,SPACES   CHECKING PID FOR ONE PERSON ONLY?                
         BNE   VPIXIT                                                           
         TM    PILOBYT,PIDFRL      ARE THEY A FREELANCER?                       
         BNO   VPID07                                                           
         MVC   FULL,AIO                                                         
         MVC   AIO,AIO2                                                         
         GOTO1 DATCON,DMCB,(1,SVSTRT),(0,WORK)                                  
         GOTO1 ADDAY,DMCB,WORK,WORK+10,F'-1'                                    
         GOTO1 DATCON,DMCB,(0,WORK+10),(1,WORK)                                 
         L     R6,AIO2                                                          
         XC    BIGKEY,BIGKEY                                                    
         MVC   BIGKEY(L'ACTKEY),KEY3                                            
         GOTO1 HIGH                                                             
         CLC   BIGKEY(L'ACTKEY),KEYSAVE                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 GETREC                                                           
*                                                                               
         SR    R0,R0                                                            
         LR    RF,R6                                                            
         AH    RF,DATADISP                                                      
         USING GDAELD,RF                                                        
VPID56   CLI   GDAEL,0                                                          
         BE    VPID62                                                           
         CLI   GDAEL,GDAELQ                                                     
         BNE   VPID58                                                           
         CLI   GDATYPE,GDATMCST    BRANDOCEAN TIME?                             
         BE    VPID60                                                           
VPID58   IC    R0,GDALN                                                         
         AR    RF,R0                                                            
         B     VPID56                                                           
*                                                                               
VPID60   OC    GDADATE2,GDADATE2   ANY END DATE?                                
         BNZ   VPID58                                                           
         CLC   GDADATE,WORK        MAKE SURE NEW DATE IS < START                
         BNH   *+10                                                             
         MVC   WORK(L'GDADATE),GDADATE                                          
         MVC   GDADATE2,WORK                                                    
         B     VPID64                                                           
*                                                                               
VPID62   XC    ELEM,ELEM                                                        
         LA    RF,ELEM                                                          
         MVI   GDAEL,GDAELQ                                                     
         MVI   GDALN,GDALN2Q                                                    
         MVI   GDATYPE,GDATMCST    SET ELEMENT TYPE                             
         MVC   GDADATE,WORK                                                     
         MVC   GDADATE2,WORK                                                    
         DROP  RF                                                               
*                                                                               
         GOTO1 ADDELEM                                                          
VPID64   GOTO1 PUTREC                                                           
         MVC   AIO,FULL                                                         
*                                                                               
* RESTORE SEQUENCE                                                              
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'ACCDIR',KEY3,KEY3                     
         B     VPID07                                                           
*&&                                                                             
*                                                                               
VPIXIT   MVC   AIO,AIO1        RE-ESTABLISH AIO                                 
         MVC   BIGKEY,SVKEY1   RE-ESTABLISH BIGKEY                              
         XIT1                  ALL OK                                           
*                                                                               
         DROP  R3,R6                                                            
*                                                                               
***********************************************************************         
*                           LTORG                                     *         
***********************************************************************         
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*    CHECKS IF PROFILE SHOULD BE SHOWN                                          
*    GETS LEVEL FROM KEY IN AIO                                                 
*    R3 SHOULD BE POINTING TO ENTRY IN PROFILE TABLE                            
***********************************************************************         
*                                                                               
SHOWPROF NMOD1 0,*SHPROF*                                                       
         L     RC,SAVERC                                                        
*                                                                               
         USING COPFTABD,R3                                                      
*                                                                               
         MVC   PROFNUM,CONUM       PROFILE NUMBER                               
*                                                                               
         L     R6,AIO                                                           
         USING CAPRECD,R6                                                       
*                                                                               
         TM    COFLAG,AGYONLY      AGENCY LEVEL ONLY PROFILE                    
         BZ    SP00                                                             
         CLC   CAPKMTHD(L'CAPKEY-(CAPKOFC-CAPKEY)),SPACES                       
         BE    SHOWXY              'BASE' PROFILE RECORD - OK                   
         B     SHOWXN                                                           
*                                                                               
SP00     TM    COFLAG,HIGHLEV      IS THE PROFILE FROM A HIGHER LEVEL           
*                                  THAN METHOD?                                 
         BZ    *+12                                                             
         TM    OPTBIT,OPTGRP       IS A GROUP OPTION ON?                        
         BO    SHOWXY                                                           
SP01     CLC   CAPKMTHD,SPACES                                                  
         BNE   SP05                                                             
         TM    COSHOW,COLNOMTH     NO METHOD                                    
         BNO   SHOWXN                                                           
         CLC   CAPKOFC,SPACES      ANY OFFICE                                   
         BNE   SP10                                                             
         B     SHOWXY                                                           
*                                                                               
SP05     TM    COSHOW,COLNOMTH     THERE IS A METHOD SO                         
         BO    SHOWXN              SKIP IF FOR NO METHOD                        
         CLC   CAPKOFC,SPACES      ANY OFFICE                                   
         BNE   SP10                                                             
         TM    COSHOW,COLMETH      NO, THEN MUST BE AT METHOD LEVEL             
         BNO   SHOWXN                                                           
         BO    SHOWXY                                                           
*                                                                               
SP10     CLC   CAPKDPT,SPACES      ANY DEPT                                     
         BNE   SP20                                                             
         TM    COSHOW,COLOFFC      NO, THEN OFFICE LEVEL                        
         BNO   SHOWXN                                                           
         BO    SHOWXY                                                           
*                                                                               
SP20     CLC   CAPKSDT,SPACES      ANY SUB DEPT                                 
         BNE   SP30                                                             
         TM    COSHOW,COLDPT       NO, THEN DEPT LEVEL                          
         BNO   SHOWXN                                                           
         BO    SHOWXY                                                           
*                                                                               
SP30     CLC   CAPKPER,SPACES      ANY PERSON                                   
         BNE   SP40                                                             
         TM    COSHOW,COLSDT       NO, THEN SUB DEPT LEVEL                      
         BNO   SHOWXN                                                           
         BO    SHOWXY                                                           
*                                                                               
SP40     TM    COSHOW,COLPER       NO, THEN PERSON LEVEL                        
         BNO   SHOWXN                                                           
         BO    SHOWXY                                                           
*                                                                               
SHOWXY   SR    RC,RC                                                            
SHOWXN   LTR   RC,RC                                                            
         XIT1                                                                   
         DROP  R3,R6                                                            
         EJECT                                                                  
***********************************************************************         
*              MAKE DISPLAY TABLE                                     *         
***********************************************************************         
*                                                                               
MKDISTAB NMOD1 0,*MKDTAB*                                                       
         L     RC,SAVERC                                                        
*                                                                               
         GOTO1 =A(GOGETCAP),DMCB,RR=RELO  CALL GETCAP TO FILTER                 
*                                                                               
*---MAKE TABLE OF ALL PROFILES TO BE DISPLAYED                                  
*                                                                               
         LA    R0,DISTABLE          CLEAR DISPLAY TABLE                         
         SR    R1,R1                                                            
         AH    R1,=Y(DISTABQ)                                                   
         LR    RE,R0                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         XC    TABCOUNT,TABCOUNT   COUNT OF TABLE ENTRIES                       
*                                                                               
         LA    R4,DISTABLE          R4 POINTER TO DISPLAY TABLE                 
         USING DISTABD,R4                                                       
*                                                                               
         L     R3,APROFTAB        R3 POINTER TO PROFILE TABLE                   
         USING COPFTABD,R3                                                      
*                                                                               
MD20     DS    0H                                                               
         TM    OPTBIT,OPTSHOW      SHOW ALL PROFILES                            
         BNO   MD20A                                                            
         TM    COFLAG,NOSHOW       DO NOT SHOW THIS PROFILE IN SHOWALL?         
         BO    MD50                YES, DO NOT SHOW                             
         B     MD22                                                             
MD20A    TM    OPTBIT,OPTGRP       SHOW PROFILE FOR A CERTAIN GROUP?            
         BZ    MD21                NO                                           
         CLI   GRPFILT,COTMESHT    IS IT THE 'TIME' GROUP?                      
         BE    MD21                IF SO, NO METHOD IS NEEDED                   
         CLI   GRPFILT,COMCSTSP    OR MCS TIME                                  
         BE    MD21                IF SO, NO METHOD IS NEEDED                   
         CLI   GRPFILT,COMCSTSC    OR MCS T/SHEET COLS                          
         BE    MD21                IF SO, NO METHOD IS NEEDED                   
         CLI   GRPFILT,COMCSEXP    OR MCS EXPENSES                              
         BE    MD21                IF SO, NO METHOD IS NEEDED                   
         LA    R2,PROMETHH                                                      
         CLI   5(R2),0             IS A METHOD PROVIDED                         
         BNE   MD21                  YES, SO CONTINUE                           
         B     ERRMISS               NO, PRINT ERROR MESSAGE                    
         OI    6(R2),X'40'                                                      
MD21     GOTO1 =A(SHOWPROF),DMCB,RR=RELO SHOW PROFILE AT THIS LEVEL?            
         BNE   MD50                NO, NEXT PROFILE                             
*                                                                               
MD22     DS    0H                                                               
         TM    COFLAG,DDSONLY      DDS ONLY PROFILE?                            
         BZ    *+12                                                             
         CLI   TWAOFFC,C'*'        IF DDS SHOW IT                               
         BNE   MD50                                                             
         TM    OPTBIT,OPTGRP       ANY GROUP FILTER                             
         BNO   MD24                                                             
         CLC   GRPFILT,COFILT      SAME GROUP                                   
         BNE   MD50                NO, NEXT PROFILE                             
*                                                                               
MD24     CLI   COCNTRY,CTRYANY     ANY COUNTRY FILTER                           
         BE    MD26                                                             
*        CLI   CTRYCODE,0                                                       
*        BE    MD26                                                             
         TM    COCNTRY,CTRYNOT     DON'T SHOW IN COUNTRY                        
         BO    MD25                                                             
         CLC   COCNTRY,CTRYCODE    ONLY SHOW IN THIS COUNTRY                    
         BNE   MD50                                                             
         B     MD26                                                             
MD25     MVC   BYTE,COCNTRY                                                     
         NI    BYTE,X'FF'-CTRYNOT                                               
         CLC   BYTE,CTRYCODE       DON'T SHOW IN THIS COUNTRY                   
         BE    MD50                                                             
*                                                                               
MD26     MVC   DISNUMB,CONUM       SAVE ENTRY - PROFILE NUMBER                  
         NI    DISFLAG,X'FF'-DIS2ND                                             
         TM    COFLAG,TWOLINEP     IS THIS A 2 LINE PROFILE?                    
         BZ    *+8                                                              
         OI    DISFLAG,DIS2ND      MARK AS SO                                   
*                                                                               
         L     R6,APDSLIST                                                      
         SR    R1,R1                                                            
         ICM   R1,3,COSHORT        DISP TO SHORT DESC                           
         AR    R1,R6                                                            
         MVC   DISSHORT,0(R1)      SHORT DESCRIPTION                            
         MVC   DISDESC,CODESCR     DISP. TO LONG DESCRIPTION                    
*                                                                               
         MVC   DISDRTN,CODRTN      DISP ROUTINE #                               
         CLI   CODRTN,CODNU2Q      DISPLAY ROUTINE 2?                           
         BNE   *+8                                                              
         MVI   DISDRTN,CODNUMQ     SET NUMERIC THEN                             
         CLI   CODRTN,CODML2Q      DISPLAY ROUTINE 2?                           
         BNE   *+8                                                              
         MVI   DISDRTN,CODMLTQ     SET MULTIPLE SETTINGS                        
         MVC   DISVRTN,COVRTN      VALIDATION ROUTINE #                         
         MVC   DISPROT,COPROTCT    PROTECT BIT                                  
         MVC   DISSHOW,COSHOW      SHOW BIT                                     
         MVC   DISVXTRA,COEXTRA                                                 
*                                                                               
         L     R1,COPDESD          ADDR OF PROFILE VALIDATION ENTRY             
         A     R1,RELO                                                          
         STCM  R1,15,DISVADR       SAVE VALIDATION ENTRY ADDR                   
         USING COPDD,R1                                                         
*                                                                               
         MVC   DISOPTS,COPDLIT     DISP. TO PROFILE OPTIONS                     
*                                                                               
         LA    R6,COPTIONS         OPTION BLOCK                                 
         USING COPTION,R6                                                       
*                                                                               
         AH    R6,CODISP                                                        
         MVC   DISFROM,COLEVEL     LEVEL TAKEN FROM                             
         MVC   DISDATE,CODAT       DATE LAST CHANGED                            
         MVC   DISWHO,COWHO        WHO CHANGED IT                               
         MVC   DISSETQ,COSET       1 BYTE OF SETTING EQATE                      
         MVC   DISGRP,COFILT       PROFILE GROUP FILTER                         
*                                                                               
         CLI   CODRTN,CODMVCQ      SETTING IN VALIDATION TABLE                  
         BNE   MD30                                                             
*                                                                               
         SR    R0,R0                                                            
         IC    R0,COPDNUM          NUMBER OF OPTIONS                            
         LA    R2,COPDENTY         MINI ENTRIES                                 
         USING COPDENTY,R2                                                      
MD27     CLC   COPDCOB,COSET                                                    
         BE    MD28                                                             
         LA    R2,COPDMLEN(R2)     NEXT MINI ENTRY                              
         BCT   R0,MD27                                                          
         DC    H'0'                                                             
         DROP  R6                                                               
*                                                                               
MD28     L     R1,APUDSLST                                                      
         SR    R6,R6                                                            
         ICM   R6,3,COPDOPT        DISP TO SETTING                              
         AR    R6,R1                                                            
         MVC   DISSET,0(R6)        OPTION SETTING                               
         B     MD40                                                             
         DROP  R2,R1                                                            
*                                                                               
         USING COPTION,R6                                                       
MD30     DS    0H                                                               
         CLI   CODRTN,CODNUMQ      SETTING IN VALIDATION TABLE                  
         BE    MD32                                                             
         CLI   CODRTN,CODNU2Q                                                   
         BNE   MD35                                                             
         CLC   COSET,SPACES        IS IT SPACES OR PACKED                       
         BE    MD40                                                             
*                                                                               
MD32     CURED (P4,COSET),(8,DISSET),0,ALIGN=LEFT                               
         ORG   *-2                                                              
         MVC   CURPCDEC-CURPARMD(L'CURPCDEC,R1),COEXTRA   N'DPS                 
         NI    CURPCDEC-CURPARMD(R1),DISVNDPQ  SWITCH OFF OTHER BITS            
                                                                                
         BASR  RE,RF                                                            
         B     MD40                                                             
*                                                                               
MD35     SR    R1,R1                                                            
         IC    R1,COBLEN           LENGTH OF SETTING                            
         BCTR  R1,0                                                             
         MVC   DISSET(0),COSET                                                  
         EX    R1,*-6                                                           
*                                                                               
MD40     DS    0H                                                               
         OC    DISSET,SPACES                                                    
         LA    R4,DISTABLN(R4)     BUMP TABLE POINTER                           
         LH    R1,TABCOUNT         COUNT OF TABLE ENTRIES                       
         LA    R1,1(R1)                                                         
         STH   R1,TABCOUNT                                                      
*                                                                               
MD50     LA    R3,COTBLEN(R3)      CHECK NEXT PROFILE                           
         CLI   0(R3),X'00'         END OF TABLE                                 
         BNE   MD20                                                             
         OI    BITS,MADETAB        TABLE HAS BEEN MADE                          
*                                                                               
         XIT1                                                                   
ZEROS    DC    X'00000000'                                                      
         DROP  R3,R6                                                            
         EJECT                                                                  
***********************************************************************         
*    CHECK FOR DUPLICATE SETTINGS FOR MULTIPLE SETTING PROFILES       *         
*    NTRY - P1:  NUMBER OF SETTINGS ALLOWED,A(SCREEN FIELD)                     
*           'SETTING' CONTAINS VALID SETTINGS                                   
***********************************************************************         
*                                                                               
CHKDUPS  NMOD1 0,*CHKDUP*                                                       
         L     RC,SAVERC                                                        
*                                                                               
         ICM   R0,1,0(R1)          # OF ENTRIES TO CHECK FOR                    
         L     R6,0(R1)            ADDRESS OF SETTING INPUT FIELD               
         LA    R2,SETTING          CURRENT SETTING BEING VALIDATED              
*                                                                               
         CLC   0(1,R2),0(R6)                                                    
         BE    CHKDUPN                                                          
         LA    R2,2(R2)            BUMP PAST COMMA INTO NEXT SPOT               
         BCT   R0,*-14                                                          
*                                                                               
CHKDUPY  SR    RC,RC                                                            
CHKDUPN  LTR   RC,RC                                                            
         XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
*    CALLS GETCAP FROM KEY IN AIO                                               
***********************************************************************         
*                                                                               
GOGETCAP NMOD1 0,*GOGTCAP                                                       
         L     RC,SAVERC                                                        
*                                                                               
         LA    R0,COBLOCK          CLEAR COBLOCK                                
         L     R1,=A(COBLOCKX-COBLOCK)                                          
         LR    RE,R0                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         L     R6,AIO              FILL IN BLOCK KEY                            
         USING CAPRECD,R6                                                       
*                                                                               
         MVC   COADM,DATAMGR       PASS A(DATA MANAGER)                         
         MVC   COBKEY(COBKEYLN),SPACES                                          
         MVC   COKCPY,CAPKCPY                                                   
         MVC   COKMTHD,CAPKMTHD                                                 
         MVC   COKOFC(L'CAPKOFC),CAPKOFC                                        
         MVC   COKDPT(L'CAPKDPT),CAPKDPT                                        
         MVC   COKSDT(L'CAPKSDT),CAPKSDT                                        
         MVC   COKPER(L'CAPKPER),CAPKPER                                        
*                                                                               
         TM    OPTBIT,OPTDFLT      DISP DEFAULT LEVELS                          
         BNO   GC10                                                             
         MVI   COSELLEV,COSLVDFT                                                
*                                                                               
GC10     GOTO1 VGETCAP,DMCB,COBLOCK                                             
         CLI   COSTATUS,X'00'      ANY ERRORS                                   
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         TM    OPTBIT,OPTSHOW      IF OPTION SHOWALL                            
         BNO   GCX                                                              
         CLC   CAPKMTHD,SPACES     AND METHOD IS SPECIFIED                      
         BE    GCX                                                              
*                                                                               
         L     R0,AIO3             CLEAR AIO3                                   
         L     R1,=A(COBLOCKX-COBLOCK)                                          
         LR    RE,R0                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
IO3      USING BLOCKSD,R2                                                       
         L     R2,AIO3             USE AIO3                                     
         MVC   IO3.COADM,DATAMGR       PASS A(DATA MANAGER)                     
         MVC   IO3.COBKEY(COBKEYLN),SPACES                                      
         MVC   IO3.COKCPY,CAPKCPY                                               
         MVC   IO3.COKMTHD,SPACES      *** GET NO METHOD PROFILES               
         MVC   IO3.COKOFC(L'CAPKOFC),CAPKOFC                                    
         MVC   IO3.COKDPT(L'CAPKDPT),CAPKDPT                                    
         MVC   IO3.COKSDT(L'CAPKSDT),CAPKSDT                                    
         MVC   IO3.COKPER(L'CAPKPER),CAPKPER                                    
         TM    OPTBIT,OPTDFLT      DISP DEFAULT LEVELS                          
         BNO   GC20                                                             
         MVI   IO3.COSELLEV,COSLVDFT                                            
*                                                                               
GC20     GOTO1 VGETCAP,DMCB,(R2)   USE AIO3                                     
         CLI   IO3.COSTATUS,X'00'      ANY ERRORS                               
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  R6                                                               
*                                                                               
         USING COPFTABD,R6                                                      
         L     R6,APROFTAB        BUMP THRU PROFILE TABLE                       
         B     GC30                                                             
*                                                                               
GC30NX   LA    R6,COTBLEN(R6)                                                   
GC30     CLI   0(R6),0                                                          
         BE    GCX                                                              
*                                                                               
         TM    COSHOW,COLNOMTH     PROFILE VALID FOR NO METHOD                  
         BNO   GC30NX                                                           
*                                                                               
         ZICM  R1,COBLEN,1         LENGTH OF SETTING                            
         LA    R0,COHEDLN          LENGTH OF HEADER                             
         AR    R1,R0                                                            
         SH    R1,=H'1'                                                         
         BM    GC30NX                                                           
         LA    R4,IO3.COPTIONS                                                  
         AH    R4,CODISP           R4=DISP INTO AIO3 OF PROFILE                 
         LA    R3,COPTIONS                                                      
         AH    R3,CODISP           R3=DISP INTO COBLOCK OF PROFILE              
         MVC   0(0,R3),0(R4)     MOVE FROM AIO3 TO COBLOCK                      
         EX    R1,*-6                                                           
         B     GC30NX                                                           
*                                                                               
GCX      XIT1                                                                   
         DROP  R6,IO3                                                           
         EJECT                                                                  
***********************************************************************         
*        UPDATE COUPLED PROFILE                                       *         
***********************************************************************         
*                                                                               
         USING DISTABD,R4                                                       
         USING COUPTABD,R3                                                      
CHKCOUPL NMOD1 0,*CHKCPL*                                                       
         L     RC,SAVERC                                                        
*                                                                               
         L     R3,ACOUPTAB                                                      
CC10     CLI   0(R3),0                                                          
         BE    CCX                                                              
         CLC   DISNUMB,COUPPR1     MATCH CONTROLLING PROFILE                    
         BNE   CC15                                                             
         OC    SETTING,SETTING     IF CONTROLLING PROFILE IS DEFAULT            
         BZ    CC20                THEN DELETE DEPENDANT ELEM                   
         CLC   COUPSET1,SETTING    MATCH CONTROLLING PROFILE                    
         BE    CC20                                                             
CC15     LA    R3,COUPTLEN(R3)                                                  
         B     CC10                                                             
*                                                                               
         USING OPDELD,R6                                                        
CC20     L     R6,AIO              DELETE DEPENDANT PROFILE                     
         MVI   ELCODE,OPDELQ                                                    
         BAS   RE,GETEL                                                         
         B     *+8                                                              
CC30     BAS   RE,NEXTEL                                                        
         BNE   CC40                                                             
         CLC   OPDNUM,COUPPR2                                                   
         BNE   CC30                                                             
         MVI   0(R6),X'FF'         REMOVE ELEMENT                               
         MVI   ELCODE,X'FF'                                                     
         GOTO1 REMELEM                                                          
         OI    BITS,CHANGES                                                     
*                                                                               
CC40     OC    SETTING,SETTING     IF CONTROLLING PROFILE IS DEFAULT            
         BZ    CCX                 THEN DEPENDANT WILL BE DAFAULT               
         CLI   COUPSET2,X'FF'      DEFAULT FOR DEPENDANT ANYWAY                 
         BE    CCX                                                              
*                                                                               
         XC    ELEM,ELEM                                                        
         LA    R6,ELEM                                                          
         MVI   ELCODE,OPDELQ                                                    
         MVI   OPDEL,OPDELQ                                                     
         MVC   OPDNUM,COUPPR2                                                   
         L     R1,ASECBLK                                                       
         USING SECD,R1                                                          
         MVC   OPDPERS,SECPID                                                   
         DROP  R1                                                               
         GOTO1 DATCON,DMCB,(5,0),(1,OPDLAST)                                    
         LA    R1,OPDLN1Q                                                       
         LA    R1,1(R1)            SETTING IS ALWAYS 1                          
         STC   R1,OPDLN                                                         
         MVC   OPDDATA(1),COUPSET2                                              
*                                                                               
         GOTO1 ADDELEM                                                          
         OI    BITS,CHANGES                                                     
*                                                                               
CCX      XIT1                                                                   
         DROP  R3,R4,R6                                                         
         EJECT                                                                  
***********************************************************************         
*        VALIDATE OPTIONS                                            *          
***********************************************************************         
*                                                                               
VALOPTS  NMOD1 0,*VALOPT*                                                       
         L     RC,SAVERC                                                        
*                                                                               
         MVI   OPTBIT,0                                                         
         LA    R2,CONOPTH                                                       
         CLI   5(R2),0                                                          
         BE    VOXIT                                                            
         CLI   ACTEQU,ACTLIST      NO VALID OPTIONS FOR LIST                    
         BE    EINVOPT                                                          
*                                                                               
         XC    BLOCK(200),BLOCK                                                 
*                                                                               
         GOTO1 SCANNER,DMCB,(R2),BLOCK                                          
         CLI   DMCB+4,0                                                         
         BNE   *+8                                                              
         B     EINVOPT                                                          
*                                                                               
         LA    R3,BLOCK                                                         
*                                                                               
         USING VALOPTD,R4                                                       
VO01     L     R4,AOPTSTAB         OPTION TABLE                                 
         CLI   0(R3),1             MUST GIVE AT LEAST 1ST TWO CHAR              
         BNH   EINVOPT             TO DETERMINE THE OPTION                      
*                                                                               
VO02     ZIC   R1,0(R3)            LENGTH                                       
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   VOPTNAME(0),12(R3)                                               
         BE    VO03                                                             
         LA    R4,VOPTLEN(R4)                                                   
         CLI   0(R4),X'00'         EOT                                          
         BNE   VO02                                                             
         B     EINVOPT                                                          
*                                                                               
VO03     OC    OPTBIT,VOPTBIT      TURN ON BIT                                  
         TM    VOPTBIT,OPTGRP      IS THERE A GROUP FILTER                      
         BNO   VO04                                                             
         MVC   GRPFILT,VOPTGRP                                                  
*                                                                               
VO04     LA    R3,32(R3)           NEXT SCANNER LINE                            
         CLI   0(R3),X'00'                                                      
         BNE   VO01                                                             
*                                                                               
         TM    OPTBIT,OPTDFLT      IS DEFAULT OPTION SET                        
         BNO   VOXIT                                                            
         CLI   ACTEQU,ACTDIS       CAN ONLY HAVE WITH ACTION DISP               
         BNE   EDEFDISP                                                         
*                                                                               
VOXIT    XIT1                                                                   
*                                                                               
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
*                TURN OFF DELETE BITS                                           
***********************************************************************         
         SPACE 1                                                                
DELOFF   NTR1  BASE=*,LABEL=*                                                   
         USING CAPRECD,R6                                                       
         L     R6,AIO                                                           
         NI    CAPRSTAT,X'FF'-X'80'                                             
         LA    R6,BIGKEY                                                        
         NI    CAPKSTAT,X'FF'-X'80'                                             
         XIT1                                                                   
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*                           LTORG                                     *         
***********************************************************************         
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*                       CLEAR SOME FIELDS                                       
***********************************************************************         
*                                                                               
CLRSCR   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R2,PROLIN1H         CLEAR ALL UNPROTECTED FIELDS                 
         LA    R3,PROPFKYH                                                      
DRCLR    ZIC   R1,0(R2)            FIELD LENGTH                                 
         SH    R1,=H'9'            8 FOR HEADER, 1 FOR EX                       
         MVC   8(0,R2),SPACES                                                   
         EX    R1,*-6                                                           
         OI    6(R2),X'80'         TRANSMIT                                     
DRCLR5   ZIC   R1,0(R2)                                                         
         AR    R2,R1                                                            
         CR    R2,R3               ARE WE DONE                                  
         BL    DRCLR               NO                                           
*                                                                               
         XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
*                           LTORG                                     *         
***********************************************************************         
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*    CHECKS IF PROFILE IS SET TO SOMETHING (NOT THE DEFAULT)                    
*    CHECKS FOR ELEMENT IN AIO                                                  
*    PROFNUM HAS PROFILE NUMBER TO BE CHECKED                                   
***********************************************************************         
*                                                                               
PROFSET  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R6,AIO                                                           
         USING OPDELD,R6                                                        
         MVI   ELCODE,OPDELQ                                                    
         BAS   RE,GETEL                                                         
*        BE    PS20                                                             
         BNE   PSNO                NO PROFILE SETTING                           
*                                                                               
PS20     CLC   PROFNUM,OPDNUM                                                   
*        BNE   PS10                                                             
         BE    PSYES                                                            
*                                                                               
PS10     BAS   RE,NEXTEL                                                        
         BE    PS20                                                             
         B     PSNO                                                             
*                                                                               
PSYES    SR    RC,RC                                                            
PSNO     LTR   RC,RC                                                            
         XIT1                                                                   
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*                           LTORG                                     *         
***********************************************************************         
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*                 
*                                                                               
BLOCKSD  DSECT                     BLOCKS PUT IN SYSSPARE                       
       ++INCLUDE ACCAPBLOCK        COBLOCK                                      
         EJECT                                                                  
DISTABLE DS    125CL(DISTABLN)                                                  
DISTABQ  EQU   *-DISTABLE                                                       
         EJECT                                                                  
*                                                                               
       ++INCLUDE ACCAPPROFD        DSECTS FOR ACCAPROTAB                        
*                                                                               
* DISPLAY TABLE - HOLDS ALL LINES TO BE DISPLAYED FOR EASY SCROLLING            
*                                                                               
DISTABD  DSECT                                                                  
DISNUMB  DS    XL1                 PROFILE NUMBER                               
DISSHORT DS    CL3                 PROFILE SHORT NAME                           
DISDESC  DS    XL2                 DISP. TO PROFILE DESCRPITION                 
DISPROT  DS    XL1                 PROTECTION BIT                               
DISSHOW  DS    XL1                 SHOW BITS                                    
DISDRTN  DS    XL1                 DISPLAY ROUTINE NUMBER                       
DISVRTN  DS    XL1                 VALIDATION ROUTINE NUMBER                    
DISVADR  DS    XL4                 A(VALIDATION TABLE ENTRY)                    
DISOPTS  DS    XL2                 DISP. TO VALID OPTIONS FOR SETTING           
DISSET   DS    CL8                 SETTING                                      
DISSETQ  DS    CL1                 FIRST BYTE OF SETTING EQAUTE                 
DISGRP   DS    CL1                 PROFILE GROUP FILTER                         
DISFROM  DS    CL1                 LEVEL TAKEN FROM                             
DISDATE  DS    CL3                 DATE LAST CHANGED                            
DISWHO   DS    CL8                 WHO CHANGED IT                               
DISFLAG  DS    XL1                 DISPLAY FLAG                                 
DIS2ND   EQU   X'80'               PROFILE HAS A 2ND LINE OF SETTINGS           
DISIS2ND EQU   X'40'               THIS IS THE 2ND LINE                         
DISVXTRA DS    XL1                 EXTRA VALIDATION INFO                        
DISVNDPQ EQU   COVNDPS             RESERVED FOR N'DPS                           
*                                  VALNUM ROUTINE                               
*                                  RANGE VALIDATION:                            
DISVRNG1 EQU   COVRNG1             1-3                                          
DISVRNG2 EQU   COVRNG2             1-99                                         
DISVRNG3 EQU   COVRNG3             0-99                                         
DISVRNG4 EQU   COVRNG4             0-4                                          
DISVRNG5 EQU   COVRNG5             1-30                                         
*                                  VALNUM2 ROUTINE                              
*                                  RANGE VALIDATION:                            
DISVR2G1 EQU   COVR2G1             1-12                                         
DISVR2G2 EQU   COVR2G2             1-10                                         
DISVR2G3 EQU   COVR2G3             0-365                                        
DISTABLN EQU   *-DISTABD                                                        
*                                                                               
*                                                                               
* COST/INCOME VALIDATION TABLE                                                  
*                                                                               
CITABD   DSECT                                                                  
CITCI    DS    XL1                 COST/INCOME PROFILE EQUATE                   
CITDOA   DS    XL1                 DEPT/OFF/AGY PROFILE EQUATE                  
CITPC    DS    XL1                 PERSON/CLIENT EQUATE                         
CITABLEN EQU   *-CITABD                                                         
*                                                                               
* COUPLED PROFILES TABLE                                                        
*                                                                               
COUPTABD DSECT                                                                  
COUPPR1  DS    XL1                 CONTROLLING PROFILE EQUATE                   
COUPSET1 DS    XL1                 SETTING FOR CONTROLLING PROFILE              
COUPPR2  DS    XL1                 DEPENDANT PROFILE EQUATE                     
COUPSET2 DS    XL1                 DEPENDANT PROFILE SETTING FF=DELETE          
COUPTLEN EQU   *-COUPTABD                                                       
*                                                                               
* DISPLAY AND VALIDATION ROUTINE TABLE                                          
*                                                                               
DVTABD   DSECT                                                                  
DVTABNUM DS    XL1                 DISPLAY ROUTINE NUMBER                       
         DS    XL3                 SPARE                                        
DVTABADR DS    A                   DIS ROUTINE ADDRESS                          
DVTABLEN EQU   *-DVTABD                                                         
*                                                                               
* VALID OPTIONS TABLE DSECT                                                     
*                                                                               
VALOPTD  DSECT                                                                  
VOPTNAME DS    CL8                                                              
VOPTBIT  DS    X                   OPTBIT SETTING                               
VOPTGRP  DS    X                   SETTING FOR GROUP FILTER                     
VOPTLEN  EQU   *-VALOPTD                                                        
*                                                                               
* GROUP FILTER NAME DSECT                                                       
*                                                                               
GRPTBD   DSECT                                                                  
GRPSET   DS    X                   SETTING FOR GROUP FILTER                     
GRPDNME  DS    XL2                 DISP TO GROUP FILTER NME IN PUDSLIST         
GRPLNQ   EQU   *-GRPTBD                                                         
         EJECT                                                                  
***********************************************************************         
*                          INCLUDES                                   *         
***********************************************************************         
*                                                                               
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* DDPERVALD                                                                     
* DDCTRYEQUS                                                                    
* FASECRETD                                                                     
* ACCAPWORKD                                                                    
* ACCAPDSECT                                                                    
* ACBMONVALD                                                                    
* ACGENFILE                                                                     
* CTGENFILE                                                                     
* ACDDEQUS                                                                      
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDPERVALD                                                      
       ++INCLUDE DDCTRYEQUS                                                     
       ++INCLUDE DDSCANBLKD                                                     
       ++INCLUDE FASECRETD                                                      
       ++INCLUDE ACCAPWORKD                                                     
       ++INCLUDE ACCAPDSECT                                                     
       ++INCLUDE ACGENFILE                                                      
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE ACDDEQUS                                                       
       ++INCLUDE DDCUREDITD                                                     
         PRINT ON                                                               
         EJECT                                                                  
***********************************************************************         
*                          SCREENS                                    *         
***********************************************************************         
*                                                                               
       ++INCLUDE ACCAPFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE ACCAPE4D                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE ACCAPE5D                                                       
         EJECT                                                                  
***********************************************************************         
*                   REMAINING WORK AREA                               *         
***********************************************************************         
*                                                                               
RELO     DS    A                                                                
SAVERC   DS    F                                                                
SAVER2   DS    F                                                                
SAVER3   DS    F                                                                
SAVER4   DS    F                                                                
SVADDR   DS    F                                                                
APUDCLST DS    A                                                                
APDCLIST DS    A                                                                
APUDSLST DS    A                                                                
APDSLIST DS    A                                                                
AOPTSTAB DS    A                                                                
AGRPTAB  DS    A                                                                
APROFTAB DS    A                                                                
ACOUPTAB DS    A                                                                
ACITABLE DS    A                                                                
AOFFLTAB DS    A                                                                
AFROMTAB DS    A                                                                
APFTABLE DS    A                                                                
ALPFTBLE DS    A                                                                
LSTDISP  DS    H                   DISPLACEMENT TO LAST DISPLAYED               
STDISP   DS    H                   DISP TO START DISPLAYING                     
PRVSTDSP DS    H                   DISP TO PREVIOUS START FOR PAGE UP           
PROFDISP DS    H                   DISP INTO PROFILE TABLE                      
CODISP1  DS    H                   DISP TO ENTRY IN COBLOCK                     
TABCOUNT DS    H                   COUNT OF TABLE ENTRIES                       
HALFWD   DS    0H                                                               
BYTE1    DS    XL1                                                              
BYTE2    DS    XL1                                                              
*                                                                               
METHNUM  DS    CL1                 METHOD NUMBER                                
METHCODE DS    CL3                 METHOD CODE                                  
*                                                                               
SVMETHCD DS    CL3                 SAVED METHOD CODE                            
SVMETHNM DS    CL1                 SAVED METHOD NUMBER                          
*                                                                               
OFFICE   DS    CL2                 OFFICE CODE                                  
DEPT     DS    CL6                 DEPARTMENT CODE                              
SUBDPT   DS    CL6                 SUB DEPARTMENT CODE                          
PERSON   DS    CL8                 PERSON CODE                                  
*                                                                               
PROFILT  DS    CL3                 PROFILE LIST FILTER                          
GRPFILT  DS    XL1                 GROUP FILTER                                 
SVGRPFLT DS    XL1                                                              
SVGRPOPT DS    XL1                 SAVED GROUP FILTER OPTION                    
SETTING  DS    CL8                 SETTING                                      
SETLEN   DS    XL1                 LENGTH OF SETTING                            
DTIMEST  DS    XL1                 DAILY TIME SETTING                           
MCSTIME  DS    XL1                 MCS TIMESHEETS SETTING                       
PROFNUM  DS    XL1                 PROFILE NUMBER                               
LEDNUM   DS    XL2                 LEDGER                                       
SHRTNAME DS    CL3                                                              
LONGNAME DS    CL8                                                              
PCODE    DS    CL8                 SAVED PERSON CODE                            
LENSTOR  DS    XL1                 L'OFFICE+L'DEPT+L'SUBDEPT                    
SVSTRT   DS    3PL3                SAVED LOCSTART                               
SWDAY    DS    PL3                 SWITCH DAY                                   
MYCHAR   DS    XL1                                                              
*                                                                               
BITS     DS    XL1                                                              
MADETAB  EQU   X'80'               DISPLAY TABLE ALREADY MADE                   
REDISP   EQU   X'40'               REDISPLAY SAME PAGE                          
KEYCHNGE EQU   X'20'               KEY HAS CHANGED REDISPLAY                    
CHANGES  EQU   X'10'               CHANGES HAVE BEEN MADE                       
ONEBYTOF EQU   X'08'               AGY ON ONE BYTE OFFICES                      
FIRST    EQU   X'04'               FIRST TIME THROUGH                           
NEWREC   EQU   X'01'               FLAG TO ADD REC                              
*                                                                               
BITS2    DS    XL1                                                              
DTIMDATQ EQU   X'80'               DATE CHANGE FOR DAILY TIME                   
MCSTDATQ EQU   X'40'               DATE CHANGE FOR MCS TIMESHEETS               
SVOPTBIT DS    XL1                 SAVED AT LAST DISPLAY OPTION BIT             
OPTBIT   DS    XL1                 OPTION BIT                                   
OPTACT   EQU   X'80'               ACTIVITY OPTION                              
OPTSHOW  EQU   X'40'               SHOW ALL PROFILES                            
OPTGRP   EQU   X'20'               GROUP FILTER                                 
OPTDFLT  EQU   X'10'               DEFAULT OPTION                               
*                                                                               
LEVBIT   DS    XL1                                                              
LEVDFLT  EQU   X'80'               DEFAULT LEVEL                                
LEVMETH  EQU   X'40'               METHOD LEVEL                                 
LEVGRP   EQU   X'20'               OFFICE GROUP LEVEL                           
LEVOFF   EQU   X'10'               OFFICE LEVEL                                 
LEVDPT   EQU   X'08'               DEPARTMENT LEVEL                             
LEVSDPT  EQU   X'04'               SUB DEPT LEVEL                               
LEVPER   EQU   X'02'               PERSON LEVEL                                 
LEVNOMTH EQU   X'01'                                                            
*                                                                               
PILOBYT  DS    XL1                                                              
*OCIBYT  EQU   X'80'               LOCATION VERIFIED BIT                        
PIDIBYT  EQU   X'40'               PID VERIFIED BIT                             
LDAYBYT  EQU   X'20'               PID NEEDED IN FUTURE                         
PIDTERM  EQU   X'10'               PID TERMINATED                               
*&&US                                                                           
PIDFRL   EQU   X'08'               PID IS FREELANCER                            
FREELNCR DS    CL1                 PERSON IS A FREELANCER                       
*&&                                                                             
LDGFLG   DS    XL1                 LEDGER INDICATOR                             
PIDV     DS    XL1                 FOR PID VALIDATION                           
CNINES   DS    CL(L'ACTKACT)       9999999999999...                             
*                                                                               
LEVELLN  DS    CL4                 LENGTHS OF ALL LEVELS                        
ACCNT    DS    CL12                MY TEMP ACCOUNT FIELD                        
WAREA1   DS    CL20                                                             
SAVEKEY  DS    XL42                ACCFILE KEY                                  
SVKEY1   DS    XL56                ANOTHER SAVE BIGKEY                          
SVKEY2   DS    XL56                SAVE BIGKEY                                  
KEY2     DS    XL42                ACCFILE KEY                                  
SVKEY3   DS    XL56                                                             
SVKEY4   DS    XL56                                                             
KEY3     DS    XL64                                                             
DSPKEY   DS    XL42                LAST KEY TO BE DISPLAYED                     
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
***********************************************************************         
*                         DSECTS                                      *         
***********************************************************************         
*                                                                               
LSTLINED DSECT                     LIST LINE DSECT                              
LSTSTRT  DS    CL3                                                              
LSTMETH  DS    CL3                 METHOD                                       
         DS    CL8                                                              
LSTOFF   DS    CL2                 OFFICE                                       
         DS    CL9                                                              
LSTDEPT  DS    CL3                 DEPT                                         
         DS    CL6                                                              
LSTSDPT  DS    CL3                 SUB DEPT                                     
         DS    CL9                                                              
LSTPER   DS    CL8                 PERSON                                       
*                                                                               
DSPLINED DSECT                     DISPLAY LINE DSECT                           
DSPDESCH DS    CL8                                                              
DSPSHORT DS    CL3                 PROFILE SHORT NAME                           
DSPHYPH  DS    CL1                 HYPHEN                                       
DSPDESC  DS    CL24                PROFILE DESCRIPTION                          
         ORG   DSPSHORT                                                         
         DS    CL4                                                              
DSPGRPNM DS    CL24                PROFILE GROUP FILTER NAME                    
DSPSETH  DS    CL8                                                              
DSPSET   DS    CL8                 PROFILE SETTING                              
DSPVALH  DS    CL8                                                              
DSPVAL   DS    CL37                PROFILE VALID SETTINGS                       
         ORG   DSPVAL                                                           
         DS    CL8                                                              
DSPWHO   DS    CL8                 WHO LAST CHANGED                             
         DS    CL4                                                              
DSPDATE  DS    CL8                 DATE LAST CHANGED                            
         DS    CL9                                                              
DSPFROMH DS    CL8                                                              
DSPFROM  DS    CL2                 FROM WHAT LEVEL                              
DSPLINLN EQU   *-DSPLINED                                                       
*                                                                               
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'109ACCAP14   07/07/20'                                      
         END                                                                    
