*          DATA SET ACCAP12    AT LEVEL 008 AS OF 10/29/09                      
*PHASE T61D12A                                                                  
*                                                                               
***********************************************************************         
*                                                                     *         
*  TITLE:        ACCAP12 -- DAY HOURS DISPL/CHANGE                    *         
*                                                                     *         
*  COMMENTS:     MAINTAINS DAY HOURS RECORDS                          *         
*                                                                     *         
*  CALLED FROM:  CAP CONTROLLER (T61D00), WHICH CALLS                 *         
*                GEGENCON (T00A30), WHICH CALLS THIS.                 *         
*                                                                     *         
*  INPUTS:       SCREENS ACCAPEA (MAINTENANCE)                        *         
*                                                                     *         
*  OUTPUTS:      UPDATED EDIT HOURS RECS,                             *         
*                                                                     *         
*  REGISTERS:    R0 -- WORK                                           *         
*                R1 -- WORK                                           *         
*                R2 -- SCREEN FIELD HEADER                            *         
*                R3 -- WORK                                           *         
*                R4 -- WORK                                           *         
*                R5 -- WORK                                           *         
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
***********************************************************************         
* NSHE 003 23NOV05 ALLOW USER TO DENOTE HOLIDAYS - STAFF AND PUBLIC             
* NSHE 004 08MAY06 FIX BUG WITH EXCEPTIONS                                      
* NSHE 005 09JAN09 FIX BUG WITH EXCEPTIONS WITH DATE                            
         TITLE 'T61D12 - DAY HOURS MAINT'                                       
T61D12   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**1D12**,R7,RR=R3                                              
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING T61DFFD,RA          BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         ST    R3,RELO                                                          
         GOTO1 DICTATE,DMCB,C'L   ',DCLISTL,DSLISTL                             
         SR    R2,R2                                                            
         LA    R3,DAYPFKYH                                                      
         LA    R2,DPFTABLE         DISP/CHANGE PFKEYS                           
*                                                                               
INITPFKY GOTO1 INITIAL,DMCB,(X'40',(R2)),(R3)   INITIALIZE THE PFKEYS           
*                                                                               
TSTMODES CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VK                                                               
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VR                                                               
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    DK                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DR                                                               
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
         XC    DAYBYTE,DAYBYTE                                                  
         MVC   ACCNT,SPACES                                                     
         MVI   SEQNUM,0                                                         
         MVI   BITS,0                                                           
         MVI   BITS2,0                                                          
         GOTO1 GETLDG,DMCB,C'1R'   GET APPROPRIATE FIELD LENGTHS                
         MVC   LEVELLN,ABCDLEN                                                  
         EJECT                                                                  
***********************************************************************         
*        VALIDATE YEAR                                                          
***********************************************************************         
*                                                                               
VK05     LA    R2,DAYYEARH                                                      
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
         LA    R2,DAYOFFH          OFFICE                                       
         CLI   5(R2),0             ANY DATA?                                    
         BE    VK15                                                             
         CLC   DAYOFFH+5(1),LEVELLN         SHOULD = LEN LEVEL A ACC            
         BH    ETOOLONG                                                         
         BL    ETOOSHRT                                                         
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         MVC   OFFICE(0),DAYOFF    SAVE OFFICE                                  
         EX    R1,*-6                                                           
         MVC   0(0,R4),DAYOFF      BUILD ACCOUNT TO READ 1R                     
         EX    R1,*-6                                                           
         LA    R4,1(R1,R4)                                                      
         OI    BITS,YESOFF                                                      
*                                                                               
VK15     LA    R2,DAYDEPTH         ANY DEPARTMENT                               
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
VK20     LA    R2,DAYSDPTH         ANY SUB DEPT FILTER                          
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
VK25     LA    R2,DAYPERH          ANY PERSON                                   
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
VK26     LA    R2,DAYOFFH          CURSOR AT OFFICE ON ERROR                    
         BAS   RE,VALACCNT         VALIDATE ENTIRE ACCOUNT                      
                                                                                
         LA    R2,DAYDAYH          ANY PERSON                                   
         CLI   5(R2),0                                                          
         BE    ERRMISS                                                          
         LA    R0,7                NUMBER OF DAYS IN WEEK                       
         LA    R3,AC@MON           START AT TOP OF LIST FOR DAYS - SEE          
         LA    R4,DAYTAB                                                        
         ZIC   R1,5(R2)                            GENERAL WORK STORAGE         
         BCTR  R1,0                                                             
         OC    DAYDAY,SPACES                                                    
VK30     EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R3),8(R2)      COMPARE INPUT TO LIST                         
         BE    VK35               EQUAL                                         
         LA    R3,L'AC@MON(R3)    BUMP TO NEXT IN LIST                          
         LA    R4,1(R4)           BUMP TO NEXT ENTRY IN LIST                    
         BCT   R0,VK30            HAVE WE REACHED END - GO BACK IF NOT          
         B     ERRINV             INPUT MUST BE INVALID                         
*                                                                               
VK35     MVC   8(L'AC@MON,R2),0(R3)                                             
         MVI   5(R2),L'AC@MON                                                   
         OI    6(R2),X'80'                                                      
         MVC   DAYBYTE,0(R4)       SAVE THE INDICATOR TO USE IN DED EL          
         EJECT                                                                  
***********************************************************************         
*        READ FOR TIMESHEET CALENDAR RECORD TO DISP                             
***********************************************************************         
*                                                                               
         BAS   RE,GETCAL                                                        
         TM    TRANSTAT,RACHANG    RECORD OR ACTION CHANGE                      
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
         MVI   EDTKTYP,EDTKTYPQ    EDIT HOURS RECORD                            
         MVI   EDTKSUB,EDTKSUBQ                                                 
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
         BE    VK60                                                             
         BAS   RE,CLRSCR           CLEAR SCREEN                                 
*                                                                               
VK60     MVC   SAVEKEY,BIGKEY                                                   
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
         LA    R2,DAYYEARH         FOR ERROR TO POINT TO YEAR                   
         XC    CALSTART,CALSTART   INIT                                         
         XC    CALEND,CALEND                                                    
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
         BNE   GCX                                                              
*                                                                               
         MVC   CALSTART,TMRSTART                                                
         MVC   CALEND,TMREND                                                    
*                                                                               
GCX      MVC   8(4,R2),SPACES      DISPLAY                                      
         MVC   WORK+10(1),CALYEAR                                               
         MVC   WORK+11(2),=X'0101'                                              
         MVI   BYTE,20                                                          
         OI    BYTE,X'20'          DON'T OUTPUT FUNNY YEARS                     
         GOTO1 DATCON,DMCB,(1,WORK+10),(BYTE,WORK)                              
         MVC   8(4,R2),WORK                                                     
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
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'ACCDIR ',KEY2,KEY2,0                  
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
         USING EDTRECD,R6                                                       
         L     R6,AIO                                                           
         NI    EDTRSTAT,X'FF'-X'80'                                             
         LA    R6,BIGKEY                                                        
         NI    EDTKSTAT,X'FF'-X'80'                                             
         B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*        DISPLAYS CALENDAR IN TIMESHEET CAL RECORD                              
***********************************************************************         
*                                                                               
DISPCAL  NTR1                                                                   
         XC    SVMONTH,SVMONTH                                                  
         OI    BITS,CALDISPD       CALENDAR DISPLAYED                           
*                                  CLEAR ALL PROTECTED                          
         LA    R2,DAYCOL1H         FIRST FIELD                                  
         LA    R3,DAYTOTH          LAST FIELD                                   
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
DC12     L     R6,AIO3                                                          
         USING TMPELD,R6                                                        
         MVI   ELCODE,TMPELQ       TIMESHEET PERIODS ELEM                       
         BAS   RE,GETEL                                                         
         B     DC18                                                             
*                                                                               
DC18NX   BAS   RE,NEXTEL                                                        
DC18     BNE   DCX                                                              
         CLI   TMPNUMB,X'01'                                                    
         BNE   DC18NX                                                           
*                                                                               
         LA    R2,DAYCOL1H         COLUMN 1 HEADER                              
DC26     BAS   RE,DISPDAT          DISPLAY NEXT ELEM                            
         BNE   DC28                                                             
         BAS   RE,BUMP8            NEXT IN COLUM                                
DC28     BAS   RE,NEXTEL           NEXT ELEM                                    
         BNE   DCX                                                              
         LA    R1,DAYEND1H                                                      
         CR    R2,R1                                                            
         BNH   DC26                                                             
*                                                                               
         LA    R2,DAYCOL2H         COLUMN 2 HEADER                              
DC30     BAS   RE,DISPDAT          DISP TIMESHEET PERIOD ELEM                   
         BNE   DC32                                                             
         BAS   RE,BUMP8            NEXT IN COLUM                                
DC32     BAS   RE,NEXTEL           NEXT ELEM                                    
         BNE   DCX                                                              
         LA    R1,DAYEND2H                                                      
         CR    R2,R1                                                            
         BNH   DC30                                                             
*                                                                               
         LA    R2,DAYCOL3H         COLUMN 3 HEADER                              
DC34     BAS   RE,DISPDAT          DISP TIMESHEET PERIOD ELEM                   
         BNE   DC38                                                             
         BAS   RE,BUMP8            NEXT IN COLUM                                
DC38     BAS   RE,NEXTEL           NEXT ELEM                                    
         BNE   DCX                                                              
         LA    R1,DAYEND3H                                                      
         CR    R2,R1                                                            
         BNH   DC34                                                             
*                                                                               
         LA    R2,DAYCOL4H         COLUMN 4 HEADER                              
DC40     BAS   RE,DISPDAT          DISP TIMESHEET PERIOD ELEM                   
         BNE   DC42                                                             
         BAS   RE,BUMP8            NEXT IN COLUM                                
DC42     BAS   RE,NEXTEL           NEXT ELEM                                    
         BNE   DCX                                                              
         LA    R1,DAYEND4H                                                      
         CR    R2,R1                                                            
         BNH   DC40                                                             
DCX      ST    R2,SVADDR           SAVE R2 FOR TOTAL LINE                       
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*        DISPDAT - DISPLAYS TIMESHEET DATES FOR A CERTAIN DAY                   
***********************************************************************         
*                                                                               
         USING CALLINED,R2         R2 = SCREEN LINE                             
         USING TMPELD,R6           R6 = ELEMENT                                 
DISPDAT  NTR1                                                                   
         CLC   SVMONTH,TMPMTH+1                                                 
         BE    DP20                IF SAME MONTH DON'T DISPLAY                  
         XC    WORK,WORK           GET MONTH                                    
         MVC   WORK(L'TMPMTH),TMPMTH                                            
         MVI   WORK+2,X'01'        DOESN'T REALLY MATTER                        
         GOTO1 DATCON,DMCB,(1,WORK),(11,MMMYYDD)                                
         MVC   CALMON,MMMYYDD      DISPLAYS MONTH                               
*                                                                               
DP20     GOTO1 DATCON,DMCB,(1,TMPSTART),(0,PSTART)                              
         GOTO1 DATCON,DMCB,(1,TMPEND),(0,PEND)                                  
         MVC   NXTDAY,PSTART                                                    
         B     DP24                                                             
DP22     GOTO1 ADDAY,DMCB,PSTART,NXTDAY,1      ADD ONE DAY                      
DP23     CLC   PEND,NXTDAY         HAVE WE REACHED END OF THIS PERIOD           
         BL    DPXNO              GET NEXT ELEMENT                              
DP24     GOTO1 GETDAY,DMCB,NXTDAY,DUB                                           
         CLC   DUB(3),DAYDAY                                                    
         BE    DP28                                                             
DP26     MVC   PSTART,NXTDAY                                                    
         B     DP22                                                             
*                                                                               
DP28     MVI   BYTE,10             OUTPUT TYPE                                  
         CLI   LANGCODE,3                                                       
         BNE   *+8                                                              
         MVI   BYTE,17             OUTPUT TYPE FOR GERMANY                      
         GOTO1 DATCON,DMCB,(0,NXTDAY),(BYTE,DUB)                                
         MVC   CALDDMM,DUB                                                      
*                                  DISPLAY NUMBER                               
         EDIT  (B1,TMPNUMB),(2,CALNUM),FILL=0                                   
*                                                                               
         MVC   SVMONTH,TMPMTH+1    SAVE MONTH                                   
         GOTO1 ADDAY,DMCB,NXTDAY,PSTART,7                                       
         CLC   PSTART,PEND                                                      
         BH    DPXYES                                                           
         BAS   RE,BUMP8                                                         
         MVC   NXTDAY,PSTART                                                    
         B     DP28                                                             
                                                                                
DPXYES   SR    RC,RC                                                            
DPXNO    LTR   RC,RC                                                            
         XIT1  REGS=(R2)           RETURN R2                                    
         DROP  R2,R6                                                            
         EJECT                                                                  
***********************************************************************         
*        VALIDATE RECORD                                                        
***********************************************************************         
*                                                                               
VR       NI    BITS,X'FF'-LAST                                                  
         NI    BITS,X'FF'-NOTFIRST                                              
         MVI   BITS2,0                                                          
*                                                                               
         XC    BIGKEY,BIGKEY                                                    
         MVC   BIGKEY(L'SAVEKEY),SAVEKEY                                        
         OI    DMINBTS,X'08'       PASS BACK DELETED RECS                       
         GOTO1 HIGH                                                             
         CLC   BIGKEY(L'ACTKEY),KEYSAVE    NEW REC                              
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         USING EDTRECD,R6                                                       
         BAS   RE,DELOFF                                                        
*                                                                               
VR02     BAS   RE,REMYEAR          REMOVE ELEMS FOR YEAR                        
         USING DEDELD,R6                                                        
         L     R6,AIO              GET FIRST ELEMENT                            
         MVI   ELCODE,DEDELQ       FOR NON EXCEPTIONS                           
         BAS   RE,GETEL                                                         
         B     VR08                                                             
VR06     MVI   ELCODE,DEDELQ       HOURS ELEM                                   
         BAS   RE,NEXTEL                                                        
VR08     BNE   VR10                                                             
         CLI   DEDLN,DEDLN1Q       LOOKING FOR STANDARD ELEMENTS                
         BH    VR06                                                             
         CLC   DEDIND,DAYBYTE      AND ONLY ON THE SAME DAY                     
         BNE   VR06                                                             
         ZAP   NONEXHRS,DEDHRS                                                  
*                                                                               
VR10     LA    R2,DAYCOL1H         COLUMN 1 HEADER                              
VR20     BAS   RE,VALHOURS         VALIDATE HOURS                               
         TM    BITS,LAST           ALL DONE                                     
         BO    VR40                                                             
         BAS   RE,BUMP8            NEXT IN COLUM                                
         LA    R1,DAYEND1H                                                      
         CR    R2,R1                                                            
         BNH   VR20                                                             
*                                                                               
         LA    R2,DAYCOL2H         COLUMN 2 HEADER                              
VR25     BAS   RE,VALHOURS         VALIDATE HOURS                               
         TM    BITS,LAST                                                        
         BO    VR40                                                             
         BAS   RE,BUMP8            NEXT IN COLUM                                
         LA    R1,DAYEND2H                                                      
         CR    R2,R1                                                            
         BNH   VR25                                                             
*                                                                               
         LA    R2,DAYCOL3H         COLUMN 3 HEADER                              
VR30     BAS   RE,VALHOURS         VALIDATE HOURS                               
         TM    BITS,LAST                                                        
         BO    VR40                                                             
         BAS   RE,BUMP8            NEXT IN COLUM                                
         LA    R1,DAYEND3H                                                      
         CR    R2,R1                                                            
         BNH   VR30                                                             
*                                                                               
         LA    R2,DAYCOL4H         COLUMN 3 HEADER                              
VR35     BAS   RE,VALHOURS         VALIDATE HOURS                               
         TM    BITS,LAST                                                        
         BO    VR40                                                             
         BAS   RE,BUMP8            NEXT IN COLUM                                
         LA    R1,DAYEND4H                                                      
         CR    R2,R1                                                            
         BNH   VR35                                                             
*                                                                               
         OI    BITS,LAST                                                        
         BAS   RE,VALHOURS         GO ADD LAST ELEM                             
*                                                                               
VR40     MVI   IOOPT,C'Y'                                                       
         L     R6,AIO                                                           
         MVC   BIGKEY(L'ACTKEY),0(R6)                                           
         OI    DMINBTS,X'08'       READ FOR DELETES                             
         GOTO1 HIGH                                                             
         CLC   BIGKEY(L'ACTKEY),KEYSAVE                                         
         BE    *+6                 GO ADD THIS REC                              
         DC    H'0'                                                             
         MVC   AIO,AIO2                                                         
         GOTO1 GETREC                                                           
         BAS   RE,DELOFF                                                        
         L     R6,AIO2                                                          
         CLC   BIGKEY(L'ACTKEY),0(R6)                                           
         BNE   VRX                                                              
         MVC   AIO,AIO1                                                         
VR48     GOTO1 PUTREC                                                           
         GOTO1 WRITE                                                            
*                                                                               
VRX      MVI   IOOPT,C'Y'          ALREADY DID OWN IO                           
         B     DR                                                               
         EJECT                                                                  
***********************************************************************         
*        VALHOURS - VALIDATES HOURS AND ADDS ELEMENTS IF NECESSARY              
***********************************************************************         
*                                                                               
         USING CALLINED,R2         R2 = SCREEN LINE                             
         USING DEDELD,R6           R6 = ELEMENT                                 
VALHOURS NTR1                                                                   
*                                                                               
         CLC   CALDDMM,SPACES                                                   
         BH    VH04                                                             
         OI    BITS,LAST                                                        
         B     VHX                                                              
VH04     GOTO1 DATCON,DMCB,(1,CALSTART),(X'20',PSTART)                          
         GOTO1 DATCON,DMCB,(1,CALEND),(X'20',PEND)                              
         MVC   WORK(2),PSTART                                                   
*&&UK                                                                           
         MVC   WORK+2(2),CALDDMM+3                                              
         MVC   WORK+4(2),CALDDMM                                                
*&&                                                                             
*&&US                                                                           
         MVC   WORK+4(2),CALDDMM+3                                              
         MVC   WORK+2(2),CALDDMM                                                
*&&                                                                             
         MVC   DUB,WORK                                                         
         CLC   WORK(6),PSTART                                                   
         BNL   VH06                                                             
         GOTO1 ADDAY,DMCB,(C'Y',WORK),DUB,1                                     
VH06     GOTO1 DATCON,DMCB,(0,DUB),(1,WORK)                                     
         CLI   CALHOURH+5,0        VALIDATE HOURS                               
         BNE   VH08                                                             
         LA    R2,CALHOURH                                                      
         B     ERRMISS                                                          
*                                                                               
VH08     ZIC   R0,CALHOURH+5                                                    
         CLC   CALHOUR(L'AC@STFHL),AC@STFHL                                     
         BE    *+14                                                             
         CLC   CALHOUR(L'AC@PUBHL),AC@PUBHL                                     
         BNE   VH10                                                             
         SHI   R0,1                                                             
         GOTO1 CASHVAL,DMCB,(X'82',CALHOUR+L'AC@STFHL),(R0)                     
         B     VH12                                                             
VH10     GOTO1 CASHVAL,DMCB,(X'82',CALHOUR),(R0)                                
VH12     CLI   DMCB,X'FF'                                                       
         BE    VHERR                                                            
         ZAP   HOURS(4),DMCB+8(4)                                               
         CP    HOURS(4),=P'24.01'                                               
         BL    VH14                                                             
VHERR    LA    R2,CALHOURH                                                      
         B     EINVHRS                                                          
*                                                                               
VH14     CLC   CALHOUR(L'AC@STFHL),AC@STFHL                                     
         BE    VH20                                                             
         CLC   CALHOUR(L'AC@PUBHL),AC@PUBHL                                     
         BE    VH20                                                             
         CP    HOURS(4),NONEXHRS(4)    SAME HOURS?                              
         BE    VH50                    YES, UPDATE END DATE                     
*                                                                               
VH20     DS    0H                                                               
         XC    ELEM,ELEM                                                        
         LA    R6,ELEM                                                          
         MVI   DEDEL,DEDELQ                                                     
         MVI   DEDLN,DEDLNQ                                                     
         MVC   DEDIND,DAYBYTE                                                   
         MVC   DEDDATE,WORK                                                     
         ZAP   DEDHRS(4),HOURS(4)                                               
         CLC   CALHOUR(L'AC@STFHL),AC@STFHL                                     
         BNE   VH22                                                             
         OI    DEDSTAT,DEDSSHOL                                                 
VH22     CLC   CALHOUR(L'AC@PUBHL),AC@PUBHL                                     
         BNE   VH25                                                             
         OI    DEDSTAT,DEDSPHOL                                                 
*                                                                               
VH25     OI    GENSTAT5,ADDEQCOD                                                
         GOTO1 ADDELEM                                                          
         NI    GENSTAT5,X'FF'-ADDEQCOD                                          
         TM    BITS2,EXCPMRK                                                    
         BNZ   VHX                                                              
         L     R6,AIO                                                           
         MVI   ELCODE,DEDELQ       HOURS ELEM                                   
         BAS   RE,GETEL                                                         
         B     VH40                                                             
*                                                                               
VH30     MVI   ELCODE,DEDELQ       HOURS ELEM                                   
         BAS   RE,NEXTEL                                                        
VH40     BNE   VHX                                                              
         CLI   DEDLN,DEDLN1Q       ONLY LOOKING FOR STANDARD ELEMENTS           
         BNE   VH30                                                             
         CLC   DEDIND,DAYBYTE      AND ONLY ON THE SAME DAY                     
         BNE   VH30                                                             
         OI    DEDSTAT,DEDSEXCP    SET EXCEPTION ELEMENTS EXIST                 
         OI    BITS2,EXCPMRK       SET THAT THIS HAS BEEN DONE                  
*                                                                               
VH50     TM    BITS2,EXCPMRK       HAVE WE FOUND AN EXCEPTION                   
         BNZ   VHX                 YES                                          
         L     R6,AIO              NO - REMOVE INDICATOR                        
         MVI   ELCODE,DEDELQ       HOURS ELEM                                   
         BAS   RE,GETEL                                                         
         B     VH70                                                             
*                                                                               
VH60     MVI   ELCODE,DEDELQ       HOURS ELEM                                   
         BAS   RE,NEXTEL                                                        
VH70     BNE   VHX                                                              
         CLI   DEDLN,DEDLN1Q       ONLY LOOKING FOR STANDARD ELEMENTS           
         BNE   VH60                                                             
         CLC   DEDIND,DAYBYTE      AND ONLY ON THE SAME DAY                     
         BNE   VH60                                                             
         NI    DEDSTAT,X'FF'-DEDSEXCP    REMOVE EXCEPTION INDICATOR             
*                                                                               
VHX      B     XIT                                                              
         DROP  R2,R6                                                            
         EJECT                                                                  
***********************************************************************         
*        REMOVES ALL EXISTING EXCEPTION ELEMENTS FOR THAT DAY                   
***********************************************************************         
*                                                                               
REMYEAR  NTR1                                                                   
         OI    BITS,NOELEMS        NO ELEMS EXIST FOR YEAR                      
RY30     L     R6,AIO                                                           
         USING DEDELD,R6                                                        
         MVI   ELCODE,DEDELQ       HOURS ELEM                                   
         BAS   RE,GETEL                                                         
         B     RY40                                                             
*                                                                               
RYDNX    MVI   ELCODE,DEDELQ       HOURS ELEM                                   
         BAS   RE,NEXTEL                                                        
RY40     BNE   RYX                                                              
         CLI   DEDLN,DEDLN1Q       ONLY LOOKING FOR EXCEPTION ELEMENTS          
         BE    RYDNX                                                            
         CLC   DEDIND,DAYBYTE      AND ONLY ON THE SAME DAY                     
         BNE   RYDNX                                                            
                                                                                
         MVI   0(R6),X'FF'                                                      
         NI    BITS,X'FF'-NOELEMS                                               
         B     RYDNX                                                            
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
         MVC   DAYOFF(L'EDTKOFC),EDTKOFC                                        
         OI    DAYOFFH+6,X'80'                                                  
         MVC   DAYDEPT(L'EDTKDPT),EDTKDPT                                       
         OI    DAYDEPTH+6,X'80'                                                 
         MVC   DAYSDPT(L'EDTKSBD),EDTKSBD                                       
         OI    DAYSDPTH+6,X'80'                                                 
         MVC   DAYPER,EDTKPER                                                   
         OI    DAYPERH+6,X'80'                                                  
*                                                                               
DK10     LA    R2,DAYYEARH                                                      
         XC    WORK,WORK                                                        
         MVC   WORK(1),1(R3)                                                    
         MVC   WORK+1(2),=X'0101'                                               
         GOTO1 DATCON,DMCB,(1,WORK),(23,WORK+10)                                
         MVC   8(4,R2),WORK+10     MOVE IN CHAR YEAR                            
         MVI   5(R2),4             SET LENGTH                                   
*                                                                               
DK20     BAS   RE,VALYR                                                         
         BAS   RE,GETCAL           GET CALENDAR REC                             
         BAS   RE,DISPCAL          DISPLAY CALENDAR                             
DKX      B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*        DISPLAY RECORD                                                         
***********************************************************************         
*                                                                               
DR       BAS   RE,GETCAL                                                        
         BAS   RE,DISPCAL          DISPLAY CALENDAR                             
*                                                                               
         NI    BITS,X'FF'-LAST                                                  
         NI    BITS,X'FF'-NOTFIRST                                              
         BAS   RE,CLRSCR           CLEAR SCREEN                                 
         MVC   DAYTOT,SPACES       CLEAR TOTAL                                  
         OI    DAYTOTH+6,X'80'                                                  
         EJECT                                                                  
***********************************************************************         
*        DISPLAY  ELEMENTS                                                      
***********************************************************************         
*                                                                               
         USING CALLINED,R2         R2 = SCREEN LINE                             
         USING DEDELD,R6                                                        
         L     R6,AIO              GET FIRST ELEMENT                            
         MVI   ELCODE,DEDELQ       FOR NON EXCEPTIONS                           
         BAS   RE,GETEL                                                         
         B     DR04                                                             
DR02     MVI   ELCODE,DEDELQ       HOURS ELEM                                   
         BAS   RE,NEXTEL                                                        
DR04     BNE   DR60                                                             
         CLI   DEDLN,DEDLN1Q       LOOKING FOR STANDARD ELEMENTS                
         BH    DR02                                                             
         CLC   DEDIND,DAYBYTE      AND ONLY ON THE SAME DAY                     
         BNE   DR02                                                             
         ZAP   NONEXHRS,DEDHRS                                                  
         OI    BITS2,NOEXCP                                                     
         TM    DEDSTAT,DEDSEXCP                                                 
         BZ    DR09                                                             
         NI    BITS2,X'FF'-NOEXCP                                               
*                                                                               
         L     R6,AIO              GET FIRST ELEMENT                            
         MVI   ELCODE,DEDELQ       FOR EXCEPTIONS                               
         BAS   RE,GETEL                                                         
         B     DR08                                                             
DR06     BAS   RE,NEXTEL                                                        
DR08     BNE   DR60                                                             
         CLI   DEDLN,DEDLN1Q       LOOKING FOR STANDARD ELEMENTS                
         BE    DR06                                                             
         CLC   DEDIND,DAYBYTE      AND ONLY ON THE SAME DAY                     
         BNE   DR06                                                             
*                                                                               
DR09     LA    R2,DAYCOL1H         COLUMN 1 HEADER                              
DR10     BAS   RE,DISHOURS         DISPLAY  HOURS                               
         TM    BITS,LAST           ALL DONE                                     
         BO    DR60                                                             
         BAS   RE,BUMP8            NEXT IN COLUM                                
         LA    R1,DAYEND1H                                                      
         CR    R2,R1                                                            
         BNH   DR10                                                             
*                                                                               
         LA    R2,DAYCOL2H         COLUMN 2 HEADER                              
DR20     BAS   RE,DISHOURS         DISPLAY  HOURS                               
         TM    BITS,LAST           ALL DONE                                     
         BO    DR60                                                             
         BAS   RE,BUMP8            NEXT IN COLUM                                
         LA    R1,DAYEND2H                                                      
         CR    R2,R1                                                            
         BNH   DR20                                                             
*                                                                               
         LA    R2,DAYCOL3H         COLUMN 3 HEADER                              
DR30     BAS   RE,DISHOURS         DISPLAY  HOURS                               
         TM    BITS,LAST           ALL DONE                                     
         BO    DR60                                                             
         BAS   RE,BUMP8            NEXT IN COLUM                                
         LA    R1,DAYEND3H                                                      
         CR    R2,R1                                                            
         BNH   DR30                                                             
*                                                                               
         LA    R2,DAYCOL4H         COLUMN 4 HEADER                              
DR40     BAS   RE,DISHOURS         DISPLAY  HOURS                               
         TM    BITS,LAST           ALL DONE                                     
         BO    DR60                                                             
         BAS   RE,BUMP8            NEXT IN COLUM                                
         LA    R1,DAYEND4H                                                      
         CR    R2,R1                                                            
         BNH   DR40                                                             
*                                                                               
DR60     L     R2,SVADDR           SAVED R2 IN DISPCAL-TOTAL LINE HERE          
*                                                                               
DRX      XC    CONOPT,CONOPT       ALWAYS CLEAR OPTIONS                         
         OI    CONOPTH+6,X'80'                                                  
*                                                                               
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
*                                                                               
         USING CALLINED,R2         R2 = SCREEN LINE                             
         USING DEDELD,R6           R6 = ELEMENT                                 
DISHOURS NTR1                                                                   
*                                                                               
         CLC   CALDDMM,SPACES                                                   
         BH    DH02                                                             
         OI    BITS,LAST                                                        
         B     DHX                                                              
*                                                                               
DH02     TM    BITS2,NOEXCP                                                     
         BNZ   DH10                                                             
         MVI   BYTE,10             OUTPUT TYPE                                  
         CLI   LANGCODE,3                                                       
         BNE   *+8                                                              
         MVI   BYTE,17             OUTPUT TYPE FOR GERMANY                      
         GOTO1 DATCON,DMCB,(1,DEDDATE),(BYTE,WORK)                              
         CLC   CALDDMM,WORK                                                     
         BNE   DH10                                                             
         TM    DEDSTAT,DEDSSHOL+DEDSPHOL                                        
         BZ    DH08                                                             
         TM    DEDSTAT,DEDSPHOL                                                 
         BZ    DH04                                                             
         MVC   CALHOUR(L'AC@PUBHL),AC@PUBHL                                     
         B     DH06                                                             
DH04     MVC   CALHOUR(L'AC@STFHL),AC@STFHL                                     
DH06     CURED (P4,DEDHRS),(4,CALHOUR+L'AC@STFHL),2                             
         B     DH20                                                             
DH08     CURED (P4,DEDHRS),(5,CALHOUR),2                                        
         B     DH20                                                             
DH10     CURED (P4,NONEXHRS),(5,CALHOUR),2                                      
         B     DHX                                                              
*                                                                               
DH20     DS    0H                                                               
         BAS   RE,NEXTEL                                                        
         BNE   DH30                                                             
         CLI   DEDLN,DEDLNQ        ONLY WANT EXCEPTIONS FOR DISPLAY             
         BNE   DH20                                                             
         CLC   DEDIND,DAYBYTE      AND ONLY ON THE SAME DAY                     
         BNE   DH20                                                             
         B     DHX                                                              
DH30     OI    BITS2,NOEXCP                                                     
*                                                                               
DHX      XIT1  REGS=(R6)           PASS BACK R6                                 
         DROP  R2,R6                                                            
         EJECT                                                                  
***********************************************************************         
*        BUMP8 -   BUMPS 8 FIELDS, TO GET TO NEXT IN COLUMN                     
*        R2 SHOULD BE POINTING TO FIRST HEADER IN COLUMN                        
***********************************************************************         
BUMP8    NTR1                                                                   
         LA    R4,8                                                             
BUMPLP   ZIC   R1,0(R2)                                                         
         AR    R2,R1                                                            
         BCT   R4,BUMPLP                                                        
         XIT1  REGS=(R2)           RETURN R2                                    
         EJECT                                                                  
***********************************************************************         
*        CLEAR SOME FIELDS                                                      
***********************************************************************         
*                                                                               
CLRSCR   NTR1                                                                   
         LA    R2,DAYCOL1H         CLEAR ALL UNPROTECTED FIELDS                 
         LA    R3,DAYTOTH                                                       
                                                                                
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
         DCDDL AC#PUBHL,3                                                       
         DCDDL AC#STFHL,3                                                       
DCLISTLX DC    X'00'                                                            
         EJECT                                                                  
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
                                                                                
***********************************************************************         
*        DAILY EDIT HRS SCREEN PF KEY DEFINITIONS                               
***********************************************************************         
*                                                                               
DPFTABLE DS    0C                                                               
*                                                                               
*        EDIT HRS LIST                                                          
*                                                                               
         DC    AL1(DPF01X-*,01,PFTCPROG,0,0)                                    
         DCDD  AC#REDIT,3                                                       
         DCDD  AC#REDIT,8                                                       
         DCDD  AC#LIST,8                                                        
DPF01X   EQU   *                                                                
*                                                                               
*        EDIT HRS CHANGE                                                        
*                                                                               
         DC    AL1(DPF02X-*,02,PFTCPROG,(DPF02X-DPF02)/KEYLNQ,0)                
         DCDD  AC#CHG,3                                                         
         DCDD  AC#REDIT,8                                                       
         DCDD  AC#CHG,8                                                         
DPF02    DC    AL1(KEYTYTWA,L'DAYYEAR-1),AL2(DAYYEAR-T61DFFD)                   
         DC    AL1(KEYTYTWA,L'DAYOFF-1),AL2(DAYOFF-T61DFFD)                     
         DC    AL1(KEYTYTWA,L'DAYDEPT-1),AL2(DAYDEPT-T61DFFD)                   
         DC    AL1(KEYTYTWA,L'DAYSDPT-1),AL2(DAYSDPT-T61DFFD)                   
         DC    AL1(KEYTYTWA,L'DAYPER-1),AL2(DAYPER-T61DFFD)                     
         DC    AL1(KEYTYGLB,L'AC@YES-1),AL2(AC@YES-STARTSV)                     
DPF02X   EQU   *                                                                
*                                                                               
*        EDIT HRS DISPLAY                                                       
*                                                                               
         DC    AL1(DPF03X-*,03,PFTCPROG,(DPF03X-DPF03)/KEYLNQ,0)                
         DCDD  AC#DSP,3                                                         
         DCDD  AC#REDIT,8                                                       
         DCDD  AC#DSP,8                                                         
DPF03    DC    AL1(KEYTYTWA,L'DAYYEAR-1),AL2(DAYYEAR-T61DFFD)                   
         DC    AL1(KEYTYTWA,L'DAYOFF-1),AL2(DAYOFF-T61DFFD)                     
         DC    AL1(KEYTYTWA,L'DAYDEPT-1),AL2(DAYDEPT-T61DFFD)                   
         DC    AL1(KEYTYTWA,L'DAYSDPT-1),AL2(DAYSDPT-T61DFFD)                   
         DC    AL1(KEYTYTWA,L'DAYPER-1),AL2(DAYPER-T61DFFD)                     
         DC    AL1(KEYTYGLB,L'AC@YES-1),AL2(AC@YES-STARTSV)                     
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
*        DDPERVALD                                                              
*        DDSCANBLKD                                                             
*        ACCAPWORKD                                                             
*        ACCAPDSECT                                                             
*        ACGENFILE                                                              
*        ACDDEQUS                                                               
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDPERVALD                                                      
       ++INCLUDE DDSCANBLKD                                                     
       ++INCLUDE ACCAPWORKD                                                     
       ++INCLUDE ACCAPDSECT                                                     
       ++INCLUDE ACGENFILE                                                      
       ++INCLUDE ACDDEQUS                                                       
       ++INCLUDE DDFLDHDR                                                       
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
       ++INCLUDE ACCAPEAD                                                       
         EJECT                                                                  
***********************************************************************         
*        REMAINING WORK AREA                                                    
***********************************************************************         
*                                                                               
         DS    0D                                                               
RELO     DS    A                                                                
SVADDR   DS    F                                                                
HOURS    DS    PL4                 PERIOD HOURS PWS                             
NONEXHRS DS    PL4                 NON EXCEPTION HOURS                          
SVHOURS  DS    PL4                 PERIOD HOURS PWS                             
BYTE2    DS    XL1                                                              
OFFICE   DS    CL2                 OFFICE CODE                                  
DEPT     DS    CL3                 DEPARTMENT CODE                              
SUBDPT   DS    CL3                 SUB DEPARTMENT CODE                          
PERSON   DS    CL8                 PERSON CODE                                  
DAYBYTE  DS    XL1                 DAY                                          
CALDATE  DS    PL3                 DATE TO GET CALENDAR                         
SVMONTH  DS    PL1                 SAVE MONTH                                   
TWASCR2  DS    XL1                 BOTTOM HALF OF SCREEN PHASE                  
MMMYYDD  DS    CL8                 MMM YY DD FORMAT DATE                        
PSTART   DS    CL6                 PERIOD START YYMMDD                          
PEND     DS    CL6                 PERIOD END YYMMDD                            
NXTDAY   DS    CL6                 NEXT DAY CALCULATED                          
CALSTART DS    PL3                 CALENDAR START PWOS                          
CALEND   DS    PL3                 CALENDAR END PWOS                            
CALYEAR  DS    PL1                 CALENDAR YEAR                                
*                                                                               
OPTSTAT  DS    XL1                 OPTIONS                                      
OPTSREM  EQU   X'80'               REMOVE HOURS FOR YEAR                        
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
BITS2    DS    XL1                                                              
NOEXCP   EQU   X'80'               NO EXCPTION ELEMENTS                         
EXCPMRK  EQU   X'40'               EXCPTION STATUS MARKED ON ELEMENT            
SHOLMRK  EQU   X'20'               STAFF HOLIDAY MARKED                         
PHOLMRK  EQU   X'10'               PUBLIC HOLIDAY MARKED                        
*                                                                               
SEQNUM   DS    XL1                                                              
LEVELLN  DS    CL4                 LENGTHS OF ALL LEVELS                        
ACCNT    DS    CL12                MY TEMP ACCOUNT FIELD                        
CALKEY   DS    XL70                KEY TO CALENDAR REC                          
SAVEKEY  DS    XL70                ACCFILE KEY                                  
KEY2     DS    XL70                ACCFILE KEY                                  
DSLISTL  DS    0X                                                               
AC@PUBHL DS    CL1,CL2                                                          
AC@STFHL DS    CL1,CL2                                                          
         EJECT                                                                  
***********************************************************************         
*        DSECTS                                                                 
***********************************************************************         
*                                                                               
CALLINED DSECT                     CALENDAR LINE DSECT                          
CALHEAD  DS    CL8                                                              
CALMON   DS    CL3                 MONTH                                        
         DS    CL1                                                              
CALNUM   DS    CL2                 PERIOD NUMBER                                
         DS    CL1                                                              
CALDDMM  DS    CL5                 START DATE                                   
         ORG   CALHEAD                                                          
TOTHEAD  DS    CL8                                                              
TOTWORD  DS    CL6                                                              
TOTHOURS DS    CL6                 TOTAL HOURS                                  
*                                                                               
CALHOURH DS    CL8                 HOURS HEADER                                 
CALHOUR  DS    CL5                 HOURS                                        
CALLINEL EQU   *-CALLINED                                                       
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'008ACCAP12   10/29/09'                                      
         END                                                                    
