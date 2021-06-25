*          DATA SET CTSFM09    AT LEVEL 017 AS OF 05/01/02                      
*PHASE TA0A09A                                                                  
*                                                                               
***********************************************************************         
*                                                                     *         
*  TITLE: TA0A09 - BROADCAST MESSAGE MAINTENANCE/LIST                 *         
*                                                                     *         
*  CALLED FROM: SFM CONTROLLER (TA0A00), WHICH CALLS                  *         
*               DDGENCON (T00A30) WHICH CALLS THIS.                   *         
*                                                                     *         
*  INPUTS: SCREENS CTSFMF9 (TA0AF9) -- MAINTENANCE                    *         
*                  CTSFME9 (TA0AE9) -- LIST                           *         
*                                                                     *         
*  OUTPUTS: UPDATED BROADCAST MESSAGE RECORDS                         *         
*                                                                     *         
*  LOCALS: REGISTER USAGE                                             *         
*          R0 - WORK                                                  *         
*          R1 - WORK                                                  *         
*          R2 - POINTER TO SCREEN FLDH, ERREX WILL POSITION CURSOR    *         
*          R3 - GETEL REGISTER                                        *         
*          R4 - BROADCAST MESSAGE RECORD                              *         
*          R5 - WORK                                                  *         
*          R6 - WORK                                                  *         
*          R7 - WORK                                                  *         
*          R8 - SECOND BASE                                           *         
*          R9 - SYSD                                                  *         
*          RA - TWA                                                   *         
*          RB - FIRST BASE                                            *         
*          RC - GEND                                                  *         
*          RD - SYSTEM                                                *         
*          RE - SYSTEM                                                *         
*          RF - SYSTEM                                                *         
*                                                                     *         
***********************************************************************         
* MOD LOG:                                                            *         
* --------                                                            *         
*                                                                     *         
* 21APR92  (EFJ) --- URGENCY FIELD REMOVED, DAYS FIELDS ADDED         *         
*                --- BRDDAYS CHANGED TO BRDFDAYS                      *         
*                --- IMPLICIT LENGTH FOR LUID FIELD (GARBAGE ON SCRN) *         
*                --- CHANGES TO LIST SCREEN - FILTER ON ALL FLDS      *         
*                                                                     *         
* 23APR92  (EFJ) --- ADD REPS AND INTERVAL FIELDS TO ALLOW MULT ADDS  *         
*                                                                     *         
* 28AUG92  (EFJ) --- FIX END TIME BUG WHEN USING REPS AND INTERVALS   *         
*                                                                     *         
***********************************************************************         
*                                                                               
         TITLE 'TA0A09 BROADCAST MESSAGE MAINTENANCE/LIST'                      
TA0A09   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*TA0A09*,R8                                                    
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
*                                                                               
         L     R1,SYSPARMS         A(PARAMS)                                    
         L     R1,0(R1)            A(SYSFACS)                                   
         USING SYSFACD,R1                                                       
         MVC   ASELIST,VSELIST     A(SYSTEM EXECUTIVE LIST)                     
         L     RF,VSSB             A(SSB)                                       
         MVC   AFACIDTB,SSBAFID-SSBD(RF)  A(FACIDTAB)                           
         DROP  R1                                                               
*                                                                               
         GOTO1 GETFACT,DMCB,0      GET A(FACTSD)                                
         L     R1,0(R1)                                                         
         USING FACTSD,R1                                                        
         MVC   ASYSLST,FASYSLST    A(TABLE OF FACPAK SYSTEMS)                   
         MVC   ACNTRY,FAACTRY      A(COUNTRY TABLE)                             
         MVC   COUNTRY,FACTRY      COUNTRY CODE OF CONNECTED TERMINAL           
         DROP  R1                                                               
*                                                                               
         L     R1,ACOMFACS         A(COMFACS)                                   
         USING COMFACSD,R1         GET ADDRESS OF DEJAVU                        
         MVC   VDEJAVU,CDEJAVU                                                  
         DROP  R1                                                               
*                                                                               
         CLI   MODE,VALKEY         VALIDATE KEY                                 
         BE    VK                                                               
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VR                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DR                                                               
         CLI   MODE,XRECADD        AFTER ADDREC                                 
         BE    XRA                                                              
         CLI   MODE,XRECPUT        AFTER PUTREC                                 
         BE    XRP                                                              
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    DK                                                               
         CLI   MODE,LISTRECS       ONLINE LIST RECORDS                          
         BE    LR                                                               
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
* VALIDATE KEY ROUTINE                                                          
*                                                                               
VK       LA    R4,KEY                                                           
         USING BRDKEYD,R4                                                       
*                                                                               
         LA    R2,SFMTYPEH         MESSAGE TYPE                                 
         CLI   ACTNUM,ACTLIST      ACTION LIST?                                 
         BNE   *+8                                                              
         LA    R2,SFLTYPEH                                                      
         CLI   ACTNUM,ACTREP       ACTION REPORT?                               
         BNE   *+8                                                              
         LA    R2,SFLTYPEH                                                      
*                                                                               
         CLI   5(R2),0             ANY DATA?                                    
         BNE   *+14                                                             
         MVC   GERROR,=AL2(MISSING)                                             
         B     SFMERROR            REQUIRED                                     
*                                                                               
         MVC   MSGTYPE,8(R2)                                                    
         CLI   8(R2),C'P'          PERMANENT MESSAGE?                           
         BE    VK10                YES                                          
         CLI   8(R2),C'T'          TEMPORARY MESSAGE?                           
         BE    VK10                YES                                          
         MVC   GERROR,=AL2(INVTYPE)                                             
         B     SFMERROR            INVALID MESSAGE TYPE                         
*                                                                               
VK10     LA    R2,SFMMSGNH         MESSAGE NUMBER                               
         CLI   ACTNUM,ACTADD       ACTION ADD?                                  
         BNE   VK20                NO -- MUST VALIDATE THE NUMBER               
*                                                                               
         XC    KEY,KEY             READ HIGH MESSAGE NUMBER RECORD              
         MVI   BRDKSYS,BRDKSYSQ    BROADCAST MESSAGE RECORD                     
         MVI   BRDKSTYP,BRDKSTYQ                                                
         MVC   BRDKTYPE,MSGTYPE    MESSAGE TYPE                                 
         GOTO1 HIGH                                                             
         CLC   KEY(32),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                THIS RECORD MUST BE THERE                    
*                                                                               
         GOTO1 GETREC              GET HIGH MESSAGE RECORD                      
         L     R3,AIO                                                           
         MVI   ELCODE,BRDHIGEQ     HIGH MESSAGE ELEMENT                         
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING BRDHIGHD,R3                                                      
         SR    R1,R1                                                            
         ICM   R1,3,BRDHIGNO       THE HIGHEST MESSAGE NUMBER                   
         LA    R1,1(R1)            INCREMENT                                    
         STH   R1,MSGNUM           THE NEW MESSAGE NUMBER                       
         STH   R1,MSG1ST           KEEP TRACK OF 1ST MESSAGE ADDED              
         DROP  R3                                                               
*                                                                               
         LH    R0,MSGNUM                                                        
         EDIT  (R0),(5,SFMMSGN),ALIGN=LEFT                                      
         OI    SFMMSGNH+6,X'80'    XMIT THE NEW NUMBER                          
         B     VK30                                                             
*                                                                               
VK20     XC    MSGNUM,MSGNUM                                                    
         CLI   ACTNUM,ACTLIST      ACTION LIST?                                 
         BNE   *+16                NO -- SEE IF IT'S A REPORT                   
         LA    R2,SFLMSGNH                                                      
         CLI   5(R2),0                                                          
         BE    VK30                NOT REQUIRED FOR LIST                        
*                                                                               
         CLI   ACTNUM,ACTREP       ACTION REPORT?                               
         BNE   *+16                NO -- IT'S REQUIRED                          
         LA    R2,SFLMSGNH                                                      
         CLI   5(R2),0                                                          
         BE    VK30                NOT REQUIRED FOR REPORT                      
*                                                                               
         CLI   5(R2),0             REQUIRED FOR DIS/CHA/REST                    
         BNE   *+14                                                             
         MVC   GERROR,=AL2(MISSING)                                             
         B     SFMERROR                                                         
*                                                                               
         TM    4(R2),X'08'         NUMERIC?                                     
         BO    *+14                YES                                          
         MVC   GERROR,=AL2(NOTNUM)                                              
         B     SFMERROR                                                         
*                                                                               
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R2)                                                      
         CVB   R1,DUB                                                           
         LTR   R1,R1                                                            
         BNZ   *+14                                                             
         MVC   GERROR,=AL2(INVALID)                                             
         B     SFMERROR                                                         
         STH   R1,MSGNUM           SAVE MESSAGE NUMBER                          
*                                                                               
VK30     CLI   ACTNUM,ACTLIST      LIST?                                        
         BE    *+12                YES                                          
         CLI   ACTNUM,ACTREP       REPORT?                                      
         BNE   VKX                 NO                                           
*                                                                               
         XC    FILTERS,FILTERS     BUILD FILTER LIST                            
         LA    R2,SFLNAMEH                                                      
         CLI   5(R2),0             ANY FILTER GIVEN?                            
         BE    VK60                NO                                           
         LA    R1,8(R2)                                                         
         LA    R3,FILTERS          A(FILTER LIST)                               
*                                                                               
VK40     CLI   0(R1),C' '          END OF FILTER EXPRESSION?                    
         BNH   VK60                YES                                          
*                                                                               
         CLI   0(R1),C'*'          WILDCARD?                                    
         BNE   *+12                NO                                           
         MVI   0(R3),C'*'          PUT '*' INTO FILTERS                         
         B     VK50                BRANCH MASK IS ALREADY X'00'                 
*                                                                               
         MVI   1(R3),X'70'         ASSUME IT'S A POSITIVE FILTER                
         CLI   0(R1),C'-'          NEGATIVE FILTER?                             
         BNE   *+18                NO                                           
         LA    R1,1(R1)            BUMP PAST MINUS                              
         MVC   0(1,R3),0(R1)       SAVE NEGATIVE FILTER                         
         MVI   1(R3),X'80'         MASK - DO A 'BRANCH EQUAL' LATER             
*                                                                               
         CLI   0(R1),C'A'          ALPHANUMERIC?                                
         BNL   *+14                YES                                          
         MVC   GERROR,=AL2(INVFILTE)                                            
         B     SFMERROR                                                         
         MVC   0(1,R3),0(R1)       SAVE FILTER                                  
*                                                                               
VK50     LA    R1,1(R1)            BUMP PAST FILTER                             
         LA    R3,2(R3)            POINT TO NEXT TABLE ENTRY                    
         B     VK40                                                             
*                                                                               
VK60     MVI   APPLID,0                                                         
         LA    R2,SFLAPPLH         APPL-ID FIELD                                
         OC    SFLAPPL,MYSPACES    PAD WITH SPACES                              
         CLI   5(R2),0                                                          
         BE    VK90                NO APPL-ID FILTER                            
*                                                                               
         L     R5,AFACIDTB         TABLE OF APPLICATION-IDS                     
         USING FACITABD,R5                                                      
VK70     CLC   SFLAPPL,FACISN4     MATCH ON NAME?                               
         BE    VK80                YES                                          
         LA    R5,L'FACITAB(R5)    NO -- TRY NEXT ENTRY                         
         CLI   0(R5),X'FF'         END OF TABLE?                                
         BNE   VK70                                                             
         MVC   GERROR,=AL2(INVAPPL)                                             
         B     SFMERROR                                                         
*                                                                               
VK80     MVC   APPLID,FACIID       SAVE APPLICATION NUMBER                      
         DROP  R5                                                               
*                                                                               
VK90     CLI   ACTNUM,ACTREP       REPORT?                                      
         BE    VK200                                                            
*                                                                               
         MVI   SYSNUM,0                                                         
         MVI   OVSYSNUM,0                                                       
         LA    R2,SFLSYSH          SYSTEM FIELD                                 
         OC    SFLSYS,MYSPACES     PAD WITH SPACES                              
         CLI   5(R2),0                                                          
         BE    VK100               NO SYSTEM FILTER                             
         CLC   SFLSYS,=C'ALL    '                                               
*                                                                               
         L     R5,ASELIST          A(SYSTEM EXECUTIVE LIST)                     
         USING SELISTD,R5                                                       
         LH    R6,0(R5)            LENGTH OF TABLE ENTRY                        
         L     R7,2(R5)            A(END OF TABLE)                              
         LA    R5,6(R5)            A(FIRST ENTRY)                               
*                                                                               
         CLC   SENAME,SFLSYS       MATCH ON SPECIFIC NAME?                      
         BE    *+12                YES                                          
         BXLE  R5,R6,*-10          NO -- TRY NEXT ENTRY                         
         B     VK95                                                             
         MVC   SYSNUM,SESYS        SYSTEM NUMBER                                
         MVC   OVSYSNUM,SEOVSYS    SYSTEM OVERLAY NUMBER                        
         MVC   ASEPGMS,SEPGMS      A(PROGRAMS TABLE)                            
         B     VK100                                                            
         DROP  R5                                                               
*                                                                               
VK95     L     R5,ASYSLST                                                       
         USING SYSLSTD,R5                                                       
         LH    R6,0(R5)            LENGTH OF TABLE ENTRY                        
         L     R7,2(R5)            A(END OF TABLE)                              
         LA    R5,6(R5)            A(FIRST ENTRY)                               
*                                                                               
         CLC   SYSLNAME,SFLSYS     MATCH ON GENERIC SYSTEM?                     
         BE    *+18                YES                                          
         BXLE  R5,R6,*-10          NO -- TRY NEXT ENTRY                         
         MVC   GERROR,=AL2(INVSYS)                                              
         B     SFMERROR            INVALID SYSTEM NAME                          
         MVC   OVSYSNUM,SYSLNUM    SYSTEM OVERLAY NUMBER                        
         DROP  R5                                                               
*                                                                               
         L     R5,ASELIST          A(SYSTEM EXECUTIVE LIST)                     
         USING SELISTD,R5                                                       
         LH    R6,0(R5)            LENGTH OF TABLE ENTRY                        
         L     R7,2(R5)            A(END OF TABLE)                              
         LA    R5,6(R5)            A(FIRST ENTRY)                               
         CLC   SEOVSYS,OVSYSNUM    FIND FIRST ENTRY FOR THIS SYSTEM             
         BE    *+10                                                             
         BXLE  R5,R6,*-10          TRY NEXT ENTRY                               
         DC    H'0'                                                             
         MVC   ASEPGMS,SEPGMS      A(PROGRAM NAME LIST)                         
         DROP  R5                                                               
*                                                                               
VK100    MVI   PROG,0                                                           
         LA    R2,SFLPROGH         PROGRAM NAME                                 
         OC    SFLPROG,MYSPACES    PAD WITH SPACES                              
         CLI   5(R2),0             ANY INPUT?                                   
         BE    VK110               YES                                          
*                                                                               
         CLC   SFLPROG,=C'ALL    ' ALL PROGRAMS?                                
         BE    VR50                YES                                          
         CLI   OVSYSNUM,0          ALL SYSTEMS?                                 
         BNE   *+14                NO                                           
         MVC   GERROR,=AL2(INVALID)                                             
         B     SFMERROR            PROGRAM ONLY VALID WITHIN A SYSTEM           
*                                                                               
         L     R5,ASEPGMS          A(PROGRAMS LIST)                             
         USING PGMLSTD,R5                                                       
         LH    R6,0(R5)            LENGTH OF TABLE ENTRY                        
         L     R7,2(R5)            A(END OF TABLE)                              
         LA    R5,6(R5)            A(FIRST ENTRY)                               
         ZIC   R1,5(R2)            INPUT LENGTH                                 
         BCTR  R1,0                FOR EXECUTED COMPARE                         
*                                                                               
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   SFLPROG(0),PGMNAME  MATCH ON PROGRAM NAME?                       
         BE    *+18                YES                                          
         BXLE  R5,R6,*-18          TRY NEXT TABLE ENTRY                         
         MVC   GERROR,=AL2(INVPROG)                                             
         B     SFMERROR            PROGRAM NAME NOT IN TABLE                    
         MVC   PROG,PGMNUM         SAVE PROGRAM NUMBER                          
         DROP  R5                                                               
*                                                                               
VK110    XC    STARTDAT,STARTDAT   CLEAR START AND END DATE FILTERS             
         XC    ENDDATE,ENDDATE                                                  
         LA    R2,SFLDATEH         DATE FIELD                                   
         XC    WORK,WORK                                                        
         GOTO1 PERVAL,DMCB,(SFLDATEH+5,SFLDATE),WORK                            
         MVC   MYFLAG,DMCB+4                                                    
*                                                                               
         CLI   DMCB+4,PVRCMISS     NO DATE INPUT?                               
         BE    VK120               RIGHT -- NO DATE FILTER                      
         CLI   MSGTYPE,C'P'        PERMANENT MESSAGE?                           
         BNE   *+14                NO                                           
         MVC   GERROR,=AL2(NOTINPER)                                            
         B     SFMERROR            PERMANENT MESSAGES CAN'T HAVE DATES          
*                                                                               
         CLI   DMCB+4,PVRCINV1     DATE 1 INVALID?                              
         BNE   *+14                NO                                           
         MVC   GERROR,=AL2(INVDATE)                                             
         B     SFMERROR                                                         
         CLI   DMCB+4,PVRCINV2     DATE 2 INVALID?                              
         BNE   *+14                NO                                           
         MVC   GERROR,=AL2(INVDATE)                                             
         B     SFMERROR                                                         
*                                                                               
         LA    R5,WORK             PERVAL OUTPUT AREA                           
         USING PERVALD,R5                                                       
         MVC   STARTDAT,PVALCSTA   SAVE COMPRESSED START DATE                   
         MVC   ENDDATE,PVALCEND    SAVE COMPRESSED END DATE                     
         DROP  R5                                                               
*                                                                               
VK120    XC    LUID,LUID           CLEAR FILTER                                 
         LA    R2,SFLLUIDH         LIST SCREEN LUID FIELD                       
         CLI   5(R2),8             8 CHARACTER MAX                              
         BNH   *+14                                                             
         MVC   GERROR,=AL2(INVALID)                                             
         B     SFMERROR                                                         
         CLC   =C'ALL',SFLLUID     'ALL' LUIDS?                                 
         BE    *+10                YES -- THERE IS NO LUID FILTER               
         MVC   LUID,SFLLUID        VTAM LUID                                    
*                                                                               
         LA    R2,SFLCTRYH         COUNTRY FIELD                                
         OC    SFLCTRY,MYSPACES    PAD WITH SPACES                              
         MVI   CNTRY,0             SET TO NO COUNTRY FILTER                     
         CLI   5(R2),0                                                          
         BE    VK160                                                            
*                                                                               
VK130    CLC   =C'ALL',SFLCTRY     LIST ALL COUNTRIES?                          
         BNE   *+12                                                             
         MVI   CNTRY,X'FF'         ALL COUNTRIES                                
         B     VK160                                                            
*                                                                               
         L     R5,ACNTRY           A(COUNTRY TABLE)                             
         USING CTRYTABD,R5                                                      
         LH    R6,0(R5)            LENGTH OF TABLE ENTRY                        
         L     R7,2(R5)            A(END OF TABLE)                              
         LA    R5,6(R5)            A(FIRST ENTRY)                               
*                                                                               
         CLC   SFLCTRY,CTRYSHR     MATCH ON SHORT COUNTRY NAME?                 
         BE    *+18                YES                                          
         BXLE  R5,R6,*-10          NO -- TRY NEXT ENTRY                         
         MVC   GERROR,=AL2(INVCNTRY)                                            
         B     SFMERROR                                                         
         MVC   CNTRY,CTRYCODE      COUNTRY CODE                                 
         DROP  R5                                                               
*                                                                               
VK160    LA    R2,SFLTIMEH         TIME                                         
         XC    STTM,STTM           CLEAR START TIME FILTER                      
         XC    ENTM,ENTM           CLEAR END TIME FILTER                        
         CLI   5(R2),0                                                          
         BE    VKX                 NOT GIVEN -- IT'S EFFECTIVE ALL DAY          
         CLI   MSGTYPE,C'P'        PERMANENT MESSAGE?                           
         BNE   *+14                NO                                           
         MVC   GERROR,=AL2(NOTINPER)                                            
         B     SFMERROR            PERMANENT MESSAGES CAN'T HAVE TIMES          
*                                                                               
         TM    4(R2),X'08'         IS FIELD NUMERIC?                            
         BZ    VK185               NO -- IT'S A RANGE OF TIMES                  
         ZIC   RE,5(R2)            IT'S THE NUMBER OF EFFECTIVE MINUTES         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R2)                                                      
         CVB   RE,DUB                                                           
         LTR   RE,RE                                                            
         BNZ   *+14                                                             
         MVC   GERROR,=AL2(INVALID)                                             
         B     SFMERROR                                                         
         CH    RE,=H'960'          16 HOUR MAXIMUM RANGE (960 MINUTES)          
         BNH   *+14                                                             
         MVC   GERROR,=AL2(INVALID)                                             
         B     SFMERROR                                                         
         MH    RE,=H'60'           RE = NUMBER OF SECONDS ACTIVE                
*                                                                               
         TBIN  DDSTIME=YES         R1 = START TIME IN SECONDS                   
*&&US*&& AR    R0,R1               ADJUST FOR U.S. CLOCK DIFFERENCE             
         AR    RE,R0               RE = END TIME IN SECONDS                     
         SRDL  R0,32               PREPARE FOR DIVIDE                           
         SRDL  RE,32                                                            
         D     R0,=F'60'           CONVERT TO MINUTES                           
         D     RE,=F'60'                                                        
         SR    R0,R0               PREPARE FOR DIVIDE                           
         SR    RE,RE                                                            
         D     R0,=F'60'           R1 = START HOURS, R0 = START MINUTES         
         D     RE,=F'60'           RF = END HOURS, RE = END MINUTES             
         STC   R1,STTM             START HOURS                                  
         STC   R0,STTM+1           START MINUTES                                
         STC   RF,ENTM             END HOURS                                    
         STC   RE,ENTM+1           END MINUTES                                  
         B     VKX                                                              
*                                                                               
VK185    ZIC   R5,5(R2)                                                         
         GOTO1 TIMVAL,DMCB,((R5),SFLTIME),FULL                                  
         CLI   DMCB,X'FF'                                                       
         BNE   *+14                                                             
         MVC   GERROR,=AL2(INVTIME)                                             
         B     SFMERROR                                                         
*        OC    FULL+2(2),FULL+2    END TIME GIVEN?                              
*        BNZ   *+14                                                             
*        MVC   GERROR,=AL2(NOENDTM)                                             
*        B     SFMERROR            START/END RANGE REQUIRED                     
*                                                                               
         SR    RE,RE                                                            
         LH    RF,FULL             START TIME                                   
         D     RE,=F'100'                                                       
         STC   RF,STTM             HOURS                                        
         STC   RE,STTM+1           MINUTES                                      
         SR    RE,RE                                                            
         OC    FULL+2(2),FULL+2    END TIME GIVEN?                              
         BNZ   *+14                                                             
         MVC   ENTM,STTM           NO, SET END TIME=START TIME                  
         B     VKX                                                              
*                                                                               
         LH    RF,FULL+2           END TIME                                     
         D     RE,=F'100'                                                       
         STC   RF,ENTM             HOURS                                        
         STC   RE,ENTM+1           MINUTES                                      
         B     VKX                                                              
*                                                                               
VK200    LA    R2,SFRDATEH                                                      
         CLI   5(R2),0                                                          
         BE    VKX                                                              
*                                                                               
         GOTO1 PERVAL,DMCB,(SFRDATEH+5,SFRDATE),(X'20',WORK)                    
         TM    DMCB+4,PVRCONE      TEST ONE DATE ONLY                           
         BO    VKX                                                              
         CLI   DMCB+4,PVRCINV1     DATE 1 INVALID?                              
         BNE   *+14                NO                                           
         MVC   GERROR,=AL2(INVDATE)                                             
         B     SFMERROR                                                         
         CLI   DMCB+4,PVRCINV2     DATE 2 INVALID?                              
         BNE   *+14                NO                                           
         MVC   GERROR,=AL2(INVDATE)                                             
         B     SFMERROR                                                         
*                                                                               
VKX      XC    KEY,KEY             BUILD KEY                                    
         MVI   BRDKSYS,BRDKSYSQ    BROADCAST MESSAGE RECORD                     
         MVI   BRDKSTYP,BRDKSTYQ                                                
         MVC   BRDKTYPE,MSGTYPE    MESSAGE TYPE                                 
         MVC   BRDKMSGN,MSGNUM     MESSAGE NUMBER                               
*                                                                               
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
* VALIDATE RECORD ROUTINE                                                       
*                                                                               
VR       L     R4,AIO              A(BROADCAST MESSAGE RECORD)                  
         USING BRDKEY,R4                                                        
*                                                                               
         XC    BRDSTAT+1(3),BRDSTAT+1  CLEAR APPL-ID AND DATE SAVE AREA         
*                                                                               
         MVI   ELCODE,BRDFLTCQ     FILTER ELEMENT                               
         GOTO1 REMELEM                                                          
*                                                                               
         XC    ELEM,ELEM                                                        
         LA    R3,ELEM                                                          
         USING BRDFLTD,R3                                                       
         MVI   BRDFLTC,BRDFLTCQ    ELEMENT CODE                                 
         MVI   BRDFLTL,BRDFLTLQ    ELEMENT LENGTH                               
*                                                                               
         LA    R2,SFMNAMEH         NAME FIELD                                   
         CLI   5(R2),0             REQUIRED                                     
         BNE   *+14                                                             
         MVC   GERROR,=AL2(MISSING)                                             
         B     SFMERROR                                                         
         MVC   BRDFNAME,SFMNAME                                                 
*                                                                               
         MVI   APPLTEST,C'N'       ASSUME IT'S NOT A TEST FACPAK                
         LA    R2,SFMAPPLH         APPL-ID FIELD                                
         OC    SFMAPPL,MYSPACES    PAD WITH SPACES                              
         CLI   5(R2),0                                                          
         BNE   *+14                                                             
         MVC   GERROR,=AL2(MISSING)                                             
         B     SFMERROR                                                         
         CLC   SFMAPPL,=C'ALL '    ALL APPLICATIONS?                            
         BE    VR25                YES                                          
*                                                                               
         L     R5,AFACIDTB         TABLE OF APPLICATION-IDS                     
         USING FACITABD,R5                                                      
VR10     CLC   SFMAPPL,FACISN4     MATCH ON NAME?                               
         BE    VR20                YES                                          
         LA    R5,L'FACITAB(R5)    NO -- TRY NEXT ENTRY                         
         CLI   0(R5),X'FF'         END OF TABLE?                                
         BNE   VR10                                                             
         MVC   GERROR,=AL2(INVAPPL)                                             
         B     SFMERROR                                                         
*                                                                               
VR20     MVC   BRDFAPPL,FACIID     APPLICATION NUMBER                           
         TM    FACIFL,FACITST      IS IT A TEST FACPAK?                         
         BZ    *+8                                                              
         MVI   APPLTEST,C'Y'       YES                                          
         DROP  R5                                                               
*                                                                               
         ICM   R0,15,BRDSTAT       FOUR STATUS BYTES                            
         ZIC   R1,BRDFAPPL                                                      
         N     R1,=X'0000003F'     LEAVE 6 BITS OF APPL-ID NUMBER               
         SLL   R1,18                                                            
         OR    R0,R1               BITS 8..13                                   
         STCM  R0,15,BRDSTAT                                                    
*                                                                               
VR25     LA    R2,SFMSYSH          SYSTEM FIELD                                 
         OC    SFMSYS,MYSPACES     PAD WITH SPACES                              
         CLI   5(R2),0                                                          
         BNE   *+14                                                             
         MVC   GERROR,=AL2(MISSING)                                             
         B     SFMERROR                                                         
         CLC   SFMSYS,=C'ALL    '  ALL SYSTEMS?                                 
         BE    VR40                YES                                          
*                                                                               
         L     R5,ASELIST                                                       
         USING SELISTD,R5                                                       
         LH    R6,0(R5)            LENGTH OF TABLE ENTRY                        
         L     R7,2(R5)            A(END OF TABLE)                              
         LA    R5,6(R5)            A(FIRST ENTRY)                               
*                                                                               
         CLC   SENAME,SFMSYS       MATCH ON SPECIFIC NAME?                      
         BE    *+12                YES                                          
         BXLE  R5,R6,*-10          NO -- TRY NEXT ENTRY                         
         B     VR30                IT'S NOT A SPECIFIC SYSTEM                   
         MVC   BRDFSNUM,SESYS      SYSTEM NUMBER                                
         MVC   BRDFOVSY,SEOVSYS    SYSTEM OVERLAY NUMBER                        
         MVC   ASEPGMS,SEPGMS      A(PROGRAMS TABLE)                            
         B     VR40                                                             
         DROP  R5                                                               
*                                                                               
VR30     L     R5,ASYSLST                                                       
         USING SYSLSTD,R5                                                       
         LH    R6,0(R5)            LENGTH OF TABLE ENTRY                        
         L     R7,2(R5)            A(END OF TABLE)                              
         LA    R5,6(R5)            A(FIRST ENTRY)                               
*                                                                               
         CLC   SYSLNAME,SFMSYS     MATCH ON GENERIC SYSTEM?                     
         BE    *+18                YES                                          
         BXLE  R5,R6,*-10          NO -- TRY NEXT ENTRY                         
         MVC   GERROR,=AL2(INVSYS)                                              
         B     SFMERROR            INVALID SYSTEM NAME                          
         MVC   BRDFOVSY,SYSLNUM    SYSTEM OVERLAY NUMBER                        
         DROP  R5                                                               
*                                                                               
         L     R5,ASELIST          A(SYSTEM EXECUTIVE LIST)                     
         USING SELISTD,R5                                                       
         LH    R6,0(R5)            LENGTH OF TABLE ENTRY                        
         L     R7,2(R5)            A(END OF TABLE)                              
         LA    R5,6(R5)            A(FIRST ENTRY)                               
         CLC   SEOVSYS,BRDFOVSY    FIND FIRST ENTRY FOR THIS SYSTEM             
         BE    *+10                                                             
         BXLE  R5,R6,*-10          TRY NEXT ENTRY                               
         DC    H'0'                                                             
         MVC   ASEPGMS,SEPGMS      A(PROGRAM NAME LIST)                         
         DROP  R5                                                               
*                                                                               
VR40     LA    R2,SFMPROGH         PROGRAM NAME                                 
         OC    SFMPROG,MYSPACES    PAD WITH SPACES                              
         CLI   5(R2),0             ANY INPUT?                                   
         BNE   *+14                YES                                          
         MVC   GERROR,=AL2(MISSING)                                             
         B     SFMERROR                                                         
*                                                                               
         CLC   SFMPROG,=C'ALL    ' ALL PROGRAMS?                                
         BE    VR50                YES                                          
         CLI   BRDFOVSY,0          ALL SYSTEMS?                                 
         BNE   *+14                NO                                           
         MVC   GERROR,=AL2(INVALID)                                             
         B     SFMERROR            PROGRAM ONLY VALID WITHIN A SYSTEM           
*                                                                               
         L     R5,ASEPGMS          A(PROGRAMS LIST)                             
         USING PGMLSTD,R5                                                       
         LH    R6,0(R5)            LENGTH OF TABLE ENTRY                        
         L     R7,2(R5)            A(END OF TABLE)                              
         LA    R5,6(R5)            A(FIRST ENTRY)                               
         ZIC   R1,5(R2)            INPUT LENGTH                                 
         BCTR  R1,0                FOR EXECUTED COMPARE                         
*                                                                               
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   SFMPROG(0),PGMNAME  MATCH ON PROGRAM NAME?                       
         BE    *+18                YES                                          
         BXLE  R5,R6,*-18          TRY NEXT TABLE ENTRY                         
         MVC   GERROR,=AL2(INVPROG)                                             
         B     SFMERROR            PROGRAM NAME NOT IN TABLE                    
         MVC   BRDFPROG,PGMNUM     SAVE PROGRAM NUMBER                          
         DROP  R5                                                               
*                                                                               
VR50     LA    R2,SFMCTRYH         COUNTRY FIELD                                
         OC    SFMCTRY,MYSPACES    PAD WITH SPACES                              
         CLI   5(R2),0                                                          
         BNE   *+14                A COUNTRY WAS GIVEN                          
         MVC   BRDFCTRY,COUNTRY    DEFAULT TO CONNECTED COUNTRY                 
         B     VR60                                                             
*                                                                               
         CLC   =C'ALL',SFMCTRY     IS THE MESSAGE FOR ALL COUNTRIES?            
         BNE   *+12                                                             
         MVI   BRDFCTRY,X'FF'      YES                                          
         B     VR60                                                             
*                                                                               
         L     R5,ACNTRY           A(COUNTRY TABLE)                             
         USING CTRYTABD,R5                                                      
         LH    R6,0(R5)            LENGTH OF TABLE ENTRY                        
         L     R7,2(R5)            A(END OF TABLE)                              
         LA    R5,6(R5)            A(FIRST ENTRY)                               
*                                                                               
         CLC   SFMCTRY,CTRYSHR     MATCH ON SHORT COUNTRY NAME?                 
         BE    *+18                YES                                          
         BXLE  R5,R6,*-10          NO -- TRY NEXT ENTRY                         
         MVC   GERROR,=AL2(INVCNTRY)                                            
         B     SFMERROR                                                         
         MVC   BRDFCTRY,CTRYCODE   COUNTRY CODE                                 
         DROP  R5                                                               
*                                                                               
VR60     LA    R2,SFMDATEH         DATE FIELD                                   
         XC    WORK,WORK                                                        
         GOTO1 PERVAL,DMCB,(SFMDATEH+5,SFMDATE),WORK                            
*                                                                               
         CLI   MSGTYPE,C'P'        PERMANENT MESSAGE?                           
         BNE   VR70                NO                                           
         CLI   DMCB+4,PVRCMISS     NO DATE INPUT?                               
         BE    VR71                RIGHT                                        
         MVC   GERROR,=AL2(NOTINPER)                                            
         B     SFMERROR            PERMANENT MESSAGES CAN'T HAVE DATES          
*                                                                               
VR70     CLI   DMCB+4,PVRCINV1     DATE 1 INVALID?                              
         BNE   *+14                NO                                           
         MVC   GERROR,=AL2(INVDATE)                                             
         B     SFMERROR                                                         
         CLI   DMCB+4,PVRCINV2     DATE 2 INVALID?                              
         BNE   *+14                NO                                           
         MVC   GERROR,=AL2(INVDATE)                                             
         B     SFMERROR                                                         
*                                                                               
         LA    R5,WORK             PERVAL OUTPUT AREA                           
         USING PERVALD,R5                                                       
         MVC   BRDFSTDT,PVALCSTA   COMPRESSED START DATE                        
         MVC   BRDFENDT,PVALCEND   COMPRESSED END DATE                          
*&&US*&& MVC   BRDSTAT+2(2),BRDFENDT                                            
         DROP  R5                                                               
*&&UK                                                                           
         ICM   R0,15,BRDSTAT       FOUR STATUS BYTES                            
         SR    R1,R1                                                            
         ICM   R1,3,BRDFSTDT       COMPRESSED START DATE                        
         N     R1,=X'000001FF'     LEAVE MONTH AND DAY ONLY                     
         SLL   R1,9                                                             
         OR    R0,R1               BITS 14..22                                  
         SR    R1,R1                                                            
         ICM   R1,3,BRDFENDT       COMPRESSED END DATE                          
         N     R1,=X'000001FF'     LEAVE MONTH AND DAY ONLY                     
         OR    R0,R1               BITS 23..31                                  
         STCM  R0,15,BRDSTAT                                                    
*&&                                                                             
* VALIDATE REPETITION & INTERVAL FIELDS                                         
VR71     XC    REPS,REPS                                                        
         XC    INTERVL,INTERVL                                                  
*                                                                               
         LA    R2,SFMREPH                                                       
         CLI   5(R2),0             ANY INPUT?                                   
         BE    VR75                                                             
*                                                                               
         CLI   MSGTYPE,C'P'        PERMANENT MESSAGE?                           
         BNE   *+14                NO                                           
         MVC   GERROR,=AL2(NOTINPER)                                            
         B     SFMERROR            PERMANENT MESSAGES CAN'T HAVE TIMES          
*                                                                               
         TM    4(R2),X'08'         VALID NUMERIC?                               
         BNZ   *+14                                                             
         MVC   GERROR,=AL2(INVALID)                                             
         B     SFMERROR                                                         
*                                                                               
         PACK  DUB,8(1,R2)                                                      
         CVB   R1,DUB                                                           
         LTR   R1,R1                                                            
         BNZ   *+14                                                             
         MVC   GERROR,=AL2(INVALID)                                             
         B     SFMERROR                                                         
         STC   R1,REPS             SAVE NUMBER OF REPETITIONS                   
*                                                                               
VR75     DS    0H                  VALIDATE INTERVAL                            
         LA    R2,SFMINTH                                                       
         OC    REPS,REPS           REQ'D IF REPS                                
         BNZ   *+16                                                             
         CLI   5(R2),0                                                          
         BE    VR80                                                             
         B     *+12                                                             
*                                                                               
         TM    4(R2),X'08'         VALID NUMERIC?                               
         BNZ   *+14                                                             
         MVC   GERROR,=AL2(INVALID)                                             
         B     SFMERROR                                                         
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R2)                                                      
         CVB   R1,DUB                                                           
         LTR   R1,R1                                                            
         BZ    *+12                                                             
         CH    R1,=H'960'          16 HOUR MAXIMUM RANGE (960 MINUTES)          
         BNH   *+14                                                             
         MVC   GERROR,=AL2(INVALID)                                             
         B     SFMERROR                                                         
         STH   R1,INTERVL          SAVE INTERVAL                                
*                                                                               
VR80     LA    R2,SFMTIMEH         TIME                                         
         CLI   5(R2),0                                                          
         BNE   *+18                                                             
         OC    REPS,REPS           IF REPETITIONS, MUST HAVE A TIME             
         BNZ   VR87                                                             
         B     VR90                NOT GIVEN -- IT'S EFFECTIVE ALL DAY          
*                                                                               
         CLI   MSGTYPE,C'P'        PERMANENT MESSAGE?                           
         BNE   *+14                NO                                           
         MVC   GERROR,=AL2(NOTINPER)                                            
         B     SFMERROR            PERMANENT MESSAGES CAN'T HAVE TIMES          
*                                                                               
         TM    4(R2),X'08'         IS FIELD NUMERIC?                            
         BZ    VR85                NO -- IT'S A RANGE OF TIMES                  
         OC    REPS,REPS           NOT VALID IF REPS USED                       
         BZ    *+14                                                             
         MVC   GERROR,=AL2(INVALID)                                             
         B     SFMERROR                                                         
*                                                                               
         ZIC   RE,5(R2)            IT'S THE NUMBER OF EFFECTIVE MINUTES         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R2)                                                      
         CVB   RE,DUB                                                           
         LTR   RE,RE                                                            
         BNZ   *+14                                                             
         MVC   GERROR,=AL2(INVALID)                                             
         B     SFMERROR                                                         
         CH    RE,=H'960'          16 HOUR MAXIMUM RANGE (960 MINUTES)          
         BNH   *+14                                                             
         MVC   GERROR,=AL2(INVALID)                                             
         B     SFMERROR                                                         
         MH    RE,=H'60'           RE = NUMBER OF SECONDS ACTIVE                
*                                                                               
         TBIN  DDSTIME=YES         R1 = START TIME IN SECONDS                   
*&&US*&& AR    R0,R1               ADJUST FOR U.S. CLOCK DIFFERENCE             
         AR    RE,R0               RE = END TIME IN SECONDS                     
         SRDL  R0,32               PREPARE FOR DIVIDE                           
         SRDL  RE,32                                                            
         D     R0,=F'60'           CONVERT TO MINUTES                           
         D     RE,=F'60'                                                        
         SR    R0,R0               PREPARE FOR DIVIDE                           
         SR    RE,RE                                                            
         D     R0,=F'60'           R1 = START HOURS, R0 = START MINUTES         
         D     RE,=F'60'           RF = END HOURS, RE = END MINUTES             
         STC   R1,BRDFSTTM         START HOURS                                  
         STC   R0,BRDFSTTM+1       START MINUTES                                
         STC   RF,BRDFENTM         END HOURS                                    
         STC   RE,BRDFENTM+1       END MINUTES                                  
         B     VR90                                                             
*                                                                               
VR85     ZIC   R5,5(R2)                                                         
         GOTO1 TIMVAL,DMCB,((R5),SFMTIME),FULL                                  
         CLI   DMCB,X'FF'                                                       
         BNE   *+14                                                             
         MVC   GERROR,=AL2(INVTIME)                                             
         B     SFMERROR                                                         
         OC    FULL+2(2),FULL+2    END TIME GIVEN?                              
         BNZ   *+14                                                             
         MVC   GERROR,=AL2(NOENDTM)                                             
         B     SFMERROR            START/END RANGE REQUIRED                     
*                                                                               
         SR    RE,RE                                                            
         LH    RF,FULL             START TIME                                   
         D     RE,=F'100'                                                       
         STC   RF,BRDFSTTM         HOURS                                        
         STC   RE,BRDFSTTM+1       MINUTES                                      
         MVC   STTM,BRDFSTTM                                                    
         SR    RE,RE                                                            
         LH    RF,FULL+2           END TIME                                     
         D     RE,=F'100'                                                       
         STC   RF,BRDFENTM         HOURS                                        
         STC   RE,BRDFENTM+1       MINUTES                                      
         MVC   ENTM,BRDFENTM                                                    
         OC    REPS,REPS           USING REPS?                                  
         BZ    VR90                NO, GET OUT                                  
*                                                                               
* MAKE END TIME = START TIME + (INTERVAL-1), BUT NOT G.T. ENTM                  
         ZIC   R1,BRDFSTTM         R1=HOURS                                     
         LA    RF,60                                                            
         MR    R0,RF               R1=HOURS IN MINUTES                          
         ZIC   RE,BRDFSTTM+1       RE=MINUTES                                   
         AR    R1,RE               R1=STTM IN MINUTES                           
         SR    R0,R0                                                            
         LH    RE,INTERVL                                                       
         BCTR  RE,0                                                             
         AR    R1,RE               ADD INTERVAL TO GET NEW END TIME             
         DR    R0,RF                                                            
         STC   R1,BRDFENTM         HOURS                                        
         STC   R0,BRDFENTM+1       MINUTES                                      
         CLC   BRDFENTM,ENTM                                                    
         BNH   *+10                                                             
         MVC   BRDFENTM,ENTM                                                    
         B     VR90                                                             
*                                                                               
* REPS BEING USED, AND NO TIME GIVEN                                            
VR87     DS    0H                                                               
*                                                                               
         LH    RE,INTERVL                                                       
         BCTR  RE,0                1 MINUTE L.T.INTERVL FOR ENTM                
         MH    RE,=H'60'           RE = NUMBER OF SECONDS ACTIVE                
*                                                                               
         TBIN  DDSTIME=YES         R1 = START TIME IN SECONDS                   
*&&US*&& AR    R0,R1               ADJUST FOR U.S. CLOCK DIFFERENCE             
         AR    RE,R0               RE = END TIME IN SECONDS                     
         SRDL  R0,32               PREPARE FOR DIVIDE                           
         SRDL  RE,32                                                            
         D     R0,=F'60'           CONVERT TO MINUTES                           
         D     RE,=F'60'                                                        
         SR    R0,R0               PREPARE FOR DIVIDE                           
         SR    RE,RE                                                            
         D     R0,=F'60'           R1 = START HOURS, R0 = START MINUTES         
         D     RE,=F'60'           RF = END HOURS, RE = END MINUTES             
         STC   R1,BRDFSTTM         START HOURS                                  
         STC   R0,BRDFSTTM+1       START MINUTES                                
         STC   RF,BRDFENTM         END HOURS                                    
         STC   RE,BRDFENTM+1       END MINUTES                                  
         MVC   STTM,BRDFSTTM                                                    
         XC    ENTM,ENTM                                                        
*                                                                               
VR90     LA    R2,SFMDAYSH         DAYS OF WEEK TO SEND MSG ON                  
         CLI   5(R2),0                                                          
         BNE   VR95                                                             
         CLI   MSGTYPE,C'P'        PERM MESSAGE?                                
         BE    VR100               YES, DOESN'T HAVE DAYS                       
         MVI   BRDFDAYS,X'7F'      TEMP MSG - DEFAULT TO EVERY DAY              
         B     VR100                                                            
*                                                                               
VR95     CLI   MSGTYPE,C'P'        PERMANENT MESSAGE?                           
         BNE   *+14                NO                                           
         MVC   GERROR,=AL2(NOTINPER)                                            
         B     SFMERROR            PERMANENT MESSAGES CAN'T HAVE TIMES          
*                                                                               
         GOTO1 VDEJAVU,DMCB,(SFMDAYSH+5,SFMDAYS),(X'10',BRDFDAYS),0             
         CLI   DMCB+4,X'FF'        IF X'FF', INVALID INPUT                      
         BNE   VR100                                                            
         MVC   GERROR,=AL2(INVALID)                                             
         B     SFMERROR                                                         
*                                                                               
VR100    LA    R2,SFMLUIDH         LUID FIELD                                   
         CLI   5(R2),8             8 CHARACTER MAX                              
         BNH   *+14                                                             
         MVC   GERROR,=AL2(INVALID)                                             
         B     SFMERROR                                                         
         CLC   =C'ALL',SFMLUID     'ALL' LUIDS?                                 
         BE    VR105               YES -- THERE IS NO LUID FILTER               
         MVC   BRDFLUID,SFMLUID    VTAM LUID                                    
         DROP  R3                                                               
*                                                                               
VR105    GOTO1 ADDELEM                                                          
         CLI   DMCB+12,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVI   ELCODE,BRDHEDEQ     HEADING ELEMENT                              
         GOTO1 REMELEM                                                          
*                                                                               
         LA    R2,SFMHEADH         HEADING FIELD                                
         CLI   5(R2),0             ANY HEADING?                                 
         BNE   *+14                NO                                           
         MVC   GERROR,=AL2(MISSING)                                             
         B     SFMERROR                                                         
*                                                                               
         XC    ELEM,ELEM                                                        
         LA    R3,ELEM                                                          
         USING BRDHEDEL,R3                                                      
         MVI   BRDHEDEL,BRDHEDEQ   ELEMENT CODE                                 
         MVI   BRDHEDLN,BRDHEDLQ   ELEMENT LENGTH                               
         MVC   BRDHEDTL,5(R2)      LENGTH OF HEADING                            
         MVC   BRDHEDTX,SFMHEAD    ACTUAL HEADING                               
         OC    BRDHEDTX,MYSPACES   MAKE HEADING UPPER CASE                      
         DROP  R3                                                               
*                                                                               
         GOTO1 ADDELEM                                                          
         CLI   DMCB+12,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         BAS   RE,SCREDIT          CHECK FOR SCREEN EDIT COMMANDS               
*                                                                               
         LA    R2,SFMTXTH          FIRST TEXT FIELD                             
         MVI   TXTFOUND,C'N'       NO TEXT FOUND YET                            
         MVI   SEQNUM,0                                                         
         MVI   ELCODE,BRDTXTEQ     TEXT ELEMENT CODE                            
         GOTO1 REMELEM             GET RID OF EXISTING ELEMENTS                 
         LA    R3,ELEM                                                          
         USING BRDTXTEL,R3                                                      
*                                                                               
VR110    LA    RF,SFMTAGH                                                       
         CR    R2,RF               END OF SCREEN?                               
         BNL   VR160               YES                                          
         TM    1(R2),X'20'         PROTECTED?                                   
         BO    VR150               YES                                          
*                                                                               
         XC    ELEM,ELEM                                                        
         ZIC   RF,SEQNUM           INCREMENT SEQUENCE NUMBER                    
         LA    RF,1(RF)                                                         
         STC   RF,SEQNUM                                                        
         MVI   BRDTXTEL,BRDTXTEQ   TEXT LINE ELEMENT CODE                       
         MVC   BRDTXTSQ,SEQNUM     SEQUENCE NUMBER                              
*                                                                               
         ZIC   R1,5(R2)            LENGTH OF INPUT                              
         LTR   R1,R1               ANY INPUT ON THIS LINE?                      
         BZ    VR120               NO                                           
         MVI   TXTFOUND,C'Y'       SOME TEXT WAS FOUND                          
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   BRDTXTTX(0),8(R2)   TEXT LINE                                    
         LA    R6,BRDTXTOV         LENGTH OF ELEMENT OVERHEAD                   
         LA    R1,1(R6,R1)         TOTAL LENGTH OF ELEMENT                      
         B     VR140                                                            
*                                                                               
VR120    ST    R2,FULL             HANG ON TO CURRENT TWA POINTER               
         LA    RF,SFMTAGH                                                       
*                                                                               
VR130    ZIC   R0,0(R2)            BUMP TO NEXT TEXT FIELD                      
         AR    R2,R0                                                            
         CR    R2,RF               END OF SCREEN?                               
         BL    *+12                NO                                           
         L     R2,FULL                                                          
         B     VR160                                                            
*                                                                               
         TM    1(R2),X'20'         PROTECTED?                                   
         BO    VR130               YES                                          
         CLI   5(R2),0             ANY INPUT THIS FIELD?                        
         BE    VR130               NO -- TRY NEXT FIELD                         
         L     R2,FULL                                                          
*                                                                               
         MVI   BRDTXTTX,C' '       MUST SAVE A BLANK LINE                       
         LA    R1,BRDTXTOV                                                      
         LA    R1,1(R1)            TOTAL LENGTH OF ELEMENT                      
*                                                                               
VR140    STC   R1,BRDTXTLN                                                      
         DROP  R3                                                               
*                                                                               
         GOTO1 ADDELEM                                                          
         CLI   DMCB+12,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
VR150    ZIC   R0,0(R2)            BUMP TO NEXT TEXT FIELD                      
         AR    R2,R0                                                            
         B     VR110                                                            
*                                                                               
VR160    CLI   TXTFOUND,C'Y'       MAKE SURE SOME TEXT WAS FOUND                
         BE    *+18                                                             
         LA    R2,SFMTXTH                                                       
         MVC   GERROR,=AL2(NOTEXT)                                              
         B     SFMERROR                                                         
*                                                                               
         CLI   SFMTYPE,C'T'        TEMPORARY MESSAGE?                           
         BNE   VRX                 NO -- CONFIRMATION NOT REQUIRED              
         CLI   APPLTEST,C'Y'       GOING TO A TEST FACPAK?                      
         BE    VRX                 YES -- CONFIRMATION NOT REQUIRED             
         LA    R2,SFMOKH           CONFIRMATION FIELD                           
         CLI   5(R2),0                                                          
         BNE   *+14                                                             
         MVC   GERROR,=AL2(INVCONF)                                             
         B     SFMERROR            MISSING                                      
         CLC   =C'YES',SFMOK                                                    
         BE    *+14                OK -- IT'S CONFIRMED                         
         MVC   GERROR,=AL2(INVCONF)                                             
         B     SFMERROR            INVALID                                      
*                                                                               
VRX      OC    ACOMI,ACOMI         FORCE CURSOR ON INSERT                       
         BZ    XIT                                                              
         L     R2,ACOMI                                                         
         AH    R2,=Y(SFMTXT2-SFMTXT-8)                                          
         ST    R2,ACURFORC                                                      
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*************************************************************                   
*        SCAN TEXT FOR /? COMMANDS AND EXECUTE THEM         *                   
*************************************************************                   
         SPACE 1                                                                
SCREDIT  NTR1                                                                   
         XC    COMTABL(COMTABX-COMTABL),COMTABL                                 
         LA    R2,SFMTXT           START ON TOP LINE                            
SCR000   ST    R2,FULL             SAVE A(LINE)                                 
*                                                                               
SCR010   LA    R0,L'SFMTXT-1                                                    
         CLI   0(R2),C'/'          SCAN FOR A '/'                               
         BE    SCR015                                                           
SCR011   LA    R2,1(R2)                                                         
         BCT   R0,*-12                                                          
*                                                                               
         L     R2,FULL             RESTORE R2                                   
         LA    RF,SFMTAGH                                                       
         LA    R2,(SFMTXT2-SFMTXT)(R2)                                          
         CR    R2,RF               END OF SCREEN?                               
         BL    SCR000                                                           
         B     SCR100              EXECUTE COMMANDS                             
*                                                                               
SCR015   LA    R1,COMCHARS                                                      
         LA    RF,COMTABL                                                       
         MVC   BYTE,1(R2)                                                       
         OI    BYTE,X'40'                                                       
SCR016   CLC   0(1,R1),BYTE        IS THIS A VALID COMMAND                      
         BE    SCR017                                                           
         LA    RF,4(RF)            BUMP COMMAND TABLE ENTRY                     
         LA    R1,1(R1)                                                         
         CLI   0(R1),C'X'          NOT A COMMAND - IGNORE THIS /                
         BE    SCR011                                                           
         B     SCR016              ELSE KEEP TRYING                             
*                                                                               
SCR017   MVC   0(2,R2),MYSPACES    CLEAR COMMAND                                
         OC    0(4,RF),0(RF)                                                    
         BNZ   ERRCONF             COMMAND CONFLICT (DUP)                       
         MVC   0(4,RF),FULL        SAVE THIS LINE ADDRESS                       
         L     R1,FULL                                                          
         SH    R1,=H'8'                                                         
         OI    6(R1),X'80'                                                      
         B     SCR011              COMMAND SAVED - CONTINUE                     
*                                                                               
SCR100   ICM   R1,15,ACOMD         DELETES FIRST                                
         BZ    *+8                                                              
         BAS   RE,DELLINE          DELETE LINE AT R1                            
*                                                                               
         MVI   BYTE,0                                                           
         L     R2,ACOMM            COMMAND CONFLICT (MOVE & COPY)               
         BAS   RE,CHECKSRC                                                      
         L     R2,ACOMC                                                         
         BAS   RE,CHECKSRC                                                      
         L     R2,ACOMO                                                         
         BAS   RE,CHECKDST                                                      
         L     R2,ACOMB            COMMAND CONFLICT (BEFORE AFTER OVER)         
         BAS   RE,CHECKDST                                                      
         L     R2,ACOMA                                                         
         BAS   RE,CHECKDST                                                      
         TM    BYTE,X'C0'                                                       
         BZ    SCR110                                                           
         BO    SCR100A                                                          
         L     R2,FULL                                                          
         B     ERRCONF                                                          
*                                                                               
CHECKSRC LTR   R2,R2                                                            
         BZR   RE                                                               
         ST    R2,FULL                                                          
         TM    BYTE,X'80'          WE CAN ONLY HAVE ONE                         
         BO    ERRCONF                                                          
         OI    BYTE,X'80'          YES WE HAVE A SOURCE                         
         BR    RE                                                               
*                                                                               
CHECKDST LTR   R2,R2                                                            
         BZR   RE                                                               
         ST    R2,FULL                                                          
         TM    BYTE,X'40'          WE CAN ONLY HAVE ONE                         
         BO    ERRCONF                                                          
         OI    BYTE,X'40'          YES WE HAVE A SOURCE                         
         BR    RE                                                               
*                                                                               
SCR100A  ICM   R1,15,ACOMM         MOVE - SAVE LINE AT R1                       
         BZ    *+14                                                             
         MVC   ELEMENT(L'SFMTXT),0(R1)                                          
         BAS   RE,DELLINE          THEN DELETE SOURCE LINE                      
*                                                                               
         ICM   R1,15,ACOMC         COPY - SAVE LINE AT R1                       
         BZ    *+10                                                             
         MVC   ELEMENT(L'SFMTXT),0(R1)                                          
*                                                                               
         ICM   R1,15,ACOMA         AFTER - INSERT LINE                          
         BZ    *+8                                                              
         BAS   RE,INSLINE                                                       
*                                                                               
         ICM   R1,15,ACOMB         BEFORE - SUB 1 LINE AND INSERT               
         BZ    *+12                                                             
         SH    R1,=Y(SFMTXT2-SFMTXT)                                            
         BAS   RE,INSLINE                                                       
*                                                                               
         ICM   R1,15,ACOMO         OVER - REPLACE BLANKS OR NULLS               
         BZ    SCR110                                                           
         LA    RE,L'SFMTXT-1       USE RE AS INDEX                              
SCR101   LA    RF,0(RE,R1)                                                      
         CLI   0(RF),C' '          TEST FOR REAL CHARACTER                      
         BNH   SCR102                                                           
         BCT   RE,SCR101                                                        
         B     SCR103                                                           
*                                                                               
SCR102   IC    RF,ELEMENT(RE)      REPLACE NON CHR WITH ELEMENT                 
         STC   RF,0(RE,R1)                                                      
         BCT   RE,SCR101                                                        
SCR103   SH    R1,=H'8'            AND SET THE BLOODY INPUT LENGTH              
         MVI   5(R1),L'SFMTXT                                                   
*                                                                               
SCR110   ICM   R1,15,ACOMI         INSERT - INSERT 1 BLANK LINE                 
         BZ    *+14                                                             
         MVC   ELEMENT(L'SFMTXT),MYSPACES                                       
         BAS   RE,INSLINE                                                       
*                                                                               
SCREXIT  B     XIT                                                              
*                                                                               
ERRCONF  MVC   GERROR,=AL2(CONFLICT)                                            
         L     R2,FULL                                                          
         SH    R2,=H'8'                                                         
         OI    6(R2),X'01'         SET MODIFIED                                 
         B     SFMERROR                                                         
         EJECT                                                                  
*************************************************************                   
*        DELETE A LINE R1=A(LINE)                           *                   
*************************************************************                   
         SPACE 1                                                                
DELLINE  NTR1                                                                   
DELL000  ST    R1,FULL             SAVE THIS LINE ADDR                          
         LA    RF,SFMTXTL                                                       
         LA    R1,(SFMTXT2-SFMTXT)(R1)                                          
         CR    R1,RF               TEST END OF SCREEN                           
         BNH   DELL010                                                          
         L     RF,FULL             MOVE SPACES TO THIS LINE & EXIT              
         MVC   0(L'SFMTXT,RF),MYSPACES                                          
         B     XIT                                                              
*                                                                               
DELL010  BAS   RE,COPYLIN          COPY TEXT LINE                               
         B     DELL000                                                          
         EJECT                                                                  
*************************************************************                   
*    INSERT LINE R1=A(LINE) INSERTION IN ELEMENT L'SFMTXT   *                   
*************************************************************                   
         SPACE 1                                                                
INSLINE  NTR1                                                                   
         ST    R1,DUB              SAVE THIS LINE ADDR                          
         LA    R1,SFMTXTL                                                       
INSL000  ST    R1,FULL                                                          
         SH    R1,=Y(SFMTXT2-SFMTXT)                                            
         L     RF,DUB                                                           
         CR    R1,RF               TEST INSERT LINE                             
         BH    INSL010                                                          
         L     R1,FULL             MOVE ELEMENT TO THIS LINE & EXIT             
         MVC   0(L'SFMTXT,R1),ELEMENT                                           
         SH    R1,=H'8'                                                         
         MVI   5(R1),L'SFMTXT      SET IP LEN                                   
         B     XIT                                                              
*                                                                               
INSL010  BAS   RE,COPYLIN          COPY TEXT LINE                               
         B     INSL000                                                          
*                                                                               
COPYLIN  NTR1                                                                   
         L     RF,FULL             RF = THIS LINE                               
         SH    RF,=H'8'                                                         
         SH    R1,=H'8'                                                         
         MVC   5(1,RF),5(R1)          COPY THE BLOODY INPUT LENGTH              
         LA    R1,8(R1)                                                         
         LA    RF,8(RF)                                                         
         MVC   0(L'SFMTXT,RF),0(R1)   COPY NEXT LINE TO THIS LINE               
         LA    RE,COMTABL             CHECK COMMANDS AS WELL                    
         LA    R0,COMTABX-COMTABL                                               
         SRL   R0,2                                                             
COPYL010 C     R1,0(RE)               WAS THERE A COMMAND                       
         BNE   *+10                                                             
         MVC   0(4,RE),FULL           SWAP ADDRESSES                            
         LA    RE,4(RE)                                                         
         BCT   R0,COPYL010            NEXT                                      
         B     XIT                                                              
         EJECT                                                                  
* DISPLAY RECORD                                                                
*                                                                               
DR       L     R3,AIO                                                           
         USING BRDFLTD,R3                                                       
         MVI   ELCODE,BRDFLTCQ     FILTER ELEMENT CODE                          
         BAS   RE,GETEL            FILTER ELEMENT MUST BE THERE                 
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   MODE,XRECADD                                                     
         BNE   DR05                                                             
         L     R4,AIO                                                           
         USING BRDKEY,R4                                                        
         EDIT  BRDKMSGN,(5,SFMMSGN),ALIGN=LEFT                                  
         DROP  R4                                                               
         OI    SFMMSGNH+6,X'80'    MESSAGE NUMBER                               
*                                                                               
DR05     OI    SFMAPPLH+6,X'80'    XMIT APPL-ID                                 
         SR    RF,RF                                                            
         ICM   RF,1,BRDFAPPL                                                    
         BNZ   *+14                ONLY ONE APPLICATION                         
         MVC   SFMAPPL,=C'ALL '    ALL APPLICATIONS                             
         B     DR07                                                             
         MHI   RF,L'FACITAB        NO -- INDEX INTO TABLE                       
         L     RE,AFACIDTB                                                      
         AR    RF,RE                                                            
         MVC   SFMAPPL,FACISN4-FACITABD(RF)                                     
*                                                                               
DR07     OI    SFMDATEH+6,X'80'    XMIT DATE FIELD                              
         XC    SFMDATE,SFMDATE     CLEAR                                        
         OC    BRDFSTDT,BRDFSTDT   ANY DATES TO DISPLAY?                        
         BZ    DR10                NO                                           
*                                                                               
         LA    RE,BRDFSTDT                                                      
         ST    RE,DMCB             A(DATES) -- START, AND MAYBE END TOO         
         MVI   DMCB,2              COMPRESSED INPUT TYPE                        
         CLC   BRDFSTDT,BRDFENDT   SAME START AND END DATES?                    
         BE    *+8                                                              
         OI    DMCB,X'10'          NO -- PASS BOTH DATES                        
         GOTO1 DATCON,DMCB,,(17,SFMDATE)                                        
*                                                                               
DR10     OI    SFMSYSH+6,X'80'     XMIT SYSTEM                                  
         CLI   BRDFSNUM,0          DO WE HAVE SPECIFIC SYSTEM NUMBER?           
         BE    DR20                NO                                           
*                                                                               
         L     R5,ASELIST                                                       
         USING SELISTD,R5                                                       
         LH    R6,0(R5)            LENGTH OF TABLE ENTRY                        
         L     R7,2(R5)            A(END OF TABLE)                              
         LA    R5,6(R5)            A(FIRST ENTRY)                               
*                                                                               
         CLC   BRDFSNUM,SESYS      MATCH ON SPECIFIC NUMBER?                    
         BE    *+10                YES                                          
         BXLE  R5,R6,*-10          NO -- TRY NEXT ENTRY                         
         DC    H'0'                                                             
         MVC   SFMSYS,SENAME       SYSTEM NAME                                  
         MVC   ASEPGMS,SEPGMS      A(PROGRAMS LIST)                             
         B     DR30                                                             
         DROP  R5                                                               
*                                                                               
DR20     CLI   BRDFOVSY,0          ALL SYSTEMS?                                 
         BNE   *+14                NO                                           
         MVC   SFMSYS,=C'ALL    '                                               
         B     DR30                                                             
*                                                                               
         L     R5,ASYSLST                                                       
         USING SYSLSTD,R5                                                       
         LH    R6,0(R5)            LENGTH OF TABLE ENTRY                        
         L     R7,2(R5)            A(END OF TABLE)                              
         LA    R5,6(R5)            A(FIRST ENTRY)                               
*                                                                               
         CLC   BRDFOVSY,SYSLNUM    MATCH ON SYSTEM OVERLAY NUMBER?              
         BE    *+10                YES                                          
         BXLE  R5,R6,*-10          NO -- TRY NEXT ENTRY                         
         DC    H'0'                                                             
         MVC   SFMSYS,SYSLNAME     SYSTEM NAME                                  
         DROP  R5                                                               
*                                                                               
         L     R5,ASELIST                                                       
         USING SELISTD,R5                                                       
         LH    R6,0(R5)            LENGTH OF TABLE ENTRY                        
         L     R7,2(R5)            A(END OF TABLE)                              
         LA    R5,6(R5)            A(FIRST ENTRY)                               
         CLC   BRDFOVSY,SEOVSYS    FIND FIRST ENTRY FOR THIS SYSTEM             
         BE    *+10                                                             
         BXLE  R5,R6,*-10          TRY NEXT ENTRY                               
         DC    H'0'                                                             
         MVC   ASEPGMS,SEPGMS      A(PROGRAMS LIST)                             
         DROP  R5                                                               
*                                                                               
DR30     OI    SFMPROGH+6,X'80'    XMIT PROGRAM FIELD                           
         CLI   BRDFPROG,0          ALL PROGRAMS?                                
         BNE   *+14                NO                                           
         MVC   SFMPROG,=C'ALL    '                                              
         B     DR40                                                             
*                                                                               
         L     R5,ASEPGMS          A(PROGRAM NAME LIST)                         
         USING PGMLSTD,R5                                                       
         LH    R6,0(R5)            LENGTH OF TABLE ENTRY                        
         L     R7,2(R5)            A(END OF TABLE)                              
         LA    R5,6(R5)            A(FIRST ENTRY)                               
*                                                                               
         CLC   BRDFPROG,PGMNUM     MATCH ON PROGRAM NAME?                       
         BE    *+18                                                             
         BXLE  R5,R6,*-10          TRY NEXT TABLE ENTRY                         
         MVC   SFMPROG(3),=C'...'  BAD PROGRAM NUMBER IN RECORD                 
         B     DR40                                                             
         MVC   SFMPROG,PGMNAME                                                  
         DROP  R5                                                               
*                                                                               
DR40     OI    SFMCTRYH+6,X'80'    XMIT COUNTRY FIELD                           
         CLI   BRDFCTRY,X'FF'      'ALL' COUNTRIES?                             
         BNE   *+14                                                             
         MVC   SFMCTRY,=C'ALL'     YES                                          
         B     DR45                                                             
*                                                                               
         L     R5,ACNTRY           A(COUNTRY TABLE)                             
         USING CTRYTABD,R5                                                      
         LH    R6,0(R5)            LENGTH OF TABLE ENTRY                        
         L     R7,2(R5)            A(END OF TABLE)                              
         LA    R5,6(R5)            A(FIRST ENTRY)                               
*                                                                               
         CLC   BRDFCTRY,CTRYCODE   MATCH ON COUNTRY CODE?                       
         BE    *+10                YES                                          
         BXLE  R5,R6,*-10          NO -- TRY NEXT ENTRY                         
         DC    H'0'                                                             
         MVC   SFMCTRY,CTRYSHR                                                  
         DROP  R5                                                               
*                                                                               
DR45     XC    SFMREP,SFMREP       CLEAR & DISPLAY REP & INTERVAL               
         OI    SFMREPH+6,X'80'                                                  
*                                                                               
         XC    SFMINT,SFMINT                                                    
         OI    SFMINTH+6,X'80'                                                  
         OI    SFMTIMEH+6,X'80'    XMIT TIME                                    
         XC    SFMTIME,SFMTIME                                                  
         OC    BRDFSTTM,BRDFSTTM   ANY TIMES IN RECORD?                         
         BZ    DR50                NO                                           
*                                                                               
         ZIC   RE,BRDFSTTM         START HOUR                                   
         MH    RE,=H'100'                                                       
         ZIC   RF,BRDFSTTM+1       START MINUTES                                
         AR    RE,RF                                                            
         STH   RE,FULL             START TIME (MILITARY)                        
         ZIC   RE,BRDFENTM         END HOUR                                     
         MH    RE,=H'100'                                                       
         ZIC   RF,BRDFENTM+1       END MINUTES                                  
         AR    RE,RF                                                            
         STH   RE,FULL+2           END TIME (MILITARY)                          
         GOTO1 UNTIME,DMCB,FULL,SFMTIME                                         
*                                                                               
DR50     MVC   SFMLUID(L'BRDFLUID),BRDFLUID    LUID                             
         OI    SFMLUIDH+6,X'80'    XMIT                                         
*                                                                               
         CLI   SFMTYPE,C'P'        DON'T DISPLAY DAYS FOR PERM RECS             
         BNE   *+18                                                             
         XC    SFMDAYS,SFMDAYS                                                  
         OI    SFMDAYSH+6,X'80'                                                 
         B     DR60                                                             
*                                                                               
         CLI   BRDFDAYS,X'00'      IF ZERO, SAME AS ALL DAYS                    
         BNE   *+8                                                              
         MVI   BRDFDAYS,B'01111111' MON-SUN                                     
         GOTO1 VDEJAVU,DMCB,(7,BRDFDAYS),(X'00',SFMDAYS),0                      
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                SHIT JUST HIT THE FAN                        
         OI    SFMDAYSH+6,X'80'    XMIT DAYS                                    
*                                                                               
DR60     MVC   SFMNAME,BRDFNAME    NAME FIELD                                   
         OI    SFMNAMEH+6,X'80'    XMIT                                         
         DROP  R3                                                               
*                                                                               
         L     R3,AIO              A(FIRST ELEMENT)                             
         USING BRDHEDEL,R3                                                      
         MVC   SFMHEAD,MYSPACES                                                 
         MVI   ELCODE,BRDHEDEQ     HEADING ELEMENT CODE                         
         BAS   RE,GETEL            HEADING ELEMENT MUST BE THERE                
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   SFMHEAD,BRDHEDTX    PUT HEADING IN FIELD                         
         OI    SFMHEADH+6,X'80'    XMIT                                         
         DROP  R3                                                               
*                                                                               
         L     R3,AIO              A(FIRST ELEMENT)                             
         USING BRDTXTEL,R3                                                      
         LA    R2,SFMTXTH          FIRST TEXT FIELD                             
         MVI   ELCODE,BRDTXTEQ     TEXT ELEMENT CODE                            
         BAS   RE,GETEL            ANY TEXT FIELDS?                             
         BE    *+6                                                              
         DC    H'0'                MUST BE SOME TEXT                            
*                                                                               
DR100    MVC   8(L'SFMTXT,R2),MYSPACES                                          
         ZIC   R1,BRDTXTLN         LENGTH OF ELEMENT                            
         LA    R6,BRDTXTOV         OVERHEAD LENGTH                              
         SR    R1,R6               LENGTH OF TEXT                               
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),BRDTXTTX    LINE OF TEXT                                 
         OI    6(R2),X'80'         XMIT                                         
         DROP  R3                                                               
*                                                                               
DR120    ZIC   R0,0(R2)            NEXT TEXT FIELD                              
         AR    R2,R0                                                            
         LA    RF,SFMTAGH                                                       
         CR    R2,RF               END OF SCREEN?                               
         BNL   DR150               YES                                          
         TM    1(R2),X'20'         PROTECTED?                                   
         BO    DR120                                                            
         BAS   RE,NEXTEL           NEXT LINE OF TEXT                            
         BE    DR100                                                            
*                                                                               
DR130    TM    1(R2),X'20'         PROTECTED?                                   
         BO    DR140               YES -- DON'T CLEAR THE FIELD                 
         ZIC   R1,0(R2)                                                         
         SH    R1,=H'9'            LENGTH OF DATA + 1 (FOR EX)                  
         TM    1(R2),X'02'         TEXT EXTENDED HEADER                         
         BZ    *+8                                                              
         SH    R1,=H'8'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),MYSPACES    BLANK OUT REMAINING FIELDS                   
         OI    6(R2),X'80'         XMIT                                         
*                                                                               
DR140    ZIC   R0,0(R2)            NEXT FIELD                                   
         AR    R2,R0                                                            
         LA    RF,SFMTAGH                                                       
         CR    R2,RF               END OF SCREEN?                               
         BL    DR130               NO                                           
*                                                                               
DR150    CLI   MODE,XRECPUT        HAVE WE JUST CHANGED A MESSAGE?              
         BNE   *+18                NO                                           
         XC    SFMOK,SFMOK         CLEAR CONFIRMATION FIELD                     
         OI    SFMOKH+6,X'80'      XMIT                                         
         B     DRX                                                              
*                                                                               
         CLI   MODE,XRECADD        HAVE WE JUST ADDED A NEW MESSAGE?            
         BNE   DRX                 NO                                           
         XC    SFMOK,SFMOK         CLEAR CONFIRMATION FIELD                     
         OI    SFMOKH+6,X'80'      XMIT                                         
*                                                                               
         XC    KEY,KEY             YES -- MUST UPDATE HIGH MSG RECORD           
         LA    R4,KEY                                                           
         USING BRDKEY,R4                                                        
         MVI   BRDKSYS,BRDKSYSQ    BROADCAST MESSAGE RECORD                     
         MVI   BRDKSTYP,BRDKSTYQ                                                
         MVC   BRDKTYPE,MSGTYPE    MESSAGE TYPE                                 
         GOTO1 HIGH                                                             
         CLC   KEY(32),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                THIS RECORD MUST BE THERE                    
         DROP  R4                                                               
*                                                                               
         GOTO1 GETREC              GET HIGH MESSAGE RECORD                      
         L     R3,AIO                                                           
         MVI   ELCODE,BRDHIGEQ     HIGH MESSAGE ELEMENT                         
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING BRDHIGHD,R3                                                      
         SR    R1,R1                                                            
         ICM   R1,3,BRDHIGNO       THE HIGHEST MESSAGE NUMBER                   
         OC    INTERVL,INTERVL                                                  
         BZ    DR160                                                            
         ZIC   RF,REPS                                                          
         AR    R1,RF                                                            
         B     *+8                                                              
DR160    LA    R1,1(R1)            INCREMENT                                    
         STH   R1,BRDHIGNO         THE NEW MESSAGE NUMBER                       
         DROP  R3                                                               
*                                                                               
         CH    R1,MSGNUM                                                        
         BE    *+6                                                              
         DC    H'0'                WE'RE OUT OF SYNC                            
         GOTO1 PUTREC                                                           
*                                                                               
* SET EXIT MESSAGE IF USING REPS                                                
         OC    INTERVL,INTERVL                                                  
         BZ    DRX                 STANDARD EXIT MESSAGE                        
         XC    WORK,WORK                                                        
         LA    R2,CONRECH          TO POSITION CUR                              
         LA    R5,WORK                                                          
         EDIT  MSG1ST,(5,(R5)),ALIGN=LEFT,WRK=MYWORK                            
         AR    R5,R0               R0 = # OF SIGNIFICANT CHARS                  
         MVC   0(4,R5),=X'40A39640'  LOWER CASE:  =C' TO '                      
         LA    R5,4(R5)                                                         
         EDIT  MSGNUM,(5,(R5)),ALIGN=LEFT,WRK=MYWORK                            
         AR    R5,R0                                                            
         LA    R1,WORK             GET LENGTH OF TEXT                           
         SR    R5,R1                                                            
*                                                                               
         STCM  R1,7,GATXT          A(TEXT)                                      
         STC   R5,GLTXT            SET LENGTH OF TEXT                           
         MVC   GERROR,=AL2(RECSADD)  MESSSAGE NUMBER                            
         MVI   GMSGTYPE,C'I'       SET INFORMATIONAL MESSAGE                    
         B     SFMERROR                                                         
*                                                                               
DRX      B     XIT                                                              
         EJECT                                                                  
*                                                                               
* AFTER RECORD ADD                                                              
XRA      DS    0H                                                               
         OC    REPS,REPS                                                        
         BZ    XRAX                NO REP/INTERVAL RECS                         
*                                                                               
         ZIC   R5,REPS                                                          
         BCTR  R5,0                1ST REC ALREADY OUT                          
*                                                                               
* SET NEW TIMES                                                                 
XRA10    DS    0H                                                               
         L     R3,AIO              GET LAST REC                                 
         USING BRDFLTD,R3                                                       
         MVI   ELCODE,BRDFLTCQ     FILTER ELEMENT CODE                          
         BAS   RE,GETEL            FILTER ELEMENT MUST BE THERE                 
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         ZIC   R1,BRDFENTM         R1=HOURS                                     
         LA    RF,60                                                            
         MR    R0,RF               R1=MINUTES                                   
         ZIC   RE,BRDFENTM+1                                                    
         AR    R1,RE                                                            
         ST    R1,DUB              SAVE OLD END TIME IN MINUTES                 
         LA    R1,1(R1)            R1=NEW START TIME IN MINUTES                 
         DR    R0,RF               R0=MIN, R1=HRS                               
*                                                                               
* MAKE SURE REC WILL GET WRITTEN BEFORE MAKING CHANGES                          
         STC   R1,WORK                                                          
         STC   R0,WORK+1                                                        
         OC    ENTM,ENTM                                                        
         BZ    *+14                IF NO END TIME, NO TOP BOUND                 
         CLC   WORK,ENTM           START > END TIME?                            
         BH    XRA20                                                            
*                                                                               
         STC   R1,BRDFSTTM                                                      
         STC   R0,BRDFSTTM+1                                                    
         SR    R0,R0                                                            
         L     R1,DUB              OLD END TIME IN MINUTES                      
         LH    RE,INTERVL                                                       
         AR    R1,RE               ADD INTERVAL TO GET NEW END TIME             
         DR    R0,RF                                                            
         STC   R1,BRDFENTM         HOURS                                        
         STC   R0,BRDFENTM+1       MINUTES                                      
         OC    ENTM,ENTM           NO BOUND IF NO END TIME ENTERED              
         BZ    *+20                                                             
         CLC   BRDFENTM,ENTM       NEW END TIME > ENTERED END TIME?             
         BNH   *+10                                                             
         MVC   BRDFENTM,ENTM                                                    
         DROP  R3                                                               
*                                                                               
* INC MESSAGE NUMBER                                                            
         L     R3,AIO                                                           
         USING BRDKEY,R3                                                        
         ZICM  R1,BRDKMSGN,2                                                    
         LA    R1,1(R1)                                                         
         STCM  R1,3,BRDKMSGN                                                    
         DROP  R3                                                               
*                                                                               
         GOTO1 ADDREC                                                           
         LH    RF,MSGNUM                                                        
         LA    RF,1(RF)                                                         
         STH   RF,MSGNUM                                                        
         BCT   R5,XRA10                                                         
*                                                                               
* SEE IF #RECS ADDED = REPS                                                     
XRA20    DS    0H                                                               
         LTR   R5,R5                                                            
         BZ    XRAX                                                             
         ZIC   R1,REPS                                                          
         SR    R1,R5                                                            
         STC   R1,REPS             REPS = #RECS REALLY ADDED                    
*                                                                               
XRAX     B     DR                  DISPLAY RECORD ADDED                         
         EJECT                                                                  
* AFTER RECORD CHANGE                                                           
*                                                                               
XRP      L     R4,AIO              A(BROADCAST RECORD)                          
         USING BRDKEY,R4                                                        
         LA    R1,KEY                                                           
         MVC   BRDKSTAT+1-BRDKEY(3,R1),BRDSTAT+1                                
         DROP  R4                                                               
*                                                                               
         GOTO1 WRITE               UPDATE KEY                                   
*                                                                               
         B     DR                  NOW DISPLAY RECORD                           
         EJECT                                                                  
* DISPLAY KEY                                                                   
*                                                                               
DK       L     R4,AIO              RECORD SELECTED                              
         USING BRDKEY,R4                                                        
*                                                                               
         EDIT  BRDKMSGN,(5,SFMMSGN),ALIGN=LEFT                                  
         OI    SFMMSGNH+6,X'80'    MESSAGE NUMBER                               
         MVC   SFMTYPE,BRDKTYPE                                                 
         OI    SFMTYPEH+6,X'80'    MESSAGE TYPE                                 
*                                                                               
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
********************************************************************            
*                 ONLINE LIST ROUTINE                                           
********************************************************************            
*                                                                               
LR       LA    R4,KEY                                                           
         USING BRDKEY,R4                                                        
*                                                                               
         OC    KEY(BRDKLENQ),KEY   FIRST TIME THROUGH?                          
         BNZ   LR10                NO                                           
*                                                                               
         MVI   BRDKSYS,BRDKSYSQ    BROADCAST MESSAGE RECORD                     
         MVI   BRDKSTYP,BRDKSTYQ                                                
         MVC   BRDKTYPE,MSGTYPE    MESSAGE TYPE                                 
         MVC   BRDKMSGN,=H'1'      SKIP OVER HIGH MESSAGE RECORD                
         OC    MSGNUM,MSGNUM                                                    
         BZ    *+10                                                             
         MVC   BRDKMSGN,MSGNUM     START AT USER'S MESSAGE NUMBER               
         MVC   SAVEKEY,KEY                                                      
*                                                                               
LR10     GOTO1 HIGH                FIRST RECORD                                 
*                                                                               
LR20     CLC   KEY(10),SAVEKEY     ANY MORE MESSAGES OF THIS TYPE?              
         BNE   XIT                 NO                                           
*                                                                               
         MVC   LISTAR,MYSPACES     CLEAR LIST LINE                              
         GOTO1 GETREC                                                           
         L     R4,AIO              BROADCAST MESSAGE RECORD                     
*                                                                               
         LA    R3,BRDFSTEL         A(FIRST ELEMENT)                             
         USING BRDFLTD,R3                                                       
         MVI   ELCODE,BRDFLTCQ     FILTER ELEMENT CODE                          
         BAS   RE,FIRSTEL          FILTER ELEMENT MUST BE THERE                 
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   LSTNAME,BRDFNAME    NAME FIELD                                   
         LA    R6,BRDFNAME                                                      
*                                                                               
         LA    R5,FILTERS          FILTER EXPRESSION TABLE                      
         LA    R0,4                MAXIMUM OF FOUR FILTER POSITIONS             
*                                                                               
LR30     CLI   0(R5),0             TEST END OF TABLE                            
         BE    LR40                YES - THIS RECORD MAY BE DISPLAYED           
         ZIC   R1,1(R5)            BRANCH MASK                                  
         CLC   0(1,R5),0(R6)       TEST MATCH ON FILTER. . .                    
         EX    R1,*+8              . . . WITH THE PROPER CONDITION              
         B     *+8                                                              
         BC    0,LR100             FILTER FAILED - GET ANOTHER RECORD           
         LA    R5,2(R5)            BUMP TO NEXT ENTRY IN TABLE                  
         LA    R6,1(R6)            BUMP TO NEXT FILTER CHARACTER                
         BCT   R0,LR30             UP TO FOUR FILTERS                           
*                                                                               
LR40     CLI   APPLID,0            ANY APPL-ID FILTER GIVEN?                    
         BE    *+14                NO -- TAKE IT                                
         CLC   BRDFAPPL,APPLID     MATCH ON APPL-ID?                            
         BNE   LR100               NO -- DON'T DISPLAY THIS RECORD              
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,1,BRDFAPPL                                                    
         BNZ   *+14                ONLY ONE APPLICATION                         
         MVC   LSTAPPL,=C'ALL '    ALL APPLICATIONS                             
         B     LR42                                                             
         MHI   RF,L'FACITAB        NO -- INDEX INTO TABLE                       
         L     RE,AFACIDTB                                                      
         AR    RF,RE                                                            
         MVC   LSTAPPL,FACISN4-FACITABD(RF)                                     
*                                                                               
LR42     CLI   OVSYSNUM,0          ANY SYSTEM FILTER GIVEN?                     
         BE    *+14                NO                                           
         CLC   OVSYSNUM,BRDFOVSY   MATCH ON SYSTEM?                             
         BNE   LR100               NO -- DON'T DISPLAY THIS RECORD              
*                                                                               
         CLI   SYSNUM,0            ANY SPECIFIC SYSTEM GIVEN?                   
         BE    *+14                NO                                           
         CLC   SYSNUM,BRDFSNUM     MATCH ON SPECIFIC SYSTEM?                    
         BNE   LR100               NO -- DON'T DISPLAY THIS RECORD              
*                                                                               
         CLI   PROG,0              ANY PROGRAM GIVEN?                           
         BE    *+14                NO                                           
         CLC   PROG,BRDFPROG       MATCH ON PROGRAM?                            
         BNE   LR100               NO -- DON'T DISPLAY THIS RECORD              
*                                                                               
         OC    BRDFSTDT,BRDFSTDT   ANY DATES TO DISPLAY?                        
         BZ    LR50                NO                                           
         OC    STARTDAT,STARTDAT   ANY DATE FILTER GIVEN?                       
         BZ    LR45                NO                                           
         TM    MYFLAG,PVRCONE      ONE DATE ONLY (IGNORE END DATE)              
         BO    *+14                                                             
         CLC   BRDFSTDT,ENDDATE                                                 
         BH    LR100                                                            
         CLC   BRDFENDT,STARTDAT                                                
         BL    LR100               OUT OF RANGE -- DON'T DISPLAY                
*                                                                               
LR45     CLI   MSGTYPE,C'P'        PERMANENT MESSAGE LIST?                      
         BE    LR50                YES -- THERE'S NO DATE TO DISPLAY            
         LA    RE,BRDFSTDT                                                      
         ST    RE,DMCB             A(DATES) -- START, AND MAYBE END TOO         
         MVI   DMCB,2              COMPRESSED INPUT TYPE                        
         CLC   BRDFSTDT,BRDFENDT   SAME START AND END DATES?                    
         BE    *+8                                                              
         OI    DMCB,X'10'          NO -- PASS BOTH DATES                        
         GOTO1 DATCON,DMCB,,(17,LSTDATE)                                        
*                                                                               
LR50     EDIT  BRDKMSGN,(5,LSTMSGN),ALIGN=LEFT                                  
*                                                                               
         CLI   BRDFSNUM,0          DO WE HAVE SPECIFIC SYSTEM NUMBER?           
         BE    LR60                NO                                           
*                                                                               
         L     R5,ASELIST                                                       
         USING SELISTD,R5                                                       
         LH    R6,0(R5)            LENGTH OF TABLE ENTRY                        
         L     R7,2(R5)            A(END OF TABLE)                              
         LA    R5,6(R5)            A(FIRST ENTRY)                               
*                                                                               
         CLC   BRDFSNUM,SESYS      MATCH ON SPECIFIC NUMBER?                    
         BE    *+10                YES                                          
         BXLE  R5,R6,*-10          NO -- TRY NEXT ENTRY                         
         DC    H'0'                                                             
         MVC   LSTSYS,SENAME       SYSTEM NAME                                  
         MVC   ASEPGMS,SEPGMS      A(PROGRAMS TABLE)                            
         B     LR70                                                             
         DROP  R5                                                               
*                                                                               
LR60     CLI   BRDFOVSY,0          ALL SYSTEMS?                                 
         BNE   *+14                NO                                           
         MVC   LSTSYS,=C'ALL    '                                               
         B     LR70                                                             
*                                                                               
         L     R5,ASYSLST                                                       
         USING SYSLSTD,R5                                                       
         LH    R6,0(R5)            LENGTH OF TABLE ENTRY                        
         L     R7,2(R5)            A(END OF TABLE)                              
         LA    R5,6(R5)            A(FIRST ENTRY)                               
*                                                                               
         CLC   BRDFOVSY,SYSLNUM    MATCH ON SYSTEM OVERLAY NUMBER?              
         BE    *+10                YES                                          
         BXLE  R5,R6,*-10          NO -- TRY NEXT ENTRY                         
         DC    H'0'                                                             
         MVC   LSTSYS,SYSLNAME     SYSTEM NAME                                  
         DROP  R5                                                               
*                                                                               
         L     R5,ASELIST          A(SYSTEM EXECUTIVE LIST)                     
         USING SELISTD,R5                                                       
         LH    R6,0(R5)            LENGTH OF TABLE ENTRY                        
         L     R7,2(R5)            A(END OF TABLE)                              
         LA    R5,6(R5)            A(FIRST ENTRY)                               
         CLC   SEOVSYS,BRDFOVSY    FIND FIRST ENTRY FOR THIS SYSTEM             
         BE    *+10                                                             
         BXLE  R5,R6,*-10          TRY NEXT ENTRY                               
         DC    H'0'                                                             
         MVC   ASEPGMS,SEPGMS      A(PROGRAM NAME LIST)                         
         DROP  R5                                                               
*                                                                               
*                                                                               
LR70     DS    0H                                                               
         CLI   BRDFPROG,0          ALL PROGRAMS?                                
         BNE   *+14                                                             
         MVC   LSTPGM,=C'ALL'                                                   
         B     LR74                                                             
         CLI   BRDFOVSY,0          ALL SYSTEMS?                                 
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R5,ASEPGMS          A(PROGRAMS LIST)                             
         USING PGMLSTD,R5                                                       
         LH    R6,0(R5)            LENGTH OF TABLE ENTRY                        
         L     R7,2(R5)            A(END OF TABLE)                              
         LA    R5,6(R5)            A(FIRST ENTRY)                               
*                                                                               
         CLC   PGMNUM,BRDFPROG     MATCH ON PGM NUM?                            
         BE    LR71                                                             
         BXLE  R5,R6,*-10                                                       
         EDIT  (B1,BRDFPROG),(3,LSTPGM)                                         
         B     LR74                                                             
LR71     MVC   LSTPGM,PGMNAME                                                   
         DROP  R5                                                               
*                                                                               
LR74     CLI   CNTRY,0             ANY COUNTRY FILTER                           
         BE    *+14                NO                                           
         CLC   BRDFCTRY,CNTRY      YES, DOES THIS REC MATCH?                    
         BNE   LR100                                                            
         CLI   BRDFCTRY,X'FF'      'ALL' COUNTRIES?                             
         BNE   *+14                                                             
         MVC   LSTCTRY,=C'ALL'     YES                                          
         B     LR75                                                             
*                                                                               
         L     R5,ACNTRY           A(COUNTRY TABLE)                             
         USING CTRYTABD,R5                                                      
         LH    R6,0(R5)            LENGTH OF TABLE ENTRY                        
         L     R7,2(R5)            A(END OF TABLE)                              
         LA    R5,6(R5)            A(FIRST ENTRY)                               
*                                                                               
         CLC   BRDFCTRY,CTRYCODE   MATCH ON COUNTRY CODE?                       
         BE    *+10                YES                                          
         BXLE  R5,R6,*-10          NO -- TRY NEXT ENTRY                         
         DC    H'0'                                                             
         MVC   LSTCTRY,CTRYSHR                                                  
         DROP  R5                                                               
*                                                                               
LR75     CLI   MSGTYPE,C'P'        PERMANENT MESSAGE LIST?                      
         BE    LR80                YES -- THERE'S NO TIME TO DISPLAY            
         OC    BRDFSTTM,BRDFSTTM   ANY TIMES IN RECORD?                         
         BZ    LR80                NO                                           
         OC    STTM,STTM           ANY START TIME FILTER?                       
         BZ    LR77                NO                                           
         CLC   BRDFSTTM,ENTM       IS RECD TIMES WITHIN FILTER TIMES            
         BH    LR100                                                            
         CLC   BRDFENTM,STTM                                                    
         BL    LR100               OUT OF RANGE -- DON'T DISPLAY                
*                                                                               
LR77     ZIC   RE,BRDFSTTM         START HOUR                                   
         MH    RE,=H'100'                                                       
         ZIC   RF,BRDFSTTM+1       START MINUTES                                
         AR    RE,RF                                                            
         STH   RE,FULL             START TIME (MILITARY)                        
         ZIC   RE,BRDFENTM         END HOUR                                     
         MH    RE,=H'100'                                                       
         ZIC   RF,BRDFENTM+1       END MINUTES                                  
         AR    RE,RF                                                            
         STH   RE,FULL+2           END TIME (MILITARY)                          
         GOTO1 UNTIME,DMCB,FULL,LSTTIME                                         
*                                                                               
LR80     OC    LUID,LUID           ANY LUID FILTER?                             
         BZ    *+14                NO                                           
         CLC   LUID,BRDFLUID       YES, DO THEY MATCH?                          
         BNE   LR100               NO, GET NEXT RECD                            
         MVC   LSTLUID,BRDFLUID    LUID                                         
*                                                                               
         DROP  R3                                                               
*                                                                               
LR85     CLI   MSGTYPE,C'P'        PERMANENT MESSAGE LIST?                      
         BNE   LR90                NO -- NO ROOM TO DISPLAY THE HEADING         
         LA    R3,BRDFSTEL         A(FIRST ELEMENT)                             
         USING BRDHEDD,R3                                                       
         MVI   ELCODE,BRDHEDEQ     HEADING ELEMENT CODE                         
         BAS   RE,FIRSTEL          HEADING ELEMENT MUST BE THERE                
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   LSTHEAD,BRDHEDTX    HEADING FIELD                                
         DROP  R3                                                               
*                                                                               
LR90     GOTO1 LISTMON             SEND LINE TO SCREEN                          
*                                                                               
LR100    GOTO1 SEQ                 NEXT RECORD                                  
         LA    R4,KEY              POINT R4 BACK TO KEY                         
         B     LR20                                                             
         DROP  R4                                                               
         EJECT                                                                  
SFMERROR GOTO1 SFMERR                                                           
         SPACE 3                                                                
         GETEL R3,DATADISP,ELCODE                                               
         SPACE 3                                                                
MYSPACES DC    CL132' '                                                         
         SPACE 3                                                                
COMCHARS DC    C'IDMBAOCX'         COMMAND CHARS (IN ORDER OF COMTABL)          
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE CTGENBRD                                                       
         PRINT OFF                                                              
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE FACTRY                                                         
       ++INCLUDE FASELIST                                                       
       ++INCLUDE FASYSFAC                                                       
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE FASYSLSTD                                                      
       ++INCLUDE FAPGMLST                                                       
       ++INCLUDE FACIDTABD                                                      
       ++INCLUDE FASSB                                                          
       ++INCLUDE DDPERVALD                                                      
       ++INCLUDE DDCOMFACS                                                      
         EJECT                                                                  
       ++INCLUDE CTSFMFFD                                                       
         PRINT ON                                                               
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE CTSFMF9D                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE CTSFME9D                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE CTSFMD9D                                                       
         EJECT                                                                  
       ++INCLUDE CTSFMWORKD                                                     
         SPACE 5                                                                
* PUT MY STORAGE DSECT HERE IF NEEDED                                           
*                                                                               
         ORG   SYSSPARE                                                         
ASELIST  DS    A                   A(SELIST TABLE)                              
ASYSLST  DS    A                   A(SYSLST TABLE)                              
ASEPGMS  DS    A                   A(PROGRAMS TABLE)                            
ACNTRY   DS    A                   A(COUNTRY TABLE)                             
AFACIDTB DS    A                   A(FACIDTAB)                                  
VDEJAVU  DS    A                   A(DEJAVU)                                    
MSGNUM   DS    H                   MESSAGE NUMBER                               
MSG1ST   DS    H                   1ST MESSAGE NUMBER ADDED                     
MSGTYPE  DS    C                   MESSAGE TYPE ('P' OR 'T')                    
APPLID   DS    X                   APPLICATION-ID FILTER                        
APPLTEST DS    C                   'Y' IF BROADCAST IS FOR TEST FACPAK          
OVSYSNUM DS    X                   OVERLAY SYSTEM NUMBER FILTER                 
SYSNUM   DS    X                   SPECIFIC SYSTEM NUMBER FILTER                
PROG     DS    X                   PROGRAM NUMBER FILTER                        
CNTRY    DS    X                   COUNTRY FILTER FOR LIST                      
STTM     DS    XL2                 START TIME FILTER FOR LIST                   
ENTM     DS    XL2                 END TIME FILTER FOR LIST                     
STARTDAT DS    XL2                 COMPRESSED START DATE FILTER                 
ENDDATE  DS    XL2                 COMPRESSED END DATE FILTER                   
DAYS     DS    XL7                 DAY OF WEEK FILTER                           
LUID     DS    XL8                 VTAM FILTER FOR LIST                         
OUT      DS    XL10                OUTPUT FIELD                                 
FILTERS  DS    XL16                NAME FILTER TABLE                            
SAVEKEY  DS    XL32                BROADCAST MESSAGE KEY                        
SEQNUM   DS    XL1                 TEXT LINE NUMBER                             
TXTFOUND DS    CL1                 'Y' IF TEXT LINE WAS FOUND                   
COUNTRY  DS    X                   COUNTRY CODE OF CONNECTED TERMINAL           
REPS     DS    X                   BINARY #OF REPETITIONS                       
INTERVL  DS    H                   BINARY INTERVAL                              
MYFLAG   DS    X                   FLAG FOR PERVAL                              
MYWORK   DS    CL17                WORK FOR EDIT                                
*                                                                               
         CNOP  0,4                                                              
COMTABL  DS    0AL4                SCREDIT COMMAND AREAS                        
ACOMI    DS    A(0)                                                             
ACOMD    DS    A(0)                                                             
ACOMM    DS    A(0)                                                             
ACOMB    DS    A(0)                                                             
ACOMA    DS    A(0)                                                             
ACOMO    DS    A(0)                                                             
ACOMC    DS    A(0)                                                             
COMTABX  EQU   *                                                                
         SPACE 5                                                                
* ONLINE LIST LINE                                                              
*                                                                               
GEND     DSECT                                                                  
         ORG   LISTAR                                                           
LSTMSGN  DS    CL5                                                              
         DS    CL1                                                              
LSTNAME  DS    CL8                                                              
         DS    CL1                                                              
LSTAPPL  DS    CL4                                                              
         DS    CL1                                                              
LSTSYS   DS    CL7                                                              
         DS    CL1                                                              
LSTPGM   DS    CL3                                                              
         DS    CL1                                                              
LSTCTRY  DS    CL3                                                              
         DS    CL1                                                              
LSTDATE  DS    CL17                FOR TEMPORARY MESSAGES LIST ONLY             
         DS    CL1                                                              
LSTTIME  DS    CL11                FOR TEMPORARY MESSAGES LIST ONLY             
         DS    CL1                                                              
         ORG   LSTDATE                                                          
LSTHEAD  DS    CL26                FOR PERMANENT MESSAGES LIST ONLY             
         DS    CL4                                                              
LSTLUID  DS    CL8                                                              
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'017CTSFM09   05/01/02'                                      
         END                                                                    
