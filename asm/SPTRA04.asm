*          DATA SET SPTRA04    AT LEVEL 248 AS OF 06/06/18                      
*PHASE T21604C                                                                  
*INCLUDE KHDUMMY                                                                
*INCLUDE PQPROF                                                                 
                                                                                
***********************************************************************         
* REGISTER USAGE                                                      *         
* R0 - WORK REG                                                       *         
* R1 - WORK REG                                                       *         
* R2 - WORK REG                                                       *         
* R3 - TWA PTR & WORK REG                                             *         
* R4 - WORK REG                                                       *         
* R5 - WORK REG                                                       *         
* R6 - WORK REG                                                       *         
* R7 - SVTABLE PTR                                                    *         
* R8 - SPOOL PTR - IN TBAUP, PTNLIST PTR                              *         
* R9 - SYSD PTR                                                       *         
* RA - BASE 2                                                         *         
* RB - BASE 1                                                         *         
* RC - GEND PTR                                                       *         
* RD - OS CHAINING                                                    *         
* RE - OS & WORK REG                                                  *         
* RF - OS & WORK REG                                                  *         
***********************************************************************         
         TITLE 'T21604 - COMMERCIAL INSTRUCTIONS '                              
***********************************************************************         
* NOTE THAT WHEN PROCESSING OFF-LINE THIS MODULE WILL CONSTRUCT DCBS  *         
* IN USER AREA POINTED TO BY TSPFUSER. THIS IS TO MAKE SURE THAT THE  *         
* DCBS REMAIN CORE RESIDENT FROM REQUEST TO REQUEST.                  *         
***********************************************************************         
                                                                                
***********************************************************************         
* TSAR USES ELEM AS AN I/O AREA                                       *         
*      0 - 1  LENGTH OF RECORD                                        *         
*      2 - 3  KEY - BINARY NUMBER 1 TO N                              *         
*      4      FORCEHED                                                          
*      5      SPACING                                                 *         
*      6 - N  PRINT DATA                                                        
***********************************************************************         
                                                                                
***********************************************************************         
* LEV 152 MHER MAR06/07 CHANGE SVCLIST TO ASVCLIST (RECLAIM STORAGE)  *         
* LEV 151 MNAS APR08/07 CHANGE VALILOC CALLS TO INCLUDE PARAM1 OF 0   *         
* LEV 165 MNAS FEB27/08 ADD NOROT OPTION - SUPRESS ROTATION INFO      *         
* LEV 166 MNAS FEB28/08 PRINT ALL (MORE THAN 3) CMMLS PER PATTERN     *         
* LEV 180 MNAS JUL25/08 BUG FIX:LOGIC FLAW WHEN DELETING OLDEST INSTR *         
*                       RECAP EL. NEW EL'S LARGER THAN OLD EL'S       *         
* LEV 181 MNAS JUL31/08 BUG FIX:PRINTING EXTRA CHAR FROM COMTEXT RECS *         
* LEV 182 MNAS AUG13/08 ARCHIVING NOW REPORTS                         *         
* LEV 183 MHER SEP/08   RUN INSTRUCTIONS FOR DAYPARTS W/IN EST AND    *         
*                       WITH START/END TIMES                          *         
* LEV 195 SMUR DEC17/08 UPDATE 0A2E RECS TO FIX BUYACT PROBLEM        *         
* LEV 212 SMUR APR29/10 FIX START AND END YEAR IN EDISTTDT            *         
* LEV 213 MNAS MAY13/10 BUG IN REVISION NUM IN MULT ELEM RECORDS      *         
* LEV 215 SMUR AUG06/10 FIXING LOCKING AND UNLOCKING BUGS             *         
* LEV 217 SMUR   OCT/10 FIX DISAPPEARING SUB-ELEMS IN 0A24 WHEN SUB-  *         
*                 SEQUENT AUTO GEN REQUEST IS FOR A SHORTER PERIOD AND*         
*                       DO NOT CREATE FIRST SUB-ELEM WITH BINARY ZEROS*         
* LEV 218 MNAS SEP22/10 FIX VALILOC CALLS TO STOP FACILITIES EMAILS   *         
* LEV 230 MNAS FEB28/12 FIX FAX=C ISSUE WITH PQ ENTRY (NOT AWX)       *         
*                       REPRINT ISSUE READING PATTERN W/INVERTED PRDS *         
*                       CL0357187N - ERROR READING MARKET GROUPS                
* LEV 231 SMUR APR23/12 GENERATE SPOT SEED REQUEST ONLY ONCE          *         
* LEV 232 SMUR JUN12/12 CREATE TBA RECS FOR TBUYS FOR MISSED STATIONS *         
* LEV 233 SMUR MAY07/12 CREATE BIGGER X'05' ELEMS ON TBA RECORDS      *         
* LEV 234 SMUR JUN25/12 FIX RERUN AND REPRINT OPTIONS                 *         
* LEV 235 MNAS SEP04/12 INCREASE PRODUCT TABLE CAPACITY               *         
*                       BUG FIX FOR DETECTING INCOMPLETE PATTERNS     *         
* LEV 236 MNAS OCT23/12 MORE BANDS                                    *         
* LEV 237 SMUR MAY22/13 CHANGE DTLIST FROM 3200 TO 3340               *         
* LEV 241 SMUR JAN05/16 NEW BAND FOR IHEART RADIO CM                  *         
* LEV 247 SMUR MAR08/18 FIX REVISION NUMBER ON AUTO GEN SPEC-21233    *         
* LEV 248 SMUR JAN20/18 SAVE ESTIMATE ON SHIP RECORD SPEC-19499 (18.2)*         
*                                                                     *         
***********************************************************************         
                                                                                
*&&TRC   SET   N                                                                
                                                                                
T21604   CSECT                                                                  
         PRINT NOGEN                                                            
FAXMAX   EQU   42                  MAX LINES/PAGE FOR LANDSCAPE FAX             
*                                  EASYLINK INSERTS LINES, SO DON'T             
*                                  INCREASE THIS NUMBER UNLESS YOU              
*                                  LIKE PRINTING BLANK PAGES!                   
         NMOD1 0,T21604,RA,RR=R3                                                
*                                                                               
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
*                                                                               
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
*                                                                               
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         ST    R3,SPTR04RR         SAVE RELOCATION VALUE                        
*                                                                               
         ST    RC,SVADGEND                                                      
*                                                                               
         L     R3,ATWA                                                          
         USING T216FFD,R3                                                       
*                                                                               
         MVC   TRACEIT+7(1),MODE                                                
         GOTO1 DATAMGR,DMCB,=C'DMTRACE',=C'DATA',TRACEIT                        
         B     GEN00                                                            
TRACEIT  DC    AL1(10),CL10'04MODE= '                                           
*                                                                               
GEN00    CLI   OFFLINE,C'Y'                                                     
         BNE   GEN10                                                            
         L     RE,ATWA                                                          
         L     RE,TWADCONS-T216FFD(,RE)                                         
         L     RE,TSPFUSER-TWADCOND(,RE) GET DCB ADDRESS                        
         OC    0(100,RE),0(RE)           TEST FIRST TIME (NMOD PRESENT)         
         BNZ   DCBOK                                                            
*                                                                               
         L     RF,=A(TR04FIL)                                                   
         MVC   0(96,RE),0(RF)     MOVE TR04FIL DCB                              
*                                                                               
         L     RF,=A(TR07AMS)                                                   
         MVC   128(96,RE),0(RF)     MOVE DCB DATA                               
*                                                                               
         L     RF,=A(ERRFILE)                                                   
         TM    WHEN,X'20'           TEST SOON                                   
         BZ    *+10                 NO, LEAVE NAME                              
         MVC   40(8,RF),=CL8'TALWRK'                                            
         MVC   256(96,RE),0(RF)     MOVE ERRFILE DCB                            
*                                                                               
         L     RF,=A(TSARFILE)                                                  
         TM    WHEN,X'20'           TEST SOON                                   
         BZ    *+10                 NO, LEAVE NAME                              
         MVC   40(8,RF),=CL8'RANKWK'                                            
         MVC   384(96,RE),0(RF)    MOVE TSARFILE DCB                            
*                                                                               
DCBOK    ST    RE,ATR04FIL         SET DCB ADDRESSES                            
         LA    RE,128(RE)                                                       
         ST    RE,ATR07AMS                                                      
         LA    RE,128(RE)                                                       
         ST    RE,AERRFILE                                                      
         LA    RE,128(RE)                                                       
         ST    RE,ATSARFIL                                                      
*                                                                               
GEN10    DS    0H                                                               
         CLI   MODE,VALKEY                                                      
         BNE   GEN20                                                            
*                                                                               
         CLI   TRAWRKRH+5,0        TEST ANY WORKER FILENUM (OPTICA)             
         BE    GEN10X              NO                                           
         OI    GENSTAT7,GES7PDF    SET FLAG TO RETURN ON ERROR                  
                                                                                
* OPEN TSARFILE TO COPY REPORT IF NOT ALREADY OPEN                              
                                                                                
         L     R2,ATSARFIL                                                      
         TM    48(R2),X'10'        TEST OPEN                                    
         BO    GEN10X                                                           
         OPEN  ((R2),OUTPUT)                                                    
         LTR   RF,RF                                                            
         JNZ   *+2                                                              
         EJECT                                                                  
*================================================================               
* PROCESS VALKEY ONLY IF ONLINE REQUEST FOR OFFLINE PROCESSING                  
*================================================================               
                                                                                
GEN10X   MVI   PQSW,1              SUPPRESS AUTO PRTQUE OPEN                    
         MVI   COVGENSW,0          SET OFF COV GEN REQ SW                       
*                                                                               
         MVI   SVQLTYP1,0                                                       
         MVI   SVFAXARC,0                                                       
         MVI   SVQLTYP2,0                                                       
         MVI   SVFAXAR2,0                                                       
         XC    ELEM,ELEM                                                        
         XC    PROFKEY,PROFKEY                                                  
         MVI   PROFKEY,C'S'                                                     
         MVC   PROFKEY+1(2),=C'TA'                                              
         MVC   PROFKEY+3(2),TWAORIG                                             
         GOTO1 =V(PQPROF),DMCB,(X'80',PROFKEY),(0,ELEM),ACOMFACS,RR=SPTC        
               R04RR                                                            
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R4,ELEM                                                          
         USING REMOTED,R4                                                       
         MVC   SVQLTYP1,REMOTTY1                                                
         MVC   SVQLARC,REMOTARC                                                 
                                                                                
         CLI   REMOTSUB,C'#'                                                    
         BNE   GEN12                                                            
         CLI   REMOTSUB+1,C'N'                                                  
         BE    GEN12A                                                           
         CLI   REMOTSUB+1,C'A'                                                  
         BNE   GEN12                                                            
         OI    SVFAXARC,REMOTARQ                                                
         B     GEN12A                                                           
GEN12    OI    SVFAXARC,REMOTAEQ                                                
                                                                                
GEN12A   DS    0H                                                               
         XC    ELEM,ELEM                                                        
         XC    PROFKEY,PROFKEY                                                  
         MVI   PROFKEY,C'S'                                                     
         MVC   PROFKEY+1(2),=C'TJ'                                              
         MVC   PROFKEY+3(2),TWAORIG                                             
         GOTO1 =V(PQPROF),DMCB,(X'80',PROFKEY),(0,ELEM),ACOMFACS,RR=SPTC        
               R04RR                                                            
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R4,ELEM                                                          
         USING REMOTED,R4                                                       
         MVC   SVQLTYP2,REMOTTY1                                                
                                                                                
         CLI   REMOTSUB,C'#'                                                    
         BNE   GEN13                                                            
         CLI   REMOTSUB+1,C'N'                                                  
         BE    GEN13A                                                           
         CLI   REMOTSUB+1,C'A'                                                  
         BNE   GEN13                                                            
         OI    SVFAXAR2,REMOTARQ                                                
         B     GEN13A                                                           
GEN13    OI    SVFAXAR2,REMOTAEQ                                                
GEN13A   DS    0H                                                               
         DROP  R4                                                               
*                                                                               
         CLI   OFFLINE,C'Y'        TEST OFFLINE PROCESSING                      
         BE    EXIT                YES                                          
*                                                                               
         TM    WHEN,X'38'          ONLINE - TEST OVERNIGHT/SOON                 
         BNZ   GEN30               YES - GO VALIDATE KEY AND EXIT               
*                                                                               
GEN20    CLI   MODE,EMPTYHK        TEST NO DATA GENERATED ERROR                 
         JNE   GEN22               YES - PRINT TWA AND SAY NO DATA              
*                                                                               
         TM    WHEN,X'20'          TEST SOON                                    
         JZ    EXIT                                                             
*                                                                               
         CLI   OFFLINE,C'Y'        AND OFFLINE                                  
         BNE   GEN20X               NO                                          
*                                                                               
* UNLOCK USING LOCKET *                                                         
*                                                                               
         MVI   DUB,X'E4'                     U=UNLOCK                           
         MVC   DUB+1(7),=X'030A220A240A25' 03=3 ENTRIES                         
*                                       0A22, 0A24, 0A25 RECS                   
         GOTO1 VALILOC,0                                                        
*                                                                               
GEN20X   CLI   TRAWRKR,C' '        TEST OPTICA (TEST FOR FILENUM)               
         JNH   EXIT                NO - EXIT                                    
*                                                                               
         CLI   PQSW,2              TEST PRTQUE OPEN                             
         BE    GEN21               YES                                          
*                                                                               
         ICM   RE,15,TWAMASTC                                                   
         BNZ   *+6                                                              
         DC    H'0'                                                             
         L     RF,MCVREMOT-MASTD(RE)                                            
         USING REMOTED,RF                                                       
         MVC   REMOTSYS,MCS2OSYS-MASTD(RE)                                      
         MVI   REMOTCPY,1                                                       
         MVI   REMOTCLS,C'G'                                                    
         MVC   REMOTJID,=C'PDF'                                                 
         MVC   REMOTDST,TWAORIG                                                 
         GOTO1 OPENPQ                                                           
         MVI   PQSW,2                                                           
*                                                                               
GEN21    BRAS  RE,SNDOPTIC         SEND EDICT CONTROL CARDS                     
*                                                                               
         GOTO1 REQTWA,DMCB,(3,(R3)),(X'FF',ACOMFACS),VPRINT                     
*                                                                               
         MVI   LINE,2                                                           
         MVI   FORCEHED,C'N'                                                    
*                                                                               
         GOTO1 TWAVPRNT,DUB,P,=C'BL01'                                          
         BASR  RE,RF                                                            
*                                                                               
         MVCDD P+3(#DSK02LQ),GE#DSK02 JOB RAN TO NORMAL COMPLETION              
         BASR  RE,RF                                                            
*                                                                               
         MVCDD P+3(#DSK01LQ),GE#DSK01 THERE WAS NOTHING ACTIVE TO REP           
         BASR  RE,RF                                                            
*                                                                               
         MVC   P+1(26),=C'*** END OF DDS MESSAGE ***'                           
         BASR  RE,RF                                                            
         J     EXIT                                                             
*                                                                               
GEN22    CLI   MODE,ERRHOOK                                                     
         JNE   GEN25                                                            
         CLI   SVXFROV,7                                                        
         JE    GEN24                                                            
         J     EXIT                                                             
                                                                                
* RETURN ERROR CODE TO AUTO/GEN                                                 
                                                                                
GEN24    DS    0H                                                               
*                                                                               
         TM    WHEN,X'20'          THIS SOON REQUEST                            
         BZ    GEN24X               NO                                          
         CLI   OFFLINE,C'Y'        AND OFFLINE                                  
         BNE   GEN24X               NO                                          
*                                                                               
* UNLOCK USING LOCKET *                                                         
*                                                                               
         MVI   DUB,X'E4'                     U=UNLOCK                           
         MVC   DUB+1(7),=X'030A220A240A25' 03=3 ENTRIES                         
*                                       0A22, 0A24, 0A25 RECS                   
         GOTO1 VALILOC,0                                                        
*                                       0A22, 0A24, 0A25 RECS                   
GEN24X   L     RF,SYSPARMS                                                      
         L     RF,16(RF)                                                        
         ICM   RF,15,CGLOBBER-COMFACSD(RF)                                      
         JZ    *+2                                                              
         XC    ELEM,ELEM                                                        
         MVC   ELEM(6),=C'<9997>'                                               
         MVC   ELEM+6(60),CONHEAD                                               
         GOTO1 (RF),DMCB,=C'PUTD',ELEM,69,GLVBUY1                               
* NOW BUILD RETURN XFRCTL                                                       
         XC    ELEM,ELEM                                                        
         LA    R5,ELEM                                                          
         USING GLVXFRSY,R5                                                      
         MVC   GLVXFRSY,=C'STR'                                                 
         MVC   GLVXFRPR,=C'TRA'                                                 
         MVC   GLVXTOSY,=C'SPO'                                                 
         MVC   GLVXTOPR,=C'LIN'                                                 
         MVI   GLVXFLG1,GLV1RETN+GLV1SIDR+GLV1SEPS+GLV1SIDE                     
         MVC   GLVXSESR(2),SVXFRSID                                             
         GOTO1 (RF),(R1),,ELEM,24,GLVXCTL                                       
         DROP  R5                                                               
         J     EXIT                                                             
*                                                                               
GEN25    CLI   MODE,PRINTREP                                                    
         BNE   GENAUTO                                                          
*                                                                               
* SET UP COMMON ON-LINE ADDRESSABILITY TO SUBROUTINES IN THIS MODULE *          
*                                                                               
GEN30    STM   R8,RC,SPTR04R8      SAVE REGISTERS                               
*                                                                               
         LA    R1,INSNMOD                                                       
         SR    R4,R4                                                            
         LA    R5,GENVCONS                                                      
         LA    R6,GENVCNT                                                       
*                                                                               
GEN40    ST    R1,0(R5)                                                         
         STC   R4,0(R5)                                                         
         LA    R4,4(R4)                                                         
         LA    R5,4(R5)                                                         
         BCT   R6,GEN40                                                         
*                                                                               
         XC    TSARCT,TSARCT                                                    
         XC    TSARBYTE,TSARBYTE                                                
         XC    ATSAR,ATSAR                                                      
         XC    TSARWK,TSARWK                                                    
*                                                                               
         LAY   R0,INSPRT                                                        
         ST    R0,VPRT                                                          
*                                                                               
         XC    DMCB(24),DMCB                                                    
         MVC   DMCB+4(4),=X'D9000AFE'    GET ADDRESS OF TRPACK                  
         GOTO1 CALLOV,DMCB                                                      
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   VTRPACK,0(R1)                                                    
                                                                                
*==============================================================                 
* ALLOCATE STORAGE FOR ON-LINE PROCESSING                                       
*==============================================================                 
                                                                                
         LA    RE,ENDSVTB                                                       
         ST    RE,ASVTABLX                                                      
         LA    RE,DYNAWORK         GET END OF ALLOCATED WORK                    
         ST    RE,ASVPRDS          SET AS A(SVPRD TABLE)                        
         BCTR  RE,0                                                             
         LA    RE,L'SVPRDDTA*50(RE)  ALLOW 50 ENTRIES                           
         ST    RE,APTNSTRT         AND SET AS PTTN TABLE ADDR                   
*--->                                                                           
*---> NOTE THAT FOR SPOT/LINK STATION LIST BUILD, THIS ADDRESS                  
*---> WILL BE MODIFIED IN SPTRA07 AFTER GLOBAL HAS BEEN READ                    
*--->                                                                           
         LA    RE,SVTABLE                                                       
         ST    RE,ASVTABLE         INITIALIZE ADCON                             
                                                                                
*==============================================================                 
* ALLOCATE ADDITIONAL STORAGE IN T2168C                                         
*==============================================================                 
                                                                                
GEN42    XC    DMCB(24),DMCB                                                    
         MVI   DMCB,X'8C'          SET OVERLAY NUMBER                           
         GOTO1 CALLOV,DMCB,,ATWA                                                
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     RE,0(R1)            GET PHASE ADDRESS                            
         LA    RE,16(RE)           CAN USE FROM +16                             
         MVC   0(8,RE),=C'*PRTSAVE'                                             
         LA    RE,8(RE)                                                         
         ST    RE,APRTSAVE         FIRST 528 BYTES SAVE P1-P4                   
*                                                                               
         LA    RE,560(RE)                                                       
         MVC   0(8,RE),=C'DATELIST'                                             
         LA    RE,8(RE)                                                         
         ST    RE,ADTLIST                                                       
*NOP     LA    RE,3200(RE)         ALLOW 8*400 BYTES                            
*    LOOK AT BLDDT32 (THE MATH DOES NOT ADD UP)                                 
         LA    RE,3340(RE)                                                      
         ST    RE,ADTLISTX                                                      
*                                                                               
         MVC   0(8,RE),=CL8'*SVCMLS*'                                           
         LA    RE,8(RE)                                                         
         ST    RE,ASVCMLS                                                       
*                                                                               
         L     RE,0(R1)                                                         
         LA    RE,0(RE)            CLEAR HOB                                    
         A     RE,8(RE)            ADD LENGTH TO PHASE ADDRESS                  
         ST    RE,ASVCMLX          SET EOT ADDR                                 
*                                                                               
         CLI   OFFLINE,C'Y'                                                     
         BNE   GENOV                                                            
*                                                                               
         LA    R0,1                                                             
         STH   R0,SEQNUM           RESET SEQUENCE PAGE NUM                      
         EJECT                                                                  
*=========================================================                      
* OPEN ERRFILE FOR FOR OFFLINE ERROR MESSAGES                                   
*=========================================================                      
                                                                                
         L     R2,AERRFILE                                                      
         TM    48(R2),X'10'        TEST ALREADY OPEN                            
         BO    GEN50               YES                                          
         OPEN  ((R2),OUTPUT)                                                    
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         EJECT                                                                  
* REALLOCATE DYNAMIC LISTS FOR OFFLINE PROCESSING *                             
*                                                                               
GEN50    LR    RE,RB                                                            
*                                                                               
* NOW USE AREA WITH ADDRESS IN VADUMMY IN GEND                                  
*                                                                               
         L     RE,VADUMMY                                                       
         MVC   0(8,RE),=C'*SVTABLE'                                             
         LA    RE,8(RE)                                                         
         ST    RE,ASVTABLE                                                      
         AHI   RE,12000            SVTABLE = 300 X 40 BYTE ENTRIES              
         ST    RE,ASVTABLX                                                      
*                                                                               
         LA    R0,48                                                            
         L     R1,ASVTABLE                                                      
         XC    0(250,R1),0(R1)                                                  
         LA    R1,250(R1)                                                       
         BCT   R0,*-10                                                          
*                                                                               
         MVC   0(8,RE),=C'*SVPRDS*'                                             
         XC    8(8,RE),8(RE)                                                    
         LA    RE,16(RE)           ALLOW ROOM FOR SAVE A-M/CLT                  
         ST    RE,ASVPRDS                                                       
*                                                                               
         AHI   RE,L'SVPRDDTA*220   220 ENTRIES                                  
         MVC   0(8,RE),=CL8'*PTNSTRT'                                           
         LA    RE,8(RE)                                                         
         ST    RE,APTNSTRT                                                      
         AHI   RE,24000                                                         
*                                                                               
         MVC   0(8,RE),=CL8'*SHPLST*'                                           
         LA    RE,8(RE)                                                         
         ST    RE,ASHPLIST                                                      
         A     RE,=F'40000'                                                     
         ST    RE,ASHPLSTX                                                      
*                                                                               
         MVC   0(8,RE),=CL8'**SVP***'                                           
         LA    RE,8(RE)                                                         
         ST    RE,APRTSAVE                                                      
         AHI   RE,528                                                           
*                                                                               
         MVC   0(8,RE),=C'DATELIST'                                             
         LA    RE,8(RE)                                                         
         ST    RE,ADTLIST                                                       
         AHI   RE,3340                                                          
         ST    RE,ADTLISTX                                                      
*                                                                               
         MVC   0(8,RE),=CL8'*SVCMLS*'                                           
         LA    RE,8(RE)                                                         
         ST    RE,ASVCMLS                                                       
         EJECT                                                                  
GENOV    XC    DMCB(24),DMCB                                                    
         MVC   DMCB(1),RECNUM                                                   
         GOTO1 CALLOV,DMCB,,ATWA                                                
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
* ALL INSTS ARE QUEUED UP AND FORMATTED BY 07                                   
* AND RETURNS HERE WHEN DONE                                                    
*                                                                               
         L     RF,0(R1)                                                         
         GOTO1 (RF),(R1),(RC)                                                   
*                                                                               
         CLI   SVXFROV,7           TEST RETURN CONTROL TO SPOT/LINK             
         JNE   GEN52                                                            
*                                                                               
         CLC   =C'SUB',SVXFRMOD                                                 
         JE    GENOVX                                                           
*                                                                               
         BRAS  RE,WRTSVTAB         ADD 0D7F RECORD IF BLD MODE                  
                                                                                
* UPDATE THE GLVSPTRF ELEM WITH SEQNUM/DATE OF 0A7F REC                         
                                                                                
         L     RF,ACOMFACS                                                      
         L     RF,CGLOBBER-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,=C'GETD',ELEM,24,GLVSPTRF                              
*                                                                               
         L     RE,ATIA                                                          
         LA    RE,XSAVSEQ-XSAVKEY(RE) POINT TO SEQNUM/DATE IN KEY               
         LA    R1,ELEM                                                          
         USING GLVTRFD,R1                                                       
         MVC   TRFSVKEY(8),0(RE)      MOVE SEQ/DATE/A-M/CLT                     
*                                                                               
         GOTO1 (RF),DMCB,=C'PUTD'                                               
         DROP  R1                                                               
                                                                                
* AND NOW TRANSFER CONTROL BACK TO SPOT/LINK                                    
                                                                                
GENOVX   XC    ELEM,ELEM                                                        
         LA    R1,ELEM                                                          
         USING GLVXFRSY,R1                                                      
         MVC   GLVXTOSY,=C'SPO'                                                 
         MVC   GLVXTOPR,=C'LIN'                                                 
         MVC   GLVXFRSY,=C'STR'                                                 
         MVC   GLVXFRPR,=C'TRA'                                                 
         MVI   GLVXFLG1,GLV1RETN+GLV1SIDR+GLV1SEPS+GLV1SIDE+GLV1SEPD            
         MVC   GLVXSESR(2),SVXFRSID                                             
*                                                                               
         L     RF,ACOMFACS                                                      
         L     RF,CGLOBBER-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,=C'PUTD',ELEM,GLVXLENQ,GLVXCTL                         
*                                                                               
         CLC   =C'SUB',SVXFRMOD                                                 
         JNE   EXIT                                                             
         J     GEN65X              GO LOCK RECORDS                              
         DROP  R1                                                               
         EJECT                                                                  
* OFFLINE REPORTS NEED TO PRINT ERROR LISTS *                                   
*                                                                               
GEN52    CLI   OFFLINE,C'Y'                                                     
         BNE   GEN60                                                            
*                                                                               
* SOON JOBS CALL LOCKET TO UNLOCK PATTERNS, INSTR AND SHIPPING RECAPS *         
*                                                                               
         TM    WHEN,X'20'          OFFLINE - TEST SOON                          
         BZ    GEN56                NO, GO TO ERROR REPORTS                     
*                                                                               
         CLI   SVAMSAUT,C'Y'       AMS TO FOLLOW                                
         BE    GEN54               YES, DO NOT UNLOCK JUST YET                  
*                                                                               
         CLI   SVAMSAUT,C'X'       AMS REQUESTED                                
         BE    GEN54               YES, DO NOT UNLOCK JUST YET                  
*                                                                               
         MVC   DUB,=X'E4030A220A240A25' U=UNLOCK, 03=3 ENTRIES,                 
*                                       0A22, 0A24, 0A25 RECS                   
         TM    SVOPT,OPTTEST       TEST TEST RUN                                
         BO    GEN54                                                            
         TM    SVOPT3,OPT3REPT     THIS A REPRINT                               
         BO    GEN54                                                            
         GOTO1 VALILOC,0                                                        
*                                                                               
* IF SOON, AND FAXING, NO ERROR REPORT, NO COPIES POSSIBLE *                    
*                                                                               
GEN54    CLI   SVTWPR1,C'Y'                                                     
         BE    EXIT                                                             
         CLI   SVTWPR1,C'2'                                                     
         BE    EXIT                                                             
                                                                                
* CLOSE ERRFILE *                                                               
                                                                                
GEN56    L     R2,AERRFILE                                                      
         CLOSE ((R2),)                                                          
         FREEPOOL (R2)                                                          
*                                                                               
* FREEMAIN MEMORY IF ANY WAS USED                                               
*                                                                               
         ICM   R0,15,TSARBUFL      WAS MEMORY ALLOCATED                         
         BZ    GEN57                                                            
         L     R0,TSARBUFL                                                      
         L     R1,TSARBUFF                                                      
         FREEMAIN RC,LV=(0),A=(1)                                               
         LTR   RF,RF                                                            
         JNZ   *+2                                                              
*                                                                               
GEN57    ICM   R1,15,AWKBUFF       TEST GETMAINED WKBUFF                        
         JZ    GEN58                                                            
         L     R0,=A(14*1024)                                                   
         FREEMAIN RC,LV=(0),A=(1)                                               
         LTR   RF,RF                                                            
         JNZ   *+2                                                              
*                                                                               
*  CALL OVLY 97 TO PRINT ERROR LIST *                                           
*                                                                               
GEN58    DS   0H                                                                
*                                                                               
         CLI   OFFLINE,C'Y'                                                     
         JNE   GEN59X                                                           
*                                                                               
GEN59X   LA    R1,DMCB                                                          
         L     RE,=V(DUMMY)                                                     
         A     RE,SPTR04RR                                                      
         SR    RE,RE                                                            
         ST    RE,0(R1)                                                         
         MVC   4(4,R1),=X'D9021697'                                             
*                                                                               
         GOTO1 CALLOV              CALL OVERLAY TO PROCESS                      
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     RF,0(R1)                                                         
         GOTO1 (RF),(R1),(RC)                                                   
                                                                                
         J     EXIT                                                             
*=========================================================                      
*                                                                               
* ONLINE SOON REQUESTS CALL LOCKET TO LOCK *                                    
* PATTERNS, INSTR AND SHIPPING RECAPS      *                                    
*                                                                               
*=========================================================                      
                                                                                
GEN60    TM    WHEN,X'20'          ONLINE - TEST SOON                           
         JZ    EXIT                 NO, EXIT                                    
                                                                                
         CLC   AGENCY,=C'OU'                                                    
         BNE   GEN65X                                                           
         TM    WHEN,X'20'                                                       
         BZ    GEN65X                                                           
         CLC   QCLT,=C'PQ2'                                                     
         BE    GEN65A                                                           
         CLC   QCLT,=C'EFI'                                                     
         BE    GEN65A                                                           
         CLC   QCLT,=C'NC3'                                                     
         BE    GEN65A                                                           
         B     GEN65X                                                           
                                                                                
GEN65A   CLC   QPRD,=C'POL'                                                     
         BE    GEN65E                                                           
         B     GEN65X                                                           
                                                                                
GEN65E   LA    R2,CONWHENH                                                      
         MVI   ERROR,INVPRINT                                                   
         B     GENERR                                                           
GEN65X   DS    0H                                                               
                                                                                
         TM    SVOPT,OPTTEST       TEST TEST RUN                                
         BO    GEN65Z                                                           
         TM    SVOPT3,OPT3REPT     THIS A REPRINT                               
         BO    GEN65Z                                                           
*                                                                               
         MVC   DUB,=X'D3030A220A240A25' L=LOCK, 03=3 ENTRIES                    
*                                       0A22, 0A24, 0A25 RECS                   
         GOTO1 VALILOC,0                                                        
         MVI   TWAWHEN,5           SET AS UPDATIVE SOON FOR FRED                
                                                                                
GEN65Z   CLC   =C'SUB',SVXFRMOD                                                 
         JNE   EXIT                                                             
         GOTO1 BLDREQST            BLDREQ DOES NOT RETURN                       
         DC    H'0'                                                             
         EJECT                                                                  
*==============================================================                 
* OPEN AUTOREQ FILE AT RUNFRST                                                  
*==============================================================                 
                                                                                
GENAUTO  CLI   OFFLINE,C'Y'                                                     
         BNE   EXIT                                                             
         CLI   MODE,RUNFRST                                                     
         BNE   GENAUTO6                                                         
*                                                                               
GENAUTO2 TM    WHEN,X'20'          TEST F...ING SOON                            
         BO    GENAUTO4                                                         
         L     R2,ATR04FIL         GET DCB ADDRESS                              
         OPEN  ((2),OUTPUT)                                                     
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
                                                                                
         L     R2,ATR07AMS         OPEN AMS/GEN AUTOREQ FILE                    
         OPEN  ((2),OUTPUT)                                                     
         LTR   RF,RF                                                            
         JNZ   *+2                                                              
*                                                                               
GENAUTO4 L     RE,ATWA                                                          
         MVI   29(RE),X'02'        INDICATE RUNLAST HOOK REQUIRED               
         B     EXIT                                                             
*                                                                               
GENAUTO6 CLI   MODE,RUNLAST                                                     
         BNE   EXIT                                                             
                                                                                
* CLOSE TR07AMS REQUEST FILE IF OPEN                                            
                                                                                
         L     R2,ATR07AMS                                                      
         TM    48(R2),X'10'        TEST OPEN                                    
         BZ    GENAUTO8                                                         
*                                                                               
         L     R1,ATR07AMS                                                      
         LA    R0,ELEM                                                          
         MVC   ELEM(80),SPACES     ALWAYS INSERT A /*                           
         MVC   ELEM(2),=C'/*'                                                   
         PUT   (1),(0)                                                          
*                                                                               
         CLOSE ((R2))                                                           
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R2,ATSARFIL                                                      
         TM    48(R2),X'10'        TEST FILE OPEN                               
         BZ    GENAUTO8                                                         
         CLOSE ((2),)                                                           
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
* CLOSE AUTOREQ FILE AND PROCESS AT RUNLAST *                                   
                                                                                
GENAUTO8 TM    WHEN,X'20'          TEST F...ING SOON                            
         BO    EXIT                YES - SKIP CLOSE/AUTO REQ PROC               
*                                                                               
         L     R2,ATR04FIL                                                      
         CLOSE ((2),)                                                           
         FREEPOOL (R2)                                                          
*                                                                               
*  CALL OVLY 98 TO GENERATE AUTO REQUESTS *                                     
*                                                                               
         LA    R1,DMCB                                                          
         L     RE,=V(DUMMY)                                                     
         A     RE,SPTR04RR                                                      
         SR    RE,RE                                                            
         ST    RE,0(R1)                                                         
         MVC   4(4,R1),=X'D9021698'                                             
*                                                                               
         GOTO1 CALLOV              CALL OVERLAY TO PROCESS                      
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     RF,0(R1)                                                         
         GOTO1 (RF),(R1),(RC)                                                   
*                                                                               
EXIT     MVI   ERROR,0             RESET ERROR CODE                             
*                                                                               
EQXIT    CR    RE,RE                                                            
         J     *+6                                                              
*                                                                               
NEQXIT   LTR   RE,RE                                                            
*                                                                               
EXITX    XIT1                                                                   
*                                                                               
GENERR   CLI   ERROPT,C'Y'                                                      
         JE    EXITX                                                            
         GOTO1 ERREX                                                            
*                                                                               
GENERR2  CLI   ERROPT,C'Y'                                                      
         JE    EXITX                                                            
ERREXIT2 GOTO1 ERREX2                                                           
         EJECT                                                                  
* PROVIDE COMMON ENTRY POINT TO SUBROUTINES *                                   
*                                                                               
INSNMOD  NTR1  BASE=SPTR04RB                                                    
         LM    R8,RC,SPTR04R8      RESTORE REGS                                 
         L     R3,ATWA                                                          
*                                                                               
         SRL   RF,24                                                            
         AR    RF,RF               X 2                                          
         B     GENBRTAB(RF)                                                     
*                                                                               
GENBRTAB BRAS  RE,CHKFLT                                                        
         J     EXITX                                                            
         BRAS  RE,GETPTNS                                                       
         J     EXITX                                                            
         BRAS  RE,BLDLIST                                                       
         J     EXITX                                                            
         BRAS  RE,BLDDATES                                                      
         J     EXITX                                                            
         DC    2AL4(0)             KEEP INDEX POSITIONS                         
         BRAS  RE,BOXER                                                         
         J     EXITX                                                            
         BRAS  RE,PCOPY                                                         
         J     EXITX                                                            
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*================================================================               
* SUBROUTINE DETERMINES FLIGHT DATES FOR GIVEN TELECAST DATES                   
*================================================================               
                                                                                
         USING SVTABLED,R7                                                      
*                                                                               
CHKFLT   NTR1  BASE=*,LABEL=*                                                   
         MVI   ERROR,NOFLTREC                                                   
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0A27'                                                  
         MVC   KEY+2(3),BAGYMD     A-M/CLT                                      
         CLI   BPRD,X'FF'          TEST POL INST                                
         BE    *+10                                                             
         MVC   KEY+5(1),BPRD       TRY FOR PRODUCT SPECIFIC RECORD              
*                                                                               
         MVC   SVKEY,KEY           SAVE KEY FOR COMPARE                         
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(6),KEYSAVE                                                   
         BE    CHKF10                                                           
         MVC   KEY,KEYSAVE         RESTORE ORIGINAL KEY                         
         MVI   KEY+5,0             CLEAR PRD TO TRY FOR CLT DATA                
*                                                                               
         MVC   SVKEY,KEY           SAVE KEY FOR COMPARE                         
         GOTO1 HIGH                                                             
*                                                                               
CHKF10   CLC   KEY(6),KEYSAVE                                                   
         BNE   CHKF12                                                           
         GOTO1 DATCON,DMCB,(3,KEY+6),(2,FULL)                                   
         CLC   SVTBSTR,FULL        FIRST TLCST DATE TO RECORD END DATE          
         BNH   CHKF14                                                           
         GOTO1 SEQ                                                              
         B     CHKF10                                                           
*                                                                               
CHKF12   DS   0H                                                                
         CLI   SVKEY+5,0           IS THIS BY PRODUCT                           
         JE    GENERR               NO, ERROR                                   
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0A27'                                                  
         MVC   KEY+2(3),BAGYMD     A-M/CLT                                      
         MVC   SVKEY,KEY           SAVE KEY FOR COMPARE                         
         GOTO1 HIGH                                                             
         B     CHKF10                                                           
*                                                                               
CHKF14   DS    0H                                                               
         XC    SVKEY,SVKEY                                                      
         L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         GOTO1 GETREC                                                           
*                                                                               
         MVI   ERROR,NOFLTEL                                                    
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         B     *+8                                                              
*                                                                               
CHKF16   BRAS  RE,NEXTEL                                                        
         JNE   GENERR                                                           
*                                                                               
         USING FLTDTAEL,R6                                                      
*                                                                               
         GOTO1 DATCON,DMCB,(3,FLTSTART),(2,FULL)                                
         GOTO1 (RF),(R1),(3,FLTEND),(2,FULL+2)                                  
*                                                                               
         OC    SVTBEND,SVTBEND     TEST END DATE GIVEN                          
         BZ    CHKF20                                                           
*                                                                               
         MVI   ERROR,FLTOVLAP                                                   
         CLC   SVTBSTR,FULL+2      FIRST TLCST AFTER FLIGHT END                 
         BH    CHKF16                                                           
         CLC   SVTBEND,FULL        LAST TLCST BEFORE FLIGHT START               
         BL    CHKF16                                                           
         EJECT                                                                  
* TELECAST DATES SHOULD FALL ENTIRELY WITHIN THIS FLIGHT *                      
*                                                                               
         CLC   SVTBEND,FULL+2      LAST TLCST DATE TO FLT END                   
         JH    GENERR                                                           
         CLC   SVTBSTR,FULL                                                     
         JL    GENERR                                                           
         MVC   SVGENDTS,FULL       SAVE FLT START/END DATES                     
         MVC   SVFLTDTS,FULL                                                    
         CLC   SVTBEND,FULL+2      TEST LAST TLCST = FLIGHT END                 
         BNE   CHKFX                                                            
         MVC   SVGENST,SVTBSTR     SAVE FLIGHT START                            
         B     CHKFX                                                            
*                                                                               
* ONLY ONE DATE GIVEN - MATCH FLIGHT START DATE *                               
*                                                                               
CHKF20   CLC   SVTBSTR,FULL                                                     
         BNE   CHKF16                                                           
*                                                                               
         MVC   SVGENDTS,FULL                                                    
         MVC   SVFLTDTS,FULL                                                    
*                                                                               
         MVC   SVTBEND,SVGENEND                FORCE END DATE                   
         GOTO1 DATCON,DMCB,(2,SVTBEND),SVQEND  AND REQ END DATE                 
*                                                                               
CHKFX    DS   0H                                                                
         J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
*============================================================                   
* SUBROUTINE TO READ PATTERN RECORDS FOR SVTABLE ENTRIES                        
*============================================================                   
                                                                                
GETPTNS  NTR1  BASE=*,LABEL=*                                                   
         L     R7,ASVTABLE                                                      
         USING SVTABLED,R7                                                      
*                                                                               
         XC    DPTLIST,DPTLIST     CLEAR DPTLIST NOW!                           
         MVI   DLYFLAG,C'N'        RESET DAILY TIMES FLAG                       
*                                                                               
         NI    UPDSW,X'FF'-UPDPGSW PIGGYBACK SWITCH                             
*                                                                               
         L     R4,APTNSTRT                                                      
         ST    R4,NEXTADDR         INTL PATTERN LIST POINTER/COUNTER            
         USING PTNLISTD,R4                                                      
*                                                                               
GETP10   ST    R4,SVTBAPTN             SET PTN LIST ADDRESS IN SVTAB            
         XC    0(L'PTNLIST+1,R4),0(R4) CLEAR ONE ENTRY + 1 BYTE                 
*                                                                               
         CLI   SVTBPRD2,0                                                       
         BE    *+8                                                              
         OI    UPDSW,UPDPGSW       PIGGYBACK PRODUCT PRESENT                    
*                                                                               
         TM    SVOPT3,OPT3REPT     THIS A REPRINT                               
         BO    GETP50              YES - GO READ INST RECAP RECS                
*                                                                               
         XC    KEY,KEY             READ PATTERN RECORDS                         
         MVC   KEY(2),=X'0A22'                                                  
         MVC   KEY+2(3),BAGYMD     A-M/CLT                                      
         MVC   KEY+5(4),SVTBPRD    PRD/SLN  PTR/SLN                             
         MVC   KEY+9(1),SVTBCOPY   COPY CODE                                    
*                                                                               
GETP12   GOTO1 HIGH                                                             
         B     GETP16                                                           
*                                                                               
GETP14   SR    RE,RE                                                            
         ICM   RE,7,KEY+10         GET REF/SUBL                                 
         SRL   RE,10               DROP SUBLINE                                 
         LA    RE,1(RE)            BUMP LINE NUMBER                             
         SLL   RE,10                                                            
         STCM  RE,7,KEY+10                                                      
         OC    KEY+10(3),KEY+10    TEST REACHED END                             
         BZ    GETP30                                                           
         GOTO1 HIGH                                                             
*                                                                               
GETP16   CLC   KEY(10),KEYSAVE                                                  
         BNE   GETP30                                                           
*                                                                               
*        TM    KEY+13,X'02'        INCOMPLETE RECORD ?                          
*        BO    GETP14              NO CMLS ON RECORD, BYPASS                    
*                                                                               
         TM    SVOPT3,OPTBPAT      WAS OPTION BPAT ENTERED                      
         BZ    GETP18              NO, SHOW PATTERN RECS ONLY                   
         TM    KEY+13,X'01'        IS THIS BPAT REC                             
         BZ    GETP14                                                           
         B     GETP20                                                           
*                                                                               
GETP18   TM    KEY+13,X'01'        BPAT RECORD?                                 
         BO    GETP14               YES,BYPASS                                  
*                                                                               
GETP20   L     R6,AIO1             SET I/O AREA ADDRESS                         
         ST    R6,AIO                                                           
         GOTO1 GETREC                                                           
*                                                                               
         TM    15(R6),X'02'        INCOMPLETE RECORD ?                          
         BO    GETP14              BYPASS PATTERNS WITH NO COMMERCIALS          
*                                                                               
         GOTO1 VBLDLIST            BUILD PATTERN TABLE ENTRY                    
         B     GETP14                                                           
*                                                                               
GETP30   DS    0H                                                               
         CLI   SVTBCOPY,0          ANY COPY EXPECTED                            
         BE    GETP32               NO                                          
         CLI   SVPROF11,C'E'       THIS COPY CODE = EST                         
         BE    GETP32               MUST HAVE CODED PTTNS                       
*                                                                               
         CLI   KEYSAVE+PATKCODE-PATKEY,0 ALREADY DONE NON-COPY CODED            
         BE    GETP32                                                           
         XC    KEY,KEY                                                          
         MVC   KEY(9),KEYSAVE                                                   
         B     GETP12                                                           
*                                                                               
GETP32   DS    0H                                                               
         GOTO1 VBLDDATE            BUILD PATTERN DATE LIST                      
         CLI   ERROR,0                                                          
         JNE   NEQXIT                                                           
*                                                                               
GETP34   L     R4,NEXTADDR         GET RESET PTTN LIST END                      
         LA    R7,L'SVTBDATA(R7)                                                
         OC    SVTBLINE,SVTBLINE   TEST MORE ENTRIES IN SVTAB                   
         BNZ   GETP10                                                           
         J     EQXIT                                                            
         EJECT                                                                  
*===============================================================                
* REPRINT - READ INSTRUCTION RECAP RECORDS                                      
*===============================================================                
                                                                                
GETP50   MVI   SVT1PR9,C'N'         IF REPRINT, THEN MUST PRINT                 
         XC    WORK,WORK                                                        
                                                                                
* GET 3-BYTE VERSIONS OF INSTRUCTION DATES                                      
                                                                                
         GOTO1 DATCON,DMCB,(2,SVFLTST),(3,WORK+16)                              
         GOTO1 (RF),(R1),(2,SVFLTEND),(3,WORK+19)                               
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0A24'     GET AN INST RECAP RECORD                     
         MVC   KEY+2(3),BAGYMD     A-M/CLT                                      
         MVC   KEY+5(1),SVTBPRD    PRD                                          
         MVC   KEY+6(5),SVTBMKST   MKT/STA                                      
         MVC   KEY+11(1),SVTBCOPY  COPY CODE                                    
*                                                                               
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(12),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
GETP52   MVC   WORK(13),KEY        SAVE INST RECAP KEY                          
*                                                                               
         L     R6,AIO2             USE I/O2 FOR INST RECAP REC                  
         ST    R6,AIO                                                           
         GOTO1 GETREC                                                           
*                                                                               
         L     R4,NEXTADDR                                                      
         USING PTNLISTD,R4                                                      
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         USING INSDTAEL,R6                                                      
GETP52A  CLC   INSPRD1(4),SVTBPRD                                               
         BNE   GETP52B                                                          
*                                                                               
*NOP     CLC   INSFTD,SVFLTEND     IF FTD IN 10 ELEM > LTD                      
*        BH    GETP52B              THEN GET NEXT ELEM                          
*        CLC   INSLTD,SVFLTST      IF LTD IN 10 ELEM < FTD                      
*        BL    GETP52B              THEN GET NEXT ELEM                          
******   B     GETP52X                                                          
*                                                                               
         CLC   INSPERST,SVFLTEND   IF FTD IN 10 ELEM > LTD                      
         BH    GETP52B              THEN GET NEXT ELEM                          
         CLC   INSPERND,SVFLTST     IF LTD IN 10 ELEM < FTD                     
         BNL   GETP52X                                                          
*                                                                               
GETP52B  BRAS  RE,NEXTEL                                                        
         BE    GETP52A                                                          
         DC    H'0'                                                             
*                                                                               
GETP52X  ST    R6,FULL             SV ADDR OF INSTR RECAP ELEM                  
         LLC   R1,INSDTALN         GET ELEMENT LENGTH                           
         AHI   R1,-(INSPTTN-INSDTAEL)                                           
         SR    R0,R0                                                            
         D     R0,=F'7'                                                         
         LTR   R0,R0                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         LR    R0,R1               SET FOR BCT ON NUM OF ENTRIES                
         LA    R1,INSPTTN                                                       
*                                                                               
GETP54   CLC   3(2,R1),SVFLTEND  1ST TLCST TO PERIOD END                        
         BH    GETP54C                                                          
         CLC   5(2,R1),SVFLTST   LAST TLCST TO PERIOD STR                       
         BNL   GETP55                                                           
*                                                                               
GETP54C  LA    R1,7(R1)            GET NEXT SUBELEM                             
         BCT   R0,GETP54                                                        
         B     GETP75              GET NEXT ELEM                                
*                                                                               
GETP55   CLI   OFFLINE,C'Y'                                                     
         BE    GETP56                                                           
         LR    RF,R9                                                            
         A     RF,LSYSD                                                         
         AHI   RF,-L'PTNLIST+1                                                  
         LA    RE,0(R4)                  CLEAR HOB OF R4                        
         CR    RE,RF                     TEST ROOM IN LIST                      
         JH    NOPTNRM                                                          
*                                                                               
GETP56   LLC   RE,NEXTADDR         BUMP PATTERN SEQ NUMBER                      
         LA    RE,1(RE)                                                         
         STC   RE,NEXTADDR         AND SAVE                                     
         CLI   NEXTADDR,X'FF'                                                   
         BL    *+6                                                              
         DC    H'0'                NO MORE PATTERNS                             
*                                                                               
         XC    0(L'PTNLIST+1,R4),0(R4)   CLEAR 1 ENTRY + 1 BYTE                 
         MVI   PTNLTYPE,16               FAKE CODE                              
         MVC   PTNLSEQ,NEXTADDR          MOVE SEQUENCE NUMBER                   
         MVC   PTNLREF,0(R1)                                                    
         MVC   PTNLSTR,3(R1)                                                    
         MVC   PTNLEND,5(R1)                                                    
*                                                                               
         CLC   PTNLREF,=XL3'FFFFFF'  THIS A TBA                                 
         BNE   *+12                                                             
         MVI   PTNLTYPE,X'FF'                                                   
         B     GETP72                                                           
*                                                                               
         OC    PTNLREF,PTNLREF       THIS HIATUS                                
         BNZ   GETP58                                                           
         OI    PTNLFLAG,PTNLFHIA                                                
         B     GETP72                                                           
*                                                                               
         DROP  R6                                                               
*                                                                               
* NOW WE NEED TO READ THE PATTERN REC TO GET TIME AND DAYPART                   
                                                                                
GETP58   MVC   KEY(2),=X'0A22'     READ PATTERN RECORD                          
         MVC   KEY+5(4),SVTBPRD    PRD/SLN  PTR/SLN                             
         MVC   KEY+9(1),SVTBCOPY   COPY CODE                                    
         MVC   KEY+10(3),0(R1)     REF/SUB                                      
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    GETP59                                                           
         MVC   KEY,KEYSAVE                                                      
         MVC   KEY+5(1),SVTBPRD+2                                               
         MVC   KEY+7(1),SVTBPRD+0                                               
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
GETP59   MVC   PTNLDSKA,KEY+14                                                  
*                                                                               
         L     R6,AIO1             USE I/O 1                                    
         ST    R6,AIO                                                           
         GOTO1 GETREC                                                           
                                                                                
* FIND A PATTERN ELEM THAT OVERLAPS THE PERIOD                                  
                                                                                
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING PATDTAEL,R6                                                      
*                                                                               
GETP60   CLC   WORK+16(3),PATEND    INST START AFTER PATTERN END                
         BH    GETP62                                                           
         CLC   WORK+19(3),PATSTART  INST END BEFORE PATTERN START               
         BL    GETP62                                                           
         B     GETP64               USE THIS ELEMENT                            
*                                                                               
GETP62   BRAS  RE,NEXTEL                                                        
         BE    GETP60                                                           
         DC    H'0'                                                             
                                                                                
GETP64   MVC   PTNLDPT,PATDPT                                                   
         CLI   PTNLDPT,0                                                        
         BNE   *+8                                                              
         MVI   PTNLDPT,X'FF'                                                    
*                                                                               
         OC    PATSTIM,PATSTIM       TEST START TIME PRESENT                    
         BZ    GETP66                                                           
         MVC   PTNLSTIM(4),PATSTIM   MOVE TIME                                  
*                                                                               
         TM    PATSTAT1,PATSDLY      TEST TIMES ARE DAILY                       
         BZ    *+12                                                             
         OI    PTNLFLAG,PTNLFDLY                                                
         MVI   DLYFLAG,C'Y'                                                     
*                                                                               
GETP66   CLC   =XL2'FFFF',PTNLEND  TEST PATTERN RUNS UFN                        
         BNE   *+8                                                              
         OI    PTNLFLAG,PTNLFUFN   SET FLAG                                     
*                                                                               
         TM    PATSTAT,X'08'       COMML TEXT UPDATE                            
         BZ    *+8                                                              
         OI    PTNLFLAG,PTNLFCMT   SET FLAG                                     
*                                                                               
         TM    PATSTAT,X'01'       PATTERN TEXT UPDATE                          
         BZ    *+8                                                              
         OI    PTNLFLAG,PTNLFSTX   SET FLAG                                     
*                                                                               
         TM    PATSTAT,X'04'       TEST STATUS = INVERT PRODUCT                 
         BZ    *+8                  NO                                          
         OI    PTNLFLAG,PTNLFIPR                                                
*                                                                               
GETP70   OC    0(3,R1),0(R1)       THIS A HIATUS                                
         BNZ   GETP72                                                           
         OI    PTNLFLAG,X'80'                                                   
*                                                                               
GETP72   LA    R1,7(R1)                                                         
         LA    R4,PTNLNEXT            NEXT PATTERN LIST ENTRY                   
         BCT   R0,GETP54                                                        
*                                                                               
         DROP  R6                                                               
*                                                                               
         L     R6,FULL             ADDR OF LAST INSTR RECAP ELEM                
GETP75   BRAS  RE,NEXTEL                                                        
         BNE   GETP80                                                           
*                                                                               
         USING INSDTAEL,R6                                                      
*                                                                               
         CLC   INSPERST,SVFLTEND   IF FTD IN 10 ELEM > LTD                      
         BH    GETP75               THEN GET NEXT ELEM                          
         CLC   INSPERND,SVFLTST     IF LTD IN 10 ELEM < FTD                     
         BL    GETP75               THEN GET NEXT ELEM                          
         B     GETP52X                                                          
*                                                                               
         DROP  R6                                                               
*                                                                               
GETP80   LA    R4,1(R4)                                                         
         STCM  R4,7,NEXTADDR+1                                                  
         DROP  R4                                                               
*                                                                               
         MVC   KEY,WORK            MOVE SAVED INST RECAP KEY                    
         CLC   KEY(2),=X'0A24'                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 HIGH                RE-ESTABLISH SEQ                             
         GOTO1 SEQ                 AND SEE IF HAVE ANOTHER DPT                  
         CLC   KEY(12),KEYSAVE                                                  
         BE    GETP52                                                           
*                                                                               
GETPX    B     GETP32              GO CALL BLDDATE AND CONTINUE                 
         LTORG                                                                  
         EJECT                                                                  
*===============================================================                
* SUBROUTINE TO BUILD A PATTERN LIST ENTRY FROM PATTERN RECORD                  
* ON ENTRY NEXTADDR POINTS TO NEXT PATTERN LIST SLOT AND                        
*          HIGH ORDER BYTE IS NEXT SEQUENCE NUMBER                              
*===============================================================                
                                                                                
BLDLIST  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R4,NEXTADDR                                                      
         XC    0(L'PTNLIST+1,R4),0(R4)    CLEAR 1 ENTRY + 1 BYTE                
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING PATDTAEL,R6                                                      
         XC    SVPDATA,SVPDATA     CLEAR PATTERN SAVE AREA                      
*                                                                               
         SR    R0,R0               ADJUST TO 6A-559A CLOCK                      
         ICM   R0,3,PATSTIM                                                     
         STCM  R0,3,SVPSTIM                                                     
         BZ    BLDLNOTM                                                         
         B     BLDL00              DO NOT ADJUST FOR CALENDAR DAYS              
*                                                                               
         CHI   R0,559              <<NOP>>                                      
         BH    *+8                                                              
         AHI   R0,2400                                                          
         STCM  R0,3,SVPSTIM                                                     
*                                                                               
BLDL00   ICM   R0,3,PATETIM                                                     
         STCM  R0,3,SVPETIM                                                     
         B     BLDLNOTM            DO NOT ADJUST FOR CALENDAR DAYS              
*                                                                               
         CHI   R0,559              <<NOP>>                                      
         BH    *+8                                                              
         AHI   R0,2400                                                          
*                                                                               
BLDLNOTM MVC   SVPDPT(1),PATDPT    DAYPART                                      
         CLI   SVPDPT,0                                                         
         BNE   *+8                                                              
         MVI   SVPDPT,X'FF'                                                     
         MVC   SVPSTAT,PATSTAT     AND STATUS                                   
         MVC   SVPSTAT1,PATSTAT1   SAVE DAILY TIME FLAG                         
*                                                                               
         GOTO1 DATCON,DMCB,(3,PATSTART),(2,SVPSTART) FIRST TLCST DT             
         GOTO1 (RF),(R1),(3,PATEND),(2,SVPEND)       LAST TLCST DT              
*                                                                               
         CLC   SVTBEND,SVPSTART    LAST TLCST BEFORE PATTERN START              
         JL    EXIT                                                             
         CLC   SVTBSTR,SVPEND      FIRST TLCST AFTER PATTERN END                
         JH    EXIT                                                             
         CLI   1(R6),38            TEST EXTENDED ELEMENT                        
         BNH   BLDL01               NO                                          
         TM    PATSTAT,X'80'       TEST STATUS = DELETED                        
         JO    EXIT                 YES - IGNORE                                
                                                                                
* TEST PATTERN APPLIES TO THIS STATION *                                        
                                                                                
BLDL01   L     R6,AIO                                                           
         MVI   ELCODE,X'20'                                                     
         BRAS  RE,GETEL            FIND LIST ELEMENT                            
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING PATLSTEL,R6                                                      
*                                                                               
         CLI   2(R6),C'M'          TEST MARKET LIST                             
         BNE   BLDL02                                                           
         OC    PATLST,PATLST       TEST ALL MARKET PATTERN                      
         BNZ   BLDL02                                                           
         MVI   ELCODE,14           SET ALL MKT IND                              
         B     BLDL60                                                           
*                                                                               
BLDL02   DS   0H                                                                
         LLC   R0,1(R6)                                                         
         SRDL  R0,32                                                            
         D     R0,=F'5'                                                         
         LR    R0,R1               SET FOR BCT                                  
         LA    R6,2(R6)                                                         
*                                                                               
* PROCESS MARKET GROUP LIST *                                                   
*                                                                               
         CLI   0(R6),C'G'          TEST MKT GROUP PATTERN                       
         BNE   BLDL10                                                           
         MVC   WORK(18),KEY                                                     
*                                                                               
BLDL04   XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D82'                                                  
         MVC   KEY+2(3),BAGYMD                                                  
         MVC   KEY+8(3),1(R6)                                                   
         MVC   KEY+11(2),SVTBMKST                                               
*                                                                               
         MVC   FILENAME,=CL8'SPTDIR'                                            
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    BLDL08                                                           
         CLC   KEY(5),KEYSAVE                                                   
         BNE   BLDL05                                                           
         OC    KEY+5(3),KEY+5      IS THIS A PRODUCT GROUP                      
         BZ    BLDL05                                                           
         MVC   KEYSAVE+5(3),KEY+5                                               
         MVC   KEY(13),KEYSAVE                                                  
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    BLDL08                                                           
*                                                                               
BLDL05   MVC   KEY(13),KEYSAVE                                                  
         XC    KEY+3(5),KEY+3      TRY NON-CLIENT SPECIFIC                      
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    BLDL08                                                           
         CLC   KEY(5),KEYSAVE                                                   
         BNE   BLDL06                                                           
         OC    KEY+5(3),KEY+5      IS THIS A PRODUCT GROUP                      
         BZ    BLDL06                                                           
         MVC   KEYSAVE+5(3),KEY+5                                               
         MVC   KEY(13),KEYSAVE                                                  
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    BLDL08                                                           
*                                                                               
BLDL06   LA    R6,5(,R6)                                                        
         BCT   R0,BLDL04                                                        
*                                                                               
         MVC   KEY(18),WORK                                                     
         XC    FILENAME,FILENAME                                                
         J     EXIT                                                             
*                                                                               
BLDL08   MVI   ELCODE,12           SET MARKET GROUP ENTRY IND                   
*                                                                               
         MVC   KEY(18),WORK                                                     
         XC    FILENAME,FILENAME                                                
         B     BLDL60                                                           
*                                                                               
BLDL10   CLI   0(R6),C'M'          TEST MKT PATTERN                             
         BNE   BLDL20                                                           
                                                                                
* PROCESS MARKET LIST *                                                         
                                                                                
BLDL12   CLC   SVTBMKST(2),4(R6)   MATCH MARKET CODES                           
         BE    BLDL14                                                           
         LA    R6,5(R6)                                                         
         BCT   R0,BLDL12                                                        
         J     EXIT                                                             
*                                                                               
BLDL14   MVI   ELCODE,10           SET MARKET ENTRY IND                         
         B     BLDL60                                                           
                                                                                
* PROCESS AFFILIATE LIST *                                                      
                                                                                
BLDL20   CLI   0(R6),C'A'          TEST AFFILIATE LIST                          
         BNE   BLDL30                                                           
BLDL22   CLC   SVTBAFFL,1(R6)                                                   
         BE    BLDL24                                                           
         LA    R6,5(R6)                                                         
         BCT   R0,BLDL22                                                        
         J     EXIT                                                             
BLDL24   MVI   ELCODE,8            SET AFFILIATE ENTRY IND                      
         B     BLDL60                                                           
                                                                                
* PROCESS COMBINED MARKET/AFFILIATE LIST *                                      
                                                                                
BLDL30   CLI   0(R6),C'C'          TEST COMBINED                                
         BNE   BLDL40                                                           
         LR    RE,R0                                                            
         LR    RF,R6                                                            
BLDL32   CLI   1(R6),0             THIS AN AFFILIATE                            
         BE    BLDL33               NO                                          
         CLC   SVTBAFFL,1(R6)                                                   
         BE    BLDL34                                                           
BLDL33   LA    R6,5(R6)                                                         
         BCT   R0,BLDL32                                                        
         J     EXIT                                                             
*                                                                               
BLDL34   CLC   SVTBMKST(2),4(RF)   MATCH MARKET CODES                           
         BE    BLDL36                                                           
         LA    RF,5(,RF)                                                        
         BCT   RE,BLDL34                                                        
         J     EXIT                                                             
*                                                                               
BLDL36   MVI   ELCODE,6            SET COMBINED                                 
         B     BLDL60                                                           
                                                                                
* PROCESS STATION TYPE *                                                        
                                                                                
BLDL40   CLI   0(R6),C'T'          TEST STATION TYPE LIMIT                      
         BNE   BLDL50                                                           
*                                                                               
BLDL42   CLC   SVTBTYPE,1(R6)                                                   
         BE    BLDL44                                                           
         LA    R6,5(R6)                                                         
         BCT   R0,BLDL42                                                        
         J     EXIT                                                             
*                                                                               
BLDL44   MVI   ELCODE,4            SET STATION TYPE ENTRY IND                   
         B     BLDL60                                                           
*                                                                               
BLDL50   CLI   0(R6),C'S'          TEST STATION LIST                            
         BE    BLDL52                                                           
         DC    H'0'                                                             
*                                                                               
BLDL52   OC    1(2,R6),1(R6)       THIS A CABLEHEAD STA                         
         BNZ   BLDL53               NO                                          
         CLC   SVTBMKST+2(3),3(R6)                                              
         BE    BLDL54                                                           
         B     BLDL53C                                                          
*                                                                               
BLDL53   CLC   SVTBSTA,1(R6)                                                    
         BE    BLDL54                                                           
*                                                                               
BLDL53C  LA    R6,5(,R6)                                                        
         BCT   R0,BLDL52                                                        
         J     EXIT                                                             
*                                                                               
BLDL54   MVI   ELCODE,2            SET STATION LIST ENTRY IND                   
*                                                                               
BLDL60   CLI   SVTBCOPY,0          ANY COPY CODE EXPECTED                       
         BE    BLDL62               NO                                          
         CLI   KEY+9,0             COPY CODED PATTN                             
         BNE   BLDL62               YES                                         
         LLC   RF,ELCODE                                                        
         LA    RF,14(,RF)                                                       
         STC   RF,ELCODE                                                        
                                                                                
*====================================================================           
* NEED TO CHECK IF HAVE TWO PATTERNS WITH SAME DATES/TIMES                      
* IF SO, JUST KEEP THE MOST SPECIFIC                                            
*====================================================================           
                                                                                
         USING PTNLISTD,R4                                                      
BLDL62   L     R4,SVTBAPTN                                                      
         SR    R0,R0                                                            
         ICM   R0,7,NEXTADDR+1                                                  
*                                                                               
BLDL64   CR    R4,R0               END OF CURRENT PATTERN LIST                  
         BNL   BLDL70              IF HIGH OR EQUAL, DONE                       
*                                                                               
         CLC   PTNLSTR(4),SVPSTART TEST SAME DATES                              
         BNE   BLDL66                                                           
         CLC   PTNLSTIM(4),SVPSTIM TEST SAME TIMES                              
         BNE   BLDL66                                                           
         CLC   PTNLDPT,SVPDPT      TEST SAME DPT                                
         BNE   BLDL66                                                           
         CLC   ELCODE,PTNLTYPE     LOWER ELCODE IS MORE SPECIFIC                
         BL    BLDL74              YES - OVERWRITE THE OLD ENTRY                
         B     BLDL80              ELSE IGNORE THIS PATTERN                     
*                                                                               
BLDL66   LA    R4,PTNLNEXT                                                      
         B     BLDL64                                                           
         EJECT                                                                  
* ADD ENTRY TO PATTERN LIST IF SPACE *                                          
*                                                                               
BLDL70   L     R4,NEXTADDR                                                      
         LA    R4,0(R4)            CLEAR HOB                                    
*                                                                               
         CLI   OFFLINE,C'Y'                                                     
         BE    BLDL72                                                           
*                                                                               
         LR    R0,R9                                                            
         A     R0,LSYSD                                                         
         AHI   R0,-L'PTNLIST+1                                                  
         CR    R4,R0                     TEST ROOM IN LIST                      
         BH    NOPTNRM                                                          
         XC    0(L'PTNLIST+1,R4),0(R4)   CLEAR 1 ENTRY + 1 BYTE                 
*                                                                               
BLDL72   LLC   RE,NEXTADDR         BUMP SEQUENCE NUMBER                         
         LA    RE,1(RE)                                                         
         L     RF,NEXTADDR         AND SAVE                                     
         LA    RF,L'PTNLIST(RF)                                                 
         ST    RF,NEXTADDR         SAVE NEXT ADDRES                             
         STC   RE,NEXTADDR         AND SEQUENCE NUMBER                          
*                                                                               
         CLI   NEXTADDR,X'FF'                                                   
         BL    *+6                                                              
         DC    H'0'                NO MORE PATTERNS                             
         MVC   PTNLSEQ,NEXTADDR    MOVE SEQUENCE NUMBER                         
*                                                                               
BLDL74   MVC   PTNLTYPE,ELCODE     SET ENTRY TYPE                               
         MVC   PTNLSTR(4),SVPSTART MOVE DATES                                   
         MVI   PTNLDPT,X'FF'       SET DEFAULT DPT                              
*                                                                               
         CLI   SVPROF11,C'E'       TEST COPYCODE=EST                            
         BNE   BLDL78                                                           
*                                                                               
         MVC   PTNLDPT,SVPDPT      MOVE DAYPART                                 
         CLI   PTNLDPT,0                                                        
         BNE   *+8                                                              
         MVI   PTNLDPT,X'FF'                                                    
*                                                                               
BLDL78   OC    SVPSTIM,SVPSTIM       TEST START TIME PRESENT                    
         BZ    *+10                                                             
         MVC   PTNLSTIM(4),SVPSTIM   MOVE START/END TIMES                       
*                                                                               
         TM    SVPSTAT1,PATSDLY    TEST DAILY TIMES                             
         BZ    *+12                                                             
         OI    PTNLFLAG,PTNLFDLY                                                
         MVI   DLYFLAG,C'Y'        SET DAILY TIMES FLAG                         
*                                                                               
         MVC   PTNLREF,KEY+10        REF/SUBLINE                                
         MVC   PTNLDSKA,KEY+14       AND DISK ADDRESS                           
*                                                                               
         CLC   =XL2'FFFF',PTNLEND  TEST PATTERN RUNS UFN                        
         BNE   *+8                                                              
         OI    PTNLFLAG,PTNLFUFN   SET FLAG                                     
*                                                                               
         TM    SVPSTAT,X'08'         COMML TEXT UPDATE                          
         BZ    *+8                                                              
         OI    PTNLFLAG,PTNLFCMT   SET FLAG                                     
*                                                                               
         TM    SVPSTAT,X'01'       PATTERN TEXT UPDATE                          
         BZ    *+8                                                              
         OI    PTNLFLAG,PTNLFSTX   SET FLAG                                     
*                                                                               
         TM    SVPSTAT,X'04'       TEST STATUS = INVERT PRODUCT                 
         BZ    *+8                  NO                                          
         OI    PTNLFLAG,PTNLFIPR                                                
                                                                                
* FIND X'30' ELEMENT TO TEST PATTERN IS HIATUS *                                
                                                                                
         L     R6,AIO                                                           
         TM    SVOPT3,OPTBPAT      BPAT RECORD                                  
         BO    BLDL80              THERE ARE NO HIATUS BPAT RECS                
         MVI   ELCODE,X'30'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   =C'HIATUS',2(R6)                                                 
         BNE   BLDL80                                                           
         OI    PTNLFLAG,PTNLFHIA                                                
         XC    PTNLREF,PTNLREF     CLEAR REF/SUBLINE FOR HIATUS                 
*                                                                               
BLDL80   J     EXIT                                                             
         DROP  R4                                                               
*                                                                               
NOPTNRM  DC    H'0'                                                             
         LTORG                                                                  
         EJECT                                                                  
*==============================================================                 
* SUBROUTINE TO PROCESS PATTERN LIST DATA AND BUILD A TABLE                     
* OF PATTERNS THAT APPLY TO EACH DAY IN THE TELECAST PERIOD                     
* DATELIST NOW HAS 8 BYTES FOR EACH DAY                                         
*          FOR A START TIME OR 1-8 DAYPARTS                                     
* DPTLIST HAS A LIST OF THE DPTS POSITIONALLY                                   
* 'NO DAYPART' IS CODED AS X'FF' AND ALWAYS IS FIRST IN THE LIST                
*                                                                               
* THE CODE FOR TIMES ONLY WORKS IF THE PATTERNS ARE IN DATE/TIME                
* ORDER, SO SORT THE TABLE BEFORE WE BEGIN!                                     
***************************************************************                 
* NOTE! NOTE! NOTE!                                                             
* BECAUSE DTLIST IS SHARED FOR DAYPARTS AND DAILY TIMES,                        
* THE NUMBER OF ENTRIES IN TIMELIST MUST BE THE SAME AS THE                     
* NUMBER OF ENTRIES IN DPTLIST                                                  
* SO WHEN PROGRAM POINTS TO TIMELIST, THE COUNTER IS SET FROM                   
* L'DPTLIST!!!                                                                  
*==============================================================                 
                                                                                
BLDDATES NTR1  BASE=*,LABEL=*                                                   
         L     R5,SVTBAPTN                                                      
         USING PTNLISTD,R5                                                      
         BRAS  RE,SORTPTN                                                       
*                                                                               
         CLI   DLYFLAG,C'Y'        TEST ANY PATTERN HAS DAILY TIMES             
         BE    BLDDT3                                                           
*                                                                               
BLDDT2   LA    RE,DPTLIST                                                       
         LA    RF,L'DPTLIST                                                     
*                                                                               
BLDDT2A  CLC   0(1,RE),PTNLDPT     MATCH DPT                                    
         BE    BLDDT2B                                                          
         CLI   0(RE),0             TEST EOL                                     
         BE    BLDDT2B                                                          
         LA    RE,1(RE)                                                         
         BCT   RF,BLDDT2A                                                       
         DC    H'0'                TOO MANY DIFFERENT DPTS                      
*                                                                               
BLDDT2B  CLI   PTNLDPT,X'FF'       TEST THIS IS ALL DPT ENTRY                   
         BE    BLDDT2C             YES - MAKE SURE IT'S FIRST IN LIST           
         MVC   0(1,RE),PTNLDPT                                                  
         B     BLDDT2X                                                          
*                                                                               
BLDDT2C  CLI   DPTLIST,X'FF'       TEST DPT=0 ENTRY YET                         
         BE    BLDDT2X             YES - HAVE ONE                               
                                                                                
* MOVE ENTRIES DOWN 1 SO ALL DPT ENTRY IS FIRST                                 
                                                                                
         LM    R0,R1,DPTLIST                                                    
         MVI   DPTLIST,X'FF'                                                    
         STCM  R0,15,DPTLIST+1                                                  
         STCM  R1,14,DPTLIST+5                                                  
         B     BLDDT2X                                                          
*                                                                               
BLDDT2X  LA    R5,L'PTNLIST(R5)                                                 
         CLI   0(R5),0                                                          
         BNE   BLDDT2                                                           
*                                                                               
         MVI   DPTINDEX,0                                                       
         B     BLDDT10                                                          
                                                                                
*===========================================================                    
* BUILD LIST OF DAILY TIMES                                                     
*===========================================================                    
                                                                                
BLDDT3   XC    TIMELIST(TIMELSTX-TIMELIST),TIMELIST                             
         MVC   TIMELIST(4),=X'0001FFFF'                                         
*                                                                               
BLDDT4   LA    RE,TIMELIST                                                      
         LA    RF,L'DPTLIST        SEE NOTE AT TOP OF ROUTINE                   
*                                                                               
         OC    PTNLSTIM(4),PTNLSTIM     TEST THIS PATTERN HAS TIME              
         BNZ   BLDDT4A                  NO                                      
         MVC   PTNLSTIM(4),=X'0001FFFF'                                         
         OI    PTNLFLAG,PTNLFDLY                                                
*                                                                               
BLDDT4A  CLC   0(4,RE),PTNLSTIM    MATCH TIMES                                  
         BE    BLDDT4C                                                          
         OC    0(2,RE),0(RE)       TEST EOL                                     
         BE    BLDDT4B                                                          
         LA    RE,4(RE)                                                         
         BCT   RF,BLDDT4A                                                       
         DC    H'0'                TOO MANY TIME SLOTS                          
*                                                                               
BLDDT4B  MVC   0(4,RE),PTNLSTIM    MOVE TIME TO LIST                            
*                                                                               
BLDDT4C  LA    R5,L'PTNLIST(R5)                                                 
         CLI   0(R5),0                                                          
         BNE   BLDDT4                                                           
*                                                                               
         LA    RE,TIMELIST         COUNT ACTIVE TIME ENTRIES                    
         SR    R0,R0                                                            
BLDDT4D  OC    0(4,RE),0(RE)                                                    
         BZ    BLDDT4E                                                          
         LA    RE,4(RE)                                                         
         BCT   R0,BLDDT4D                                                       
*                                                                               
BLDDT4E  LPR   R0,R0                                                            
         CHI   R0,1                                                             
         BNH   BLDDT4F                                                          
         CHI   R0,8                                                             
         BNH   *+6                                                              
         DC    H'0'                TOO MANY TIME ENTRIES!                       
         GOTO1 XSORT,DMCB,TIMELIST,(R0),4,4,0                                   
*                                                                               
BLDDT4F  MVI   DLYINDEX,0                                                       
                                                                                
*===========================================================                    
* BACK TO COMMON PROCESSING                                                     
*===========================================================                    
                                                                                
BLDDT10  L     R5,SVTBAPTN                                                      
         L     R2,SVTBLINE         PRESET CURSOR FOR ERROR                      
         A     R2,ATWA                                                          
*                                                                               
BLDDT14  L     R0,ADTLIST                                                       
         L     R1,ADTLISTX                                                      
         SR    R1,R0                                                            
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         GOTO1 DATCON,DMCB,(2,SVTBSTR),WORK    FIRST TELECAST DATE              
         GOTO1 (RF),(R1),(2,SVTBEND),WORK+6    LAST TELECAST DATE               
*                                                                               
         CLI   0(R5),0             TEST FOR NO PATTERN                          
         BNE   BLDDT16                                                          
         CLI   SVT1PR3,C'Y'        PRINT PATTERN ERRS AS TBA                    
         BE    BLDDT32                                                          
         CLI   SVT1PR3,C'S'        SUPPRESS PRINT PATTERN ERRS AS TBA           
         BE    BLDDT32                                                          
         TM    SVOPT2,OP2TBA       TEST-PRT UNASSIGNED SPOTS AS TBA             
         BO    BLDDT32                                                          
         OI    SVTBIND,X'01'                                                    
         XC    PARAS(8),PARAS      CLEAR DATE FOR ERROR RTN                     
         B     DTERR1                                                           
*                                                                               
BLDDT16  MVI   ELCODE,14                                                        
         CLI   SVTBCOPY,0          ANY COPY EXPECTED                            
         BE    BLDDT18             NO                                           
         MVI   ELCODE,28                                                        
*                                                                               
BLDDT18  CLI   PTNLTYPE,0          TEST FOR EOL                                 
         BE    BLDDT28                                                          
         CLC   PTNLTYPE,ELCODE     ELSE MATCH CODES                             
         BNE   BLDDT26                                                          
*                                                                               
         CLI   DPTLIST,0           TEST DPTS ACTIVE                             
         BE    BLDDT18A            NO                                           
         LLC   RE,DPTINDEX                                                      
         LA    RE,DPTLIST(RE)                                                   
         CLC   PTNLDPT,0(RE)       MATCH DPT                                    
         BNE   BLDDT26                                                          
         B     BLDDT18X                                                         
*                                                                               
BLDDT18A CLI   DLYFLAG,C'Y'        TEST DAILY TIMES ACTIVE                      
         BNE   BLDDT18X                                                         
         LLC   RE,DLYINDEX                                                      
         SLL   RE,2                X 4                                          
         LA    RE,TIMELIST(RE)                                                  
         CLC   PTNLSTIM(4),0(RE)   MATCH TIMES                                  
         BNE   BLDDT26                                                          
                                                                                
* DETERMINE LIMITS OF PATTERN DATES VS TLCST DATES *                            
                                                                                
BLDDT18X LA    R0,PTNLSTR          POINT TO PATTERN START                       
         CLC   SVTBSTR,PTNLSTR     FIRST TLCST DATE TO PATTERN START            
         BNH   *+12                LOW - USE PATTERN START DATE                 
         LA    R0,SVTBSTR          ELSE POINT TO FIRST TLCST DATE               
         OI    PTNLFLG2,PTNLF2ST   SET FLAG NOT TO PRINT PTNSTIM                
*                                                                               
         GOTO1 DATCON,DMCB,(2,(R0)),WORK+12                                     
         LA    R0,PTNLEND                                                       
         CLC   SVTBEND,PTNLEND     LAST TLCST TO PATTERN END                    
         BNL   *+12                                                             
         LA    R0,SVTBEND                                                       
         OI    PTNLFLG2,PTNLF2EN   SET FLAG NOT TO PRINT PTNETIM                
         GOTO1 (RF),(R1),(2,(R0)),WORK+18                                       
                                                                                
* GET DISPLACEMENT TO FIRST DATE IN DATELIST (FROM SVTBSTR) *                   
                                                                                
         GOTO1 PERVERT,(R1),WORK,WORK+12                                        
         LH    R0,8(R1)            GIVES INCLUSIVE DAYS                         
         BCTR  R0,0                                                             
         MHI   R0,L'DPTLIST        X 4                                          
         L     R6,ADTLIST                                                       
         AR    R6,R0               POINT TO FIRST DATE                          
         LLC   R0,DPTINDEX         GET SLOT INDEX                               
         CLI   DLYFLAG,C'Y'        TEST DAILY TIMES                             
         BNE   *+8                                                              
         IC    R0,DLYINDEX                                                      
         AR    R6,R0               POINT TO CORRECT SLOT                        
*                                                                               
BLDDT24  TM    SVOPT3,OPT3REPT     TEST REPRINT                                 
         BZ    BLDDT24X                                                         
         CLI   0(R6),0                                                          
         BE    BLDDT24X                                                         
                                                                                
* FOR REPRINT, SLOT MAY ALREADY BE USED, SO FIND A FREE ONE                     
                                                                                
         LR    RE,R6                                                            
         LA    RE,1(RE)                                                         
         CLI   0(RE),0                                                          
         BNE   *-8                                                              
         MVC   0(1,RE),PTNLSEQ                                                  
         B     BLDDT25                                                          
*                                                                               
BLDDT24X MVC   0(1,R6),PTNLSEQ     MOVE PATTERN SEQUENCE                        
*                                                                               
BLDDT25  LA    R6,L'DPTLIST(R6)    NEXT DATE                                    
*                                                                               
         GOTO1 ADDAY,DMCB,WORK+12,WORK+12,1                                     
         CLC   WORK+12(6),WORK+18  TEST REACHED END LIMIT                       
         BNH   BLDDT24                                                          
*                                                                               
BLDDT26  LA    R5,L'PTNLIST(R5)    NEXT PATTERN LIST ENTRY                      
         B     BLDDT18                                                          
                                                                                
*=================================================================              
* IF DPTS OR DAILY TIMES PRESENT, UPDATE INDEX AND PROCESS AGAIN                
* WHEN THAT'S COMPLETE,                                                         
* DECREMENT VALUE IN ELCODE AND RE-PROCESS PATTERN LIST                         
*=================================================================              
                                                                                
BLDDT28  CLI   DPTLIST,0           TEST DPTS ACTIVE                             
         BE    BLDDT28A            NO                                           
         LLC   RE,DPTINDEX                                                      
         LA    RE,1(RE)                                                         
         STC   RE,DPTINDEX                                                      
         CLI   DPTINDEX,L'DPTLIST  MAX 8 DPTS                                   
         BE    BLDDT28X                                                         
         LA    RE,DPTLIST(RE)      POINT TO NEXT SLOT IN LIST                   
         CLI   0(RE),0                                                          
         BE    BLDDT28X            NO MORE DPTS                                 
         L     R5,SVTBAPTN                                                      
         B     BLDDT18                                                          
*                                                                               
BLDDT28A CLI   DLYFLAG,C'Y'        TEST DAILY TIMES ACTIVE                      
         BNE   BLDDT28X                                                         
         LLC   RE,DLYINDEX                                                      
         LA    RE,1(RE)                                                         
         STC   RE,DLYINDEX                                                      
         CLI   DLYINDEX,(TIMELSTX-TIMELIST)/L'TIMELIST  TEST EOL                
         BE    BLDDT28X                                                         
         SLL   RE,2                X 4                                          
         LA    RE,TIMELIST(RE)     POINT TO NEXT TIME ENTRY                     
         OC    0(4,RE),0(RE)                                                    
         BZ    BLDDT28X            NO MORE TIMES                                
         L     R5,SVTBAPTN                                                      
         B     BLDDT18                                                          
*                                                                               
BLDDT28X MVI   DPTINDEX,0          RESET POINTERS                               
         MVI   DLYINDEX,0                                                       
         L     R5,SVTBAPTN                                                      
         LLC   R0,ELCODE                                                        
         BCTR  R0,0                                                             
         BCTR  R0,0                                                             
         STC   R0,ELCODE                                                        
         LTR   R0,R0                                                            
         BP    BLDDT18                                                          
         EJECT                                                                  
*===========================================================                    
* ALL LIST ELEMENTS PROCESSED - SHOULD HAVE                                     
* PATTERN FOR EVERY DAY IN TELECAST PERIOD                                      
*===========================================================                    
                                                                                
BLDDT32  GOTO1 PERVERT,DMCB,WORK,WORK+6                                         
         LH    R0,8(R1)            NUMBER OF DAYS IN TELECAST PERIOD            
         CHI   R0,371                                                           
         BH    DATSPRER            DATE SPREAD ERROR                            
*                                                                               
         MHI   R0,L'DPTLIST        NUMBER OF DAYS * ENTRY LENGTH                
         L     RE,ADTLIST                                                       
         AR    RE,R0               POINT BEYOND LAST DAY                        
         MVI   0(RE),X'FF'         SET EOL FLAGS FOR EACH DPT                   
         MVC   1(L'DPTLIST-1,RE),0(RE)                                          
*                                                                               
         L     R6,ADTLIST                                                       
         LH    R0,8(R1)                                                         
*                                                                               
BLDDT34  OC    0(L'DPTLIST,R6),0(R6)   TEST NON-ZERO ENTRY                      
         BZ    BLDDT36                                                          
         LA    R6,L'DPTLIST(R6)                                                 
         BCT   R0,BLDDT34                                                       
         B     BLDDT40                                                          
                                                                                
* DATE MISSING, FIRST CHECK FOR TBA=YES *                                       
                                                                                
BLDDT36  CLI   SVT1PR3,C'Y'        PRINT PATTERN ERRS AS TBA                    
         BE    BLDDT40                                                          
         CLI   SVT1PR3,C'S'        SUPPRESS PRINT PATTERN ERRS AS TBA           
         BE    BLDDT40                                                          
         TM    SVOPT2,OP2TBA       TEST-PRT UNASSIGNED SPOTS AS TBA             
         BO    BLDDT40                                                          
         OI    SVTBIND,X'01'       SET ON MISSING PATTERN                       
         B     BLDDTERR                                                         
         EJECT                                                                  
*===================================================================            
* NOW BUILD NEW PATTERN LIST OVER THE OLD ONE - EACH ENTRY WILL                 
* NOW CONTAIN THE ACTUAL FIRST AND LAST TELECAST DATES. AN ENTRY                
* CAN APPEAR IN THE NEW TABLE MORE THAN ONCE                                    
*===================================================================            
                                                                                
BLDDT40  L     R0,AIO3                                                          
         L     R1,SIZEIO                                                        
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         L     R4,AIO3                                                          
         L     R6,ADTLIST                                                       
         MVI   DPTINDEX,0                                                       
         MVI   TIMINDEX,0                                                       
         MVI   DLYINDEX,0                                                       
         B     BLDDT78                                                          
*                                                                               
BLDDT42  LLC   RE,DPTINDEX                                                      
         CLI   DLYFLAG,C'Y'                                                     
         BNE   *+8                                                              
         IC    RE,DLYINDEX                                                      
         AR    RE,R6                                                            
         CLC   L'DPTLIST(1,RE),0(RE)  NEXT ENTRY USE SAME PATTERN               
         BNE   BLDDT44                                                          
         LA    R6,L'DPTLIST(R6)       NEXT DPT TABLE ENTRY                      
         B     BLDDT42                                                          
*                                                                               
BLDDT44  ST    R6,ALPD             SAVE END ADDRESS                             
         SR    RE,RE                                                            
         ICM   RE,1,TIMINDEX                                                    
         BNZ   *+8                                                              
         IC    RE,DPTINDEX                                                      
         CLI   DLYFLAG,C'Y'                                                     
         BNE   *+8                                                              
         IC    RE,DLYINDEX                                                      
         AR    RE,R6               POINT TO SLOT IN DTLIST                      
         CLI   0(RE),0             THIS A TBA                                   
         BE    BLDDT46                                                          
*                                                                               
BLDDT45  LLC   RF,0(RE)            GET PATTERN SEQ NUM                          
         BCTR  RF,0                                                             
         MHI   RF,L'PTNLIST                                                     
         A     RF,SVTBAPTN           POINT TO ENTRY                             
         MVC   0(L'PTNLIST,R4),0(RF) COPY ENTRY TO BUILD AREA                   
         B     BLDDT48                                                          
*                                                                               
BLDDT46  CLI   DLYFLAG,C'Y'        TEST DAILY TIMES                             
         BE    BLDDT76             NOT SO SURE ABOUT THIS BRANCH!               
         LLC   RE,DPTINDEX                                                      
         LA    RE,DPTLIST(RE)                                                   
         CLI   0(RE),0             TEST ALL DPT ENTRY HAS NO PTTN               
         BE    *+12                                                             
         CLI   0(RE),X'FF'         TEST ALL DPT ENTRY IN PROCESS                
         BNE   BLDDT76             NO - IGNORE TBA                              
*                                                                               
         MVI   0(R4),X'FF'                 ELSE BUILD TBA ENTRY                 
         MVI   PTNLDPT-PTNLIST(R4),X'FF'   SET 'ALL' DAYPART ENTRY              
         MVC   PTNLREF-PTNLIST(3,R4),=XL3'FFFFFF'                               
* MAKE SURE X'FF' ENTRY IN DPTLIST                                              
         CLI   DPTLIST,X'FF'                                                    
         BE    BLDDT48                                                          
         LM    R0,R1,DPTLIST       ELSE MAKE X'FF' THE FIRST ONE                
         MVI   DPTLIST,X'FF'                                                    
         STCM  R0,15,DPTLIST+1                                                  
         STCM  R1,14,DPTLIST+5                                                  
         B     BLDDT48                                                          
*                                                                               
* CALCULATE START AND END DATES                                                 
*                                                                               
BLDDT48  GOTO1 DATCON,DMCB,(2,SVTBSTR),WORK                                     
         L     RF,AFPD             GET PATTERN START DATE ADDRESS               
         S     RF,ADTLIST                                                       
         SR    RE,RE                                                            
         LA    R0,L'DPTLIST                                                     
         DR    RE,R0               GIVES NUMBER OF DAYS IN RF                   
         LR    R0,RF                                                            
         GOTO1 ADDAY,(R1),WORK,WORK+8,(R0)                                      
*                                                                               
         L     RF,ALPD                                                          
         S     RF,ADTLIST                                                       
         SR    RE,RE                                                            
         LA    R0,L'DPTLIST                                                     
         DR    RE,R0                                                            
         LR    R0,RF                                                            
         GOTO1 ADDAY,(R1),,WORK+16,(R0)                                         
*                                                                               
         GOTO1 DATCON,(R1),WORK+8,(2,WORK+22)                                   
         GOTO1 (RF),(R1),WORK+16,(2,WORK+24)                                    
*                                                                               
         MVC   PTNLSTR-PTNLIST(4,R4),WORK+22                                    
         LA    R4,L'PTNLIST(R4)                                                 
                                                                                
* MAKE SURE THERE IS MORE ROOM *                                                
                                                                                
         L     RE,AIO3                                                          
         L     RF,SIZEIO                                                        
         AR    RE,RF                                                            
         AHI   RE,-L'PTNLIST                                                    
         CR    R4,RE                                                            
         BL    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R6,ALPD             GET LAST PATTERN DATE ADDRESS                
         LLC   RF,0(R6)            GET FIRST PATTERN SEQNUM                     
         BCTR  RF,0                                                             
         MHI   RF,L'PTNLIST                                                     
         A     RF,SVTBAPTN           POINT TO ENTRY                             
*                                                                               
         CLI   DLYFLAG,C'Y'                                                     
         BE    BLDDT76                                                          
                                                                                
* TEST THE PATTERN ENTRY HAS START TIME                                         
                                                                                
         OC    PTNLSTIM-PTNLIST(2,RF),PTNLSTIM-PTNLIST(RF)                      
         BZ    BLDDT76                                                          
                                                                                
* LOOK FOR ANOTHER PATTERN THAT STARTS ON THE DATE                              
* THIS ONE ENDS AND RUNS ONLY FOR ONE DAY                                       
                                                                                
         CLI   TIMINDEX,0          UNLESS WE ALREADY DID THAT                   
         BNE   BLDDT56                                                          
         TM    SVOPT3,OPT3REPT     OR IF IT IS A REPRINT                        
         BO    BLDDT60                                                          
*                                                                               
         L     RE,SVTBAPTN         GET PATTERN LIST ADDRESS                     
*                                                                               
BLDDT50  CR    RE,RF                                                            
         BE    BLDDT55             SKIP ME                                      
         CLC   PTNLEND-PTNLIST(2,RF),PTNLSTR-PTNLIST(RE)                        
         BNE   BLDDT55                                                          
         CLC   PTNLEND-PTNLIST(2,RE),PTNLSTR-PTNLIST(RE)                        
         BNE   BLDDT55                                                          
* GOT ONE                                                                       
         LR    R1,R6               CURRENT DTLIST ENTRY                         
         LA    R0,8                                                             
*                                                                               
BLDDT52  CLI   0(R1),0                                                          
         BE    BLDDT54                                                          
         LA    R1,1(R1)                                                         
         BCT   R0,BLDDT52                                                       
         DC    H'0'                                                             
*                                                                               
BLDDT54  MVC   0(1,R1),PTNLSEQ-PTNLIST(RE)   MOVE SEQNUM                        
*                                                                               
BLDDT55  LA    RE,L'PTNLIST(RE)                                                 
         CLI   0(RE),0                                                          
         BNE   BLDDT50                                                          
*                                                                               
BLDDT55X CLI   1(R6),0             DID WE EXTEND THIS DTLIST                    
         BE    BLDDT70             NO - CONTINUE!                               
*                                                                               
         MVC   AFPD,ALPD           SET START DATE=END DATE                      
         MVI   TIMINDEX,1          SET TO PROCESS IT                            
         B     BLDDT42                                                          
*                                                                               
BLDDT56  LLC   RE,TIMINDEX                                                      
         LA    RE,1(RE)                                                         
         STC   RE,TIMINDEX                                                      
         CLI   TIMINDEX,L'DPTLIST  REACHED MAX                                  
         BE    BLDDT70                                                          
         AR    RE,R6               POINT INTO DTLIST ENTRY                      
         CLI   0(RE),0             ANY MORE PATTERNS TO DO                      
         BE    BLDDT70             NO                                           
         B     BLDDT45             GO COPY PATTERN                              
                                                                                
* THIS IS A REPRINT AND PATTERN HAS A START TIME                                
* SO THE NEXT PATTERN SEQNUM FOR THIS DAY IS ALREADY THERE                      
* MOVE IT TO THE LEFT AND PROCESS IT                                            
                                                                                
BLDDT60  CLI   1(R6),0                 TEST NO MORE ENTRIES                     
         BE    BLDDT70                                                          
         MVC   0(L'DPTLIST-1,R6),1(R6) MOVE PATTERN LEFT                        
         MVI   L'DPTLIST-1(R6),0                                                
         ST    R6,AFPD                                                          
         B     BLDDT42                                                          
                                                                                
BLDDT70  MVI   TIMINDEX,0          CLEAR THIS FIELD!                            
*                                                                               
         L     R6,ALPD             GET LAST PATTERN DATE ADDRESS                
         LLC   RF,0(R6)            GET PATTERN SEQ NUM                          
         BCTR  RF,0                                                             
         MHI   RF,L'PTNLIST                                                     
         A     RF,SVTBAPTN         POINT TO CURRENT PATTERN ENTRY               
*                                                                               
         LLC   RE,L'DPTLIST(R6)    GET NEXT PATTERN SEQNUM                      
         BCTR  RE,0                                                             
         MHI   RE,L'PTNLIST                                                     
         A     RE,SVTBAPTN         POINT TO NEXT PATTERN ENTRY                  
                                                                                
* TEST NEXT PATTERN STARTS SAME DATE THIS ENDS                                  
                                                                                
         CLC   PTNLEND-PTNLIST(2,RF),PTNLSTR-PTNLIST(RE)                        
         BNE   BLDDT76                                                          
                                                                                
         CLI   L'DPTLIST(R6),X'FF'   TEST NEXT IS EOL                           
         BE    BLDDT76                                                          
                                                                                
* ADVANCE R6 TO POINT TO NEXT ENTRY, BUT SET CURRENT R6 IN AFPD                 
                                                                                
BLDDT74  ST    R6,AFPD                                                          
         LA    R6,L'DPTLIST(R6)                                                 
         B     BLDDT42                                                          
*                                                                               
BLDDT76  LA    R6,L'DPTLIST(R6)      ELSE NEXT DATE                             
*                                                                               
BLDDT78  ST    R6,AFPD             SET AS NEXT PATTERN START                    
         CLI   0(R6),X'FF'                                                      
         BNE   BLDDT42                                                          
*                                                                               
         CLI   DPTLIST,0           TEST DPTS ACTIVE                             
         BE    BLDDT79             NO                                           
*                                                                               
         LLC   RE,DPTINDEX                                                      
         AHI   RE,1                                                             
         STC   RE,DPTINDEX                                                      
         CLI   DPTINDEX,L'DPTLIST                                               
         BE    BLDDT80                                                          
         LA    RE,DPTLIST(RE)      POINT TO NEXT SLOT IN LIST                   
         CLI   0(RE),0             TEST NO ENTRY                                
         BE    BLDDT80                                                          
*                                                                               
         L     R6,ADTLIST                                                       
         B     BLDDT78                                                          
*                                                                               
BLDDT79  CLI   DLYFLAG,C'Y'                                                     
         BNE   BLDDT80                                                          
*                                                                               
         LLC   RE,DLYINDEX                                                      
         AHI   RE,1                                                             
         STC   RE,DLYINDEX                                                      
         CLI   DLYINDEX,L'DPTLIST                                               
         BE    BLDDT79A                                                         
         SLL   RE,2                X 4                                          
         LA    RE,TIMELIST(RE)     POINT TO NEXT SLOT IN LIST                   
         OC    0(4,RE),0(RE)       TEST NO ENTRY                                
         BZ    BLDDT79A                                                         
*                                                                               
         L     R6,ADTLIST                                                       
         B     BLDDT78                                                          
                                                                                
* IF DAILY TIMES, SORT PATTERNS BY START DATE/START TIME                        
                                                                                
PTN      USING PTNLISTD,RE                                                      
                                                                                
BLDDT79A L     RE,AIO3             COUNT NUMBER OF PATTERNS                     
         SR    R0,R0                                                            
*                                                                               
BLDDT79B CLI   0(RE),0                                                          
         BE    BLDDT79C                                                         
         ICM   RF,3,PTN.PTNLEND          SAVE END DATE                          
         SR    R1,R1                                                            
         ICM   R1,3,PTN.PTNLSTIM                                                
         CHI   R1,2400             TEST MIDNIGHT                                
         BNE   *+6                                                              
         XR    R1,R1                                                            
         STCM  R1,3,PTN.PTNLEND    SET START TIME                               
         STCM  RF,3,PTN.PTNLSTIM   SET END DATE OVER START TIME                 
         LA    RE,L'PTNLIST(RE)                                                 
         BCT   R0,BLDDT79B                                                      
*                                                                               
BLDDT79C LPR   R0,R0                                                            
         GOTO1 XSORT,DMCB,AIO3,(R0),L'PTNLIST,4,PTNLSTR-PTNLISTD                
*                                                                               
         L     RE,AIO3                                                          
BLDDT79D CLI   0(RE),0                                                          
         BE    BLDDT80                                                          
         SR    RF,RF                                                            
         ICM   RF,3,PTN.PTNLEND    GET START TIME                               
         BNZ   *+8                 NOT MIDNIGHT                                 
         LA    RF,2400             SET MIDNIGHT BACK TO 2400                    
         MVC   PTN.PTNLEND,PTN.PTNLSTIM  MOVE END DATE BACK                     
         STCM  RF,3,PTN.PTNLSTIM         AND RESTORE START TIME                 
         LA    RE,L'PTNLIST(RE)                                                 
         BCT   R0,BLDDT79D                                                      
         DROP  PTN                                                              
                                                                                
* NOW MOVE PATTERN DATA BACK OVER ORIGINAL LIST *                               
                                                                                
BLDDT80  SR    R1,R1                                                            
         L     RE,SVTBAPTN                                                      
         L     RF,AIO3                                                          
*                                                                               
BLDDT82  MVC   0(L'PTNLIST,RE),0(RF)                                            
*                                                                               
         TM    PTNLFLAG-PTNLIST(RE),PTNLFCMT CMML TEXT UPDATE                   
         BZ    BLDDT84                                                          
         OI    SVTBIND,X'02'                                                    
*                                                                               
BLDDT84  LA    RE,L'PTNLIST(RE)                                                 
         LA    RF,L'PTNLIST(RF)                                                 
         BCTR  R1,0                                                             
         CLI   0(RF),0             TEST EOL                                     
         BNE   BLDDT82                                                          
         MVI   0(RE),0             SET EOL FLAG                                 
         LA    RE,1(RE)            INCLUDE FLAG IN LIST                         
         ST    RE,NEXTADDR         SET CURRENT LIST END ADDRESS                 
         LPR   R1,R1                                                            
         CHI   R1,MAXPTTNS         MAX PATTERNS FOR NOW                         
         BH    PATNERR                                                          
         J     EXIT                                                             
*                                                                               
DATSPRER CLI   OFFLINE,C'Y'        TOO MANY DAYS IN INST PERIOD                 
         BE    DATSPERA                                                         
         MVC   GERROR,=Y(MANYDAYS) MAX 371 DAYS                                 
         L     R2,SVTBLINE         PRESET CURSOR FOR ERROR                      
         A     R2,ATWA                                                          
         GOTO1 VTRAERR                                                          
*                                                                               
DATSPERA XC    ELEM,ELEM                                                        
         LA    R6,ELEM                                                          
         USING EFRECD,R6                                                        
         MVI   EFREASON,4          SET ERR LIST >371 DAYS IN PERIOD             
         OI    SVTBIND,X'08'                                                    
         B     DTERR12                                                          
*                                                                               
* MORE THAN 27 PATTERNS CAN'T BE HANDLED IN 1 INSTR RECAP REC ELEM              
*                                                                               
MAXPTTNS EQU   136                                                              
MAXPTNEL EQU   34                                                               
*                                                                               
PATNERR  CLI   OFFLINE,C'Y'                                                     
         BE    PATNERRC                                                         
         MVC   GERROR,=Y(MANYPTNS)  MAX 136 PTTNS                               
         L     R2,SVTBLINE         PRESET CURSOR FOR ERROR                      
         A     R2,ATWA                                                          
         GOTO1 VTRAERR                                                          
*                                                                               
PATNERRC XC    ELEM,ELEM                                                        
         LA    R6,ELEM                                                          
         USING EFRECD,R6                                                        
         MVI   EFREASON,1          SET ERR LIST TO TOO MANY PATTERNS            
         OI    SVTBIND,X'08'                                                    
         B     DTERR12                                                          
*                                                                               
BLDDTERR L     R0,ADTLIST                                                       
         LR    RF,R6                                                            
         S     RF,ADTLIST                                                       
         SR    RE,RE                                                            
         LA    R0,L'DPTLIST                                                     
         DR    RE,R0               GIVES DSPL IN DAYS IN RF                     
         LR    R0,RF                                                            
*                                                                               
         GOTO1 DATCON,DMCB,(2,SVTBSTR),DUB   GET START DT IN YYMMDD             
         GOTO1 ADDAY,(R1),DUB,DUB,(R0)       ADD DSPL                           
         GOTO1 DATCON,(R1),DUB,(8,WORK)                                         
         MVC   PARAS(8),WORK       SAVE ERROR DATE IN PARAS                     
*                                                                               
DTERR1   CLI   OFFLINE,C'Y'        TEST OFFLINE PROCESSING                      
         BE    DTERR10                                                          
         TM    SVOPT2,OP2TBA       TEST-PRT UNASSIGNED SPOTS AS TBA             
         BO    DTERR18                                                          
         CLI   SVT1PR3,C'Y'        PRINT PATTERN ERRS AS TBA                    
         BE    DTERR18                                                          
         CLI   SVT1PR3,C'S'        SUPPRESS PATTERN ERRS AS TBA                 
         BE    DTERR18                                                          
*                                                                               
* ON-LINE ERROR *                                                               
*                                                                               
         MVC   LISTAR,SPACES                                                    
         MVC   LISTAR(24),=C'* ERROR * NO PATTERN FOR'                          
         LA    R1,SVTBPRD                                                       
         LA    R4,LISTAR+25                                                     
         BAS   RE,FMTPRLN                                                       
*                                                                               
         CLI   SVTBPRD2,0          TEST PIGGYBACK                               
         BE    DTERR2                                                           
*                                                                               
         BCTR  R4,0                                                             
         MVI   0(R4),C'/'                                                       
         LA    R4,1(R4)                                                         
         LA    R1,SVTBPRD2                                                      
         BAS   RE,FMTPRLN                                                       
*                                                                               
DTERR2   CLI   SVTBCOPY,0                                                       
         BE    DTERR4                                                           
         MVC   0(1,R4),SVTBCOPY                                                 
         LA    R4,2(R4)                                                         
*                                                                               
         CLC   =X'FF00',DPTLIST    TEST DPTS ACTIVE                             
         BE    DTERR4                                                           
         CLI   DPTLIST,0                                                        
         BE    DTERR4                                                           
         MVC   0(4,R4),=C'DPT='                                                 
         LLC   RE,DPTINDEX                                                      
         LA    RE,DPTLIST(RE)                                                   
         MVC   4(1,R4),0(RE)                                                    
         LA    R4,6(R4)                                                         
*                                                                               
DTERR4   MVC   0(8,R4),PARAS       MOVE SAVED DATE                              
         MVC   CONHEAD,LISTAR      MOVE MESSAGE TO SCREEN                       
*                                                                               
         L     R2,SVTBLINE         POINT TO LINE WITH ERROR                     
         CLI   SVBIGSW,C'Y'        TEST LINE ON SCREEN                          
         BNE   *+8                 YES                                          
         L     R2,ASVTABLE         ELSE POINT TO FIRST LINE                     
         A     R2,ATWA                                                          
         MVI   ERROR,X'FF'         SET ERROR FLAG                               
         J     GENERR2                                                          
*                                                                               
* BUILD ERROR MESSAGE FOR OFF-LINE ERROR FILE *                                 
*                                                                               
DTERR10  XC    ELEM,ELEM                                                        
         LA    R6,ELEM                                                          
         USING EFRECD,R6                                                        
*                                                                               
DTERR12  MVC   EFQUESTR,QUESTOR                                                 
         MVC   EFAGY,AGENCY                                                     
         MVC   EFMED,QMED                                                       
         MVC   EFCLT,QCLT                                                       
         LA    R1,SVTBPRD                                                       
         LA    R4,EFPRDLN                                                       
         BAS   RE,FMTPRLN                                                       
*                                                                               
         LA    R1,SVTBPRD2                                                      
         LA    R4,EFPRDLN2                                                      
         CLI   0(R1),0                                                          
         BE    *+8                                                              
         BAS   RE,FMTPRLN                                                       
*                                                                               
         MVC   EFCOPY,SVTBCOPY                                                  
*                                                                               
         CLI   SVPROF11,C'E'       TEST COPY CODE = EST                         
         BNE   DTERR14                                                          
         LLC   R0,SVTBCOPY                                                      
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  EFCOPY(3),DUB                                                    
*                                                                               
         CLI   DPTLIST,0           TEST DPTS ACTIVE                             
         BE    DTERR14                                                          
         LLC   RE,DPTINDEX                                                      
         LA    RE,DPTLIST(RE)                                                   
         MVC   EFCOPY+3(1),0(RE)                                                
*                                                                               
DTERR14  MVC   EFT0PR11,SVPROF11                                                
         SR    R0,R0                                                            
         ICM   R0,3,SVTBMKST                                                    
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  EFMKT,DUB                                                        
         MVC   EFSTA,SVTBSTA                                                    
         MVC   EFAFFL,SVTBAFFL                                                  
         MVC   EFTYPE,SVTBTYPE                                                  
         MVC   EFERRDT,PARAS                                                    
*                                                                               
         MVC   EFBAGYMD(3),BAGYMD  A-M/CLT                                      
         MVC   EFBPRD(4),SVTBPRD   PRD/SLN/PRD2/SLN2                            
         MVC   EFBMKST(5),SVTBMKST                                              
         MVC   EFPAGE,SEQNUM                                                    
*                                                                               
         L     R1,AERRFILE                                                      
*                                                                               
         PUT   (1),(R6)                                                         
                                                                                
* IF TBA OPT OR PROFILE ARE ON, MISSING PATTERN BIT                             
* IS NOT ON WHEN SOME PATTERNS EXIST FOR FTD TO LTD                             
                                                                                
         TM    SVTBIND,X'01'       TEST MISSING PATTERNS                        
         BO    DTERR16                                                          
         TM    SVTBIND,X'08'       TEST 27 PTTNS OR AUTO PB PTTN ERR            
         BO    DTERR20                                                          
         J     EXIT                                                             
*                                                                               
DTERR16  CLI   SVT1PR3,C'Y'        PRINT PATTN ERRS AS TBA                      
         BE    DTERR18                                                          
         CLI   SVT1PR3,C'S'        SUPPRESS PATTERN ERRS AS TBA                 
         BE    DTERR18                                                          
         TM    SVOPT2,OP2TBA       PRT PATTERN ERRS AS TBA                      
         BO    DTERR18                                                          
         CLI   BPRD,X'FF'          TEST POL REQUEST                             
         BNE   DTERR20                                                          
         CLI   SVPROF7,C'Y'        TEST USER WANTS INST ANYWAY                  
         BNE   DTERR20             NO                                           
*                                                                               
DTERR18  OC    SVTBAPTN,SVTBAPTN   EVER USED                                    
         JZ    EXIT                NO                                           
         MVC   NEXTADDR,SVTBAPTN   RESTORE PATTERN LIST START                   
         XC    SVTBAPTN,SVTBAPTN                                                
         J     EXIT                                                             
*                                                                               
DTERR20  MVI   ERROR,X'FF'         SET ERROR FLAG FOR CALLER                    
         J     GENERR2             TAKE ERROR EXIT                              
         DROP  R5,R6                                                            
         EJECT                                                                  
FMTPRLN  L     RF,ASVCLIST                                                      
*                                                                               
FMTPRLN2 CLC   0(1,R1),3(RF)                                                    
         BE    FMTPRLN4                                                         
         LA    RF,4(RF)                                                         
         CLI   0(RF),C' '                                                       
         BH    FMTPRLN2                                                         
         DC    H'0'                                                             
*                                                                               
FMTPRLN4 MVC   0(3,R4),0(RF)                                                    
         LA    R4,2(R4)            POINT TO END OF PRD                          
         CLI   0(R4),C' '                                                       
         BNE   *+6                                                              
         BCTR  R4,0                                                             
         MVI   1(R4),C'-'                                                       
*                                                                               
         LLC   R0,1(R1)                                                         
         EDIT  (R0),(3,2(R4)),ALIGN=LEFT                                        
         AR    R4,R0                                                            
         LA    R4,3(R4)            POINT 1 BEYOND END                           
         BR    RE                                                               
         LTORG                                                                  
         EJECT                                                                  
*=======================================================                        
* SORT PATTERN TABLE BY DESCENDING START DATE/START TIME                        
* R5 POINTS TO TABLE START                                                      
* WILL SWAP END DATE AND START TIME FOR SORT                                    
* AND THEN REASSIGN PATTERN SEQNUMS AFTER SORT                                  
*=======================================================                        
                                                                                
SORTPTN  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         SR    R0,R0                CLEAR COUNTER                               
         LR    R1,R5                                                            
         MVI   BYTE,C'N'            INDICATE NO TIMES PRESENT                   
         MVI   DLYFLAG,C'N'                                                     
*                                                                               
SORTPT2  CLI   0(R1),0                                                          
         BE    SORTPT5                                                          
         LA    RE,PTNLSTIM-PTNLISTD(R1)     POINT TO START TIME                 
         OC    0(4,RE),0(RE)                TEST FOR START OR END TIME          
         BZ    SORTPT3                      NO                                  
         MVI   BYTE,C'Y'                                                        
         CLC   0(2,RE),=Y(2400)             TEST START TIME = 12M               
         BNE   *+10                         NO                                  
         XC    0(2,RE),0(RE)                CHANGE FOR SORT                     
         TM    PTNLFLAG-PTNLIST(R1),PTNLFDLY  TEST FOR DAILY TIMES              
         BZ    SORTPT3                                                          
         MVI   DLYFLAG,C'Y'                                                     
         MVI   DPTLIST,0                      AND CLEAR DAYPART FLAG!           
*                                                                               
SORTPT3  ICM   RE,15,PTNLEND-PTNLISTD(R1)   GET END DATE/START TIME             
         STCM  RE,12,PTNLSTIM-PTNLISTD(R1)  SAVE END DATE                       
         STCM  RE,3,PTNLEND-PTNLISTD(R1)    STORE TIME OVER DATE                
         LA    R1,L'PTNLIST(R1)                                                 
         BCT   R0,SORTPT2                                                       
*                                                                               
SORTPT5  LPR   R0,R0                                                            
         BZ    SORTPTX                                                          
         CLI   BYTE,C'Y'           TEST ANY TIMES PRESENT                       
         BNE   SORTPT6                                                          
                                                                                
* SORT ON START DATE/START TIME/DESCENDING                                      
                                                                                
         GOTO1 XSORT,DMCB,(1,(R5)),(R0),L'PTNLIST,4,PTNLSTR-PTNLISTD            
*                                                                               
SORTPT6  LA    R0,1                SET LOW SEQNUM                               
         LR    R1,R5                                                            
*                                                                               
SORTPT10 STC   R0,PTNLSEQ-PTNLISTD(R1)      SET NEW SEQNUM                      
*                                                                               
         ICM   RE,15,PTNLEND-PTNLISTD(R1)   GET START TIME/END DATE             
         STCM  RE,12,PTNLSTIM-PTNLISTD(R1)  RESTORE TIME                        
         STCM  RE,3,PTNLEND-PTNLISTD(R1)    AND END DATE                        
*                                                                               
         LA    RE,PTNLSTIM-PTNLISTD(R1)                                         
         OC    0(4,RE),0(RE)                TEST TIME PRESENT                   
         BZ    SORTPT12                     NO                                  
         OC    0(2,RE),0(RE)                TEST FOR 12M START TIME             
         BNZ   *+10                         NO                                  
         MVC   0(2,RE),=Y(2400)             RESTORE                             
*                                                                               
SORTPT12 AHI   R0,1                                                             
         LA    R1,L'PTNLIST(R1)                                                 
         CLI   0(R1),0                                                          
         BNE   SORTPT10                                                         
*                                                                               
SORTPTX  J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
*=====================================================                          
* SUBROUTINE TO READ INST RECAP RECORDS                                         
* AND COMPARE TO PATTERN LIST                                                   
*=====================================================                          
                                                                                
RDRCP    NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    SVINSDT,SVINSDT                                                  
         MVI   SVINSREV,0                                                       
         OI    RUNFLAG,RUNFLTBA                                                 
*                                                                               
         L     R7,ASVTABLE                                                      
         USING SVTABLED,R7                                                      
*                                                                               
         MVI   DPTINDEX,0                                                       
*                                                                               
RDRCP10  TM    SVTBIND,X'01'       TEST NO PATTERN                              
         BO    RDRCP50             YES - SKIP                                   
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING INSKEY,R4                                                        
         MVC   INSKID,=X'0A24'                                                  
         MVC   INSKAM(3),BAGYMD     A-M/CLT                                     
         MVC   INSKPRD(1),SVTBPRD                                               
         MVC   INSKMKT(5),SVTBMKST                                              
         MVC   INSKCOPY(1),SVTBCOPY                                             
*                                                                               
         LLC   RE,DPTINDEX                                                      
         LA    RE,DPTLIST(RE)      POINT TO CURRENT DPT                         
         CLI   0(RE),0                                                          
         BE    RDRCP12                                                          
         CLI   0(RE),X'FF'         ALL DPT ENTRY HAS 0 FOR DPT                  
         BE    RDRCP12                                                          
         MVC   INSKDPT,0(RE)       ELSE MOVE CURENT DPT                         
         DROP  R4                                                               
*                                                                               
RDRCP12  GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    RDRCP14             IF NOT FOUND MUST HAVE CHANGED               
         OI    SVTBIND,X'10'       SET FLAG FOR NEW PATTERN                     
         B     RDRCP50                                                          
*                                                                               
RDRCP14  L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         GOTO1 GETREC                                                           
*                                                                               
         BRAS  RE,BLDRCPSV         SAVE ALL RELEVANT RECAP SUBELS               
         OC    SVLAST10,SVLAST10   X'10' ELEM FOUND?                            
         BNZ   *+12                                                             
         OI    SVTBIND,X'10'       SET FLAG FOR NEW PATTERN                     
         B     RDRCP50             NO                                           
*                                                                               
         L     R0,SVAREAL          GET SAVE AREA LENGTH                         
         SRDL  R0,32                                                            
         D     R0,=F'7'                                                         
         LTR   R0,R1               R0 SET FOR NUMBER OF PATTERNS                
         BZ    RDRCP50                                                          
         L     R1,ASVAREA          R1 POINTS TO FIRST OF SUBELS                 
*                                                                               
RDRCP16  CLC   SVFLTEND,3(R1)      FLIGHT END BEFORE SUBEL START                
         BL    RDRCP18                                                          
         CLC   SVFLTST,5(R1)       FLIGHT START AFTER SUBEL END                 
         BH    RDRCP18                                                          
         B     RDRCP20                                                          
*                                                                               
RDRCP18  LA    R1,7(R1)            NEXT SUBEL                                   
         BCT   R0,RDRCP16                                                       
         B     RDRCP50                                                          
*                                                                               
RDRCP20  L     R6,SVLAST10         GET LAST RECAP EL ADDRESS                    
         USING INSDTAEL,R6                                                      
*                                                                               
         CLC   SVINSDT,INSDATE     SAVED DATE TO ELEM DATE                      
         BH    *+10                                                             
         MVC   SVINSDT,INSDATE     SAVE LATEST INSTRUCTION DATE                 
*                                                                               
         CLC   SVINSREV,INSREV                                                  
         BH    *+10                                                             
         MVC   SVINSREV,INSREV     AND HIGHEST REVISION NUMBER                  
*                                                                               
         MVI   INSPRTSW,C'X'       SET FLAG THERE WERE PREVIOUS INST            
*                                                                               
         TM    INSFLAG,INSFLCTX    COMML TEXT CHANGE                            
         BZ    *+8                                                              
         OI    SVTBIND,X'02'       SET ON CML TEXT CHANGE FLAG                  
         DROP  R6                                                               
*                                                                               
         TM    SVOPT3,OPT3REPT     THIS A REPRINT                               
         BO    RDRCP50              YES, NO PREV COMPARE                        
*                                                                               
         BRAS  RE,CHKSAME          COMPARE CURRENT/PRV PATTERNS                 
*                                                                               
RDRCP50  LLC   RE,DPTINDEX         NEXT DPT                                     
         LA    RE,1(RE)                                                         
         STC   RE,DPTINDEX                                                      
*                                                                               
         CLI   DPTINDEX,L'DPTLIST  TEST REACHED MAX                             
         BE    RDRCP60                                                          
*                                                                               
         LA    RE,DPTLIST(RE)      TEST EOL                                     
         CLI   0(RE),0                                                          
         BE    RDRCP60                                                          
         B     RDRCP10             ELSE PROCESS NEXT DPT                        
*                                                                               
RDRCP60  LA    R7,SVTBNEXT         NEXT TABLE ENTRY                             
         OC    SVTBLINE,SVTBLINE                                                
         BNZ   RDRCP10                                                          
         J     EXIT                                                             
         LTORG                                                                  
                                                                                
*=========================================================                      
* BUILD LIST OF PATTERN SUBELS IN AIO2                                          
* NOTE THERE CAN BE MULTIPLE ELEMENTS                                           
*=========================================================                      
                                                                                
BLDRCPSV NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    SVLAST10,SVLAST10   CLEAR X'10' ELEMENT ADDRESS                  
*                                                                               
         L     RE,AIO2                                                          
         ST    RE,ASVAREA          SET SAVE AREA ADDRESS                        
         ST    RE,ASVNEXT          SET FIRST SUBEL ADDRESS                      
*                                                                               
         L     R0,ASVAREA          SAVE AREA START                              
         LHI   R1,4000             SAVE AREA LEN                                
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE               CLEAR SAVE AREA                              
*                                                                               
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         B     *+8                                                              
*                                                                               
BLDSV2   BRAS  RE,NEXTEL                                                        
         BNE   BLDSVX                                                           
*                                                                               
         USING INSDTAEL,R6                                                      
BLDSV4   CLC   INSPRD1(4),SVTBPRD  PRD-SLN/PTR-SLN                              
         BNE   BLDSV2                                                           
*                                                                               
         CLC   INSPERST,SVFLTEND   TEST PERIOD STARTS AFTER FLT END             
         BH    BLDSV2                                                           
*                                                                               
         CLC   INSPERND,SVFLTST    TEST PERIOD ENDS BEFORE FLT ST               
         BL    BLDSV2                                                           
*                                                                               
         ST    R6,SVLAST10                                                      
*                                                                               
         L     RE,ASVNEXT               GET NEXT SAVE ADDR                      
         LLC   RF,1(R6)                 GET ELEM LEN                            
         AHI   RF,-(INSPTTN-INSDTAEL)   LESS FIXED PORTION LEN                  
         LA    R0,INSPTTN-INSDTAEL(R6)                                          
         LR    R1,RF                                                            
         MVCL  RE,R0                                                            
         ST    RE,ASVNEXT               SET NEXT SAVE ADDR                      
*                                                                               
         LLC   RF,1(R6)                 GET ELEM LEN                            
         AHI   RF,-(INSPTTN-INSDTAEL)   LESS FIXED PORTION LEN                  
         A     RF,SVAREAL               ADD PREVIOUS LEN                        
         ST    RF,SVAREAL               AND SAVE IT                             
*                                                                               
BLDSV6   BRAS  RE,NEXTEL                                                        
         BE    BLDSV4                                                           
*                                                                               
BLDSVX   J     EXIT                                                             
         EJECT                                                                  
*========================================================                       
* COMPARE PATTERN SUBELS TO PREVIOUS LIST IN SVAREA                             
*========================================================                       
                                                                                
CHKSAME  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         SR    R0,R0                                                            
         L     R1,SVAREAL          GET LEN OF SAVED DATA                        
         D     R0,=F'7'                                                         
         LTR   R0,R1               SAVE NUMBER OF SUBEL ENTRIES                 
                                                                                
         L     R1,ASVAREA          POINT TO SAVE AREA                           
*                                                                               
         L     R5,SVTBAPTN         FIRST PATTERN ENTRY                          
         USING PTNLISTD,R5                                                      
*                                                                               
CHKSAM2  LLC   RE,DPTINDEX         MATCH DPT                                    
         LA    RE,DPTLIST(RE)                                                   
         CLC   0(1,RE),PTNLDPT                                                  
         BE    CHKSAM4                                                          
*                                                                               
         LA    R5,PTNLNEXT                                                      
         CLI   PTNLTYPE,0                                                       
         BE    CHKSAMX                                                          
         B     CHKSAM2                                                          
*                                                                               
CHKSAM4  CLC   SVGENEND,3(R1)      INSTR END BEFORE SUBEL START                 
         BL    *+14                                                             
         CLC   SVGENST,5(R1)       INSTR START AFTER SUBEL END                  
         BNH   CHKSAM8             NO - IT'S IN PERIOD                          
*                                                                               
CHKSAM6  LA    R1,7(R1)            NEXT SUBEL                                   
         BCT   R0,CHKSAM4                                                       
         B     CHKSAM10            IF NO MORE, IT'S A NEW PATTERN               
*                                                                               
CHKSAM8  CLC   PTNLREF,0(R1)       SAME REF/SUBL                                
         BE    CHKSAM12                                                         
*                                                                               
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         ICM   RE,7,PTNLREF                                                     
         ICM   RF,7,0(R1)                                                       
         SRL   RE,10                                                            
         SRL   RF,10                                                            
         CR    RE,RF                                                            
         BE    CHKSAM6                                                          
         CLC   =XL3'FFFFFF',0(R1)    WAS THIS A TBA                             
         BNE   CHKSAM6                                                          
         OI    PTNLFLAG,PTNLNEW    SET FLAG FOR NEW PATTERN                     
         B     CHKSAMX                                                          
*                                                                               
CHKSAM10 OI    PTNLFLAG,PTNLCHGE   SET FLAG FOR CHANGED PATTERN                 
         NI    RUNFLAG,X'FF'-RUNFLTBA                                           
         B     CHKSAMX                                                          
*                                                                               
CHKSAM12 CLC   =XL3'FFFFFF',PTNLREF  WAS THIS A TBA                             
         BE    *+8                                                              
         NI    RUNFLAG,X'FF'-RUNFLTBA                                           
*                                                                               
         CLC   PTNLSTR(4),3(R1)    SAME DATES                                   
         BE    CHKSAM20                                                         
         CLC   SVGENST,3(R1)       INSTR START BEFORE SUBEL START               
         BL    CHKSAM14            YES - SO DATES ARE NEW                       
         CLC   PTNLEND,5(R1)       END DATE SAME                                
         BE    CHKSAM20                                                         
*                                                                               
CHKSAM14 OI    SVTBIND,X'20'       SET FLAG FOR NEW DATES                       
         B     CHKSAMX                                                          
*                                                                               
CHKSAM20 LA    R5,PTNLNEXT                                                      
*                                                                               
CHKSAM22 CLI   PTNLTYPE,0          TEST EOL                                     
         BE    CHKSAM24            YES                                          
         LA    R1,7(R1)            NEXT SUBEL                                   
         BCT   R0,CHKSAM2                                                       
         B     CHKSAM26            MORE ENTRIES/NO MORE SUBELS                  
*                                                                               
CHKSAM24 CHI   R0,1                TEST MORE SVAREA ENTRIES                     
         BNH   CHKSAM28            NO - SO THEY MATCH!                          
*                                                                               
CHKSAM26 OI    SVTBIND,X'10'       SET FLAG FOR NEW PATTERN                     
         B     CHKSAMX                                                          
*                                                                               
CHKSAM28 TM    SVTBIND,X'02'       ANY COMML TEXT CHANGES                       
         BO    *+8                                                              
         OI    SVTBIND,X'80'       SET FLAG FOR DUP INST                        
*                                                                               
CHKSAMX  XIT1                                                                   
         DROP  R5                                                               
         LTORG                                                                  
         EJECT                                                                  
*========================================================                       
* SUBROUTINE TO PRINT BOXES AROUND TEXT                                         
* AIO MUST HAVE RECORD ADDRESS                                                  
* ELCODE MUST CONTAIN COMMENT ELEM CODE                                         
* P1  (1) = MAXIMUM LEN OF EXPANDED COMMENT                                     
* P1+1(3) = EXPANDED COMMENT OUTPUT AREA                                        
*========================================================                       
                                                                                
BOXER    NTR1  BASE=*,LABEL=*                                                   
                                                                                
* FIRST - FIND LENGTH OF LONGEST COMMENT *                                      
                                                                                
         L     R7,0(R1)            GET OUTPUT AREA ADDRESS                      
         LLC   R4,0(R1)            GET OUTPUT RECORD SIZE                       
*                                                                               
         L     R6,AIO                                                           
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         SR    R5,R5                                                            
*                                                                               
BOX2     LLC   RE,1(R6)                                                         
         CLC   =C'BOX=',3(R6)                                                   
         BNE   *+8                                                              
         AHI   RE,-4                                                            
         CR    R5,RE                                                            
         BH    *+6                                                              
         LR    R5,RE                                                            
         BRAS  RE,NEXTEL                                                        
         BE    BOX2                                                             
*                                                                               
* LENGTH IN R5 INCLUDES 3 FOR ELCODE/LEN/SEQ - NOW ADJUST FOR                   
* '*/SP' AND 'SP/*' AT EITHER END                                               
*                                                                               
         LA    R5,1(R5)                                                         
*                                                                               
* CREATE ROW OF *'S THIS LENGTH                                                 
*                                                                               
         EX    R4,BOXSPC                                                        
         MVI   0(R7),C'*'                                                       
         BCTR  R5,0                ADJUST FOR FIRST *                           
         BCTR  R5,0                SET FOR EX                                   
         EX    R5,BOXR7                                                         
         AR    R7,R4               POINT TO NEXT OUTPUT LINE                    
         LA    R5,2(R5)            RESTORE LENGTH                               
*                                                                               
         L     R6,AIO                                                           
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         EJECT                                                                  
BOX4     EX    R4,BOXSPC                                                        
*                                                                               
         LLC   RE,1(R6)                                                         
         LA    RF,3(R6)                                                         
*                                                                               
         CLC   =C'BOX=',0(RF)                                                   
         BNE   *+12                                                             
         AHI   RE,-4                                                            
         LA    RF,4(RF)                                                         
*                                                                               
         MVI   0(R7),C'*'                                                       
         AHI   RE,-4               SET FOR EX                                   
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   2(0,R7),0(RF) *EXECUTED*                                         
*                                                                               
         LA    RE,0(R7,R5)         POINT TO END OF LINE                         
         BCTR  RE,0                BACK UP                                      
         MVI   0(RE),C'*'                                                       
         AR    R7,R4               POINT TO NEXT LINE                           
         BRAS  RE,NEXTEL                                                        
         BE    BOX4                                                             
*                                                                               
         EX    R4,BOXSPC                                                        
         MVI   0(R7),C'*'                                                       
         BCTR  R5,0                ADJUST FOR FIRST *                           
         BCTR  R5,0                SET FOR EX                                   
         EX    R5,BOXR7                                                         
         AR    R7,R4                                                            
         MVI   0(R7),0             SET END OF BUFFER FLAG                       
         J     EXIT                                                             
*                                                                               
BOXR7    MVC   1(0,R7),0(R7)  *EXECUTED*                                        
BOXSPC   MVC   0(0,R7),SPACES *EXECUTED*                                        
         EJECT                                                                  
*========================================================                       
* SUBROUTINE TO PRINT FAX COPIES FROM TSAR                                      
*========================================================                       
                                                                                
PCOPY    NTR1  BASE=*,LABEL=*                                                   
         L     R3,ATWA                                                          
         USING T216FFD,R3                                                       
*                                                                               
         OC    TSARBYTE,TSARBYTE   ANYTHING TO PRINT                            
         JZ    EXITX                                                            
*                                                                               
         CLI   TRAWRKRH+5,0        TEST OPTICA                                  
         BZ    PCOPY1              NO                                           
         CLI   SVT1PR9,C'Y'        IF COVER LETTER INSTR, NO PRINT              
         JE    EXITX                                                            
         BAS   RE,PRTCOPT                                                       
         J     PRTC08                                                           
*                                                                               
PCOPY1   MVI   MAXLINES,FAXMAX     SET PAGE SIZE                                
         MVI   SPMODE,X'FF'                                                     
         MVI   PQSW,X'FF'                                                       
         GOTO1 SPOOL,DMCB,(R8)    FORCE CLOSE OF SPOOL                          
         MVI   PQSW,1                                                           
*                                                                               
         OC    ATSAR,ATSAR                                                      
         JZ    EXITX                                                            
*                                                                               
         SR    R6,R6                                                            
         ICM   R6,3,SPOOLRPN                                                    
*                                                                               
         LA    R1,ELEM                                                          
         ST    R1,SPOOLQLK                                                      
         XC    ELEM(128),ELEM                                                   
         USING PQPLD,R1                                                         
         MVI   QLEXTRA,X'FF'       NEW PARM LIST                                
         OI    SPOOLIND,X'40'      USER VALUES PRESENT                          
*                                                                               
         TM    WHEN,X'40'                                                       
         BZ    *+10                                                             
         MVC   QLTYP1,SVQLTYP1                                                  
         MVC   QLARC,SVQLARC                                                    
         MVI   QLEXTRA,X'FF'                                                    
         DROP  R1                                                               
*                                                                               
         CLI   OFFLINE,C'Y'        OFFLINE, DO ALL FAXES AS 1 ENTRY             
         BNE   PRTC06                                                           
*                                                                               
         ICM   RE,15,TWAMASTC                                                   
         BNZ   *+6                                                              
         DC    H'0'                                                             
         L     RF,MCVREMOT-MASTD(RE)                                            
         USING REMOTED,RF                                                       
*                                                                               
* SEE IF ORIGINALLY PRINTING DDS SHOP                                           
*                                                                               
         OC    MCREMOTE-MASTD(,RE),MCREMOTE-MASTD(RE)                           
         BZ    PRTC02              YES                                          
*                                                                               
         LA    R1,MCREMOTE-MASTD(,RE)                                           
         MVC   REMOTSYS,MCS2OSYS-MASTD(RE)                                      
         MVC   REMOTPRG,REMOTPRG-REMOTED(R1)                                    
         MVC   REMOTFRM,REMOTFRM-REMOTED(R1)                                    
         MVC   REMOTJID,REMOTJID-REMOTED(R1)                                    
         MVC   REMOTDST,REMOTDST-REMOTED(R1)                                    
         MVC   REMOTCLS,REMOTCLS-REMOTED(R1)                                    
         MVC   REMOTCPY,REMOTCPY-REMOTED(R1)                                    
         B     PRTC06                                                           
*                                                                               
* OPEN DIRECT ENTRY (WHICH WILL BE LOST) TO PRESERVE REAL QUEUE ENTRY *         
*                                                                               
PRTC02   L     R1,MCVREMOT-MASTD(,RF)                                           
*                                                                               
         MVC   REMOTSYS,MCS2OSYS-MASTD(RE)                                      
         MVC   REMOTJID,=C'STA'                                                 
         MVC   REMOTDST,TWAORIG                                                 
         MVI   REMOTCLS,C'A'                                                    
         MVI   REMOTCPY,1                                                       
         DROP  RF                                                               
*                                                                               
         GOTO1 OPENPQ                                                           
         MVC   P(5),=C'DUMMY'                                                   
         GOTO1 SPOOL,DMCB,(R8)                                                  
         MVI   SPMODE,X'FE'        DELETE THIS ENTRY                            
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)    FORCE CLOSE OF SPOOL                          
         MVI   PQSW,1                                                           
*                                                                               
         ICM   RE,15,TWAMASTC                                                   
         MVI   MCOUTPUT-MASTD(RE),C'P'                                          
         L     RF,MCVREMOT-MASTD(RE)                                            
         USING REMOTED,RF                                                       
         XC    REMOTKEY,REMOTKEY                                                
         DROP  RF                                                               
         LA    R1,ELEM                                                          
         USING PQPLD,R1                                                         
         MVC   QLDESC,SPACES                                                    
         MVC   QLDESC(4),RCPROG                                                 
         MVC   QLCLASS,PLCLASS                                                  
         MVC   QLSUBID,=C'FTA'                                                  
*                                                                               
PRTC06   GOTO1 OPENPQ                                                           
*                                                                               
PRTC08   SR    RF,RF                                                            
         ST    RF,SPECS            NO HEADING SPECS                             
         ST    RF,HEADHOOK            HDHK                                      
         ST    RF,MIDHOOK             MIDHK                                     
         ST    RF,FOOTHOOK            FTHK                                      
*                                                                               
         LA    R2,TSARWK           GET ADDR OF TSAR TABLE                       
         USING TSARD,R2                                                         
*                                                                               
         LA    RF,1                                                             
         MVC   REMUSER,SVREMUSR                                                 
*                                                                               
         LA    R7,ELEM                                                          
         USING TSBUFD,R7                                                        
*                                                                               
         XC    ELEM,ELEM                                                        
*                                                                               
PRTC10   DS    0H                                                               
         SR    RF,RF                                                            
         ICM   RF,7,TSBUFKEY                                                    
         LA    RF,1(,RF)                                                        
         STCM  RF,7,TSBUFKEY                                                    
         CLI   OFFLINE,C'Y'        THIS OFFLINE                                 
         BNE   PRTC12                                                           
         MVI   TSOFFACT,TSARDH                                                  
         B     PRTC14                                                           
*                                                                               
PRTC12   DS    0H                                                               
         MVI   TSACTN,TSARDH                                                    
*                                                                               
PRTC14   DS    0H                                                               
         CLI   OFFLINE,C'Y'                                                     
         BNE   *+12                                                             
         CLI   TRAWRKRH+5,0        TEST OPTICA                                  
         BNE   PRTC16              YES                                          
*                                                                               
         XC    ELEM+5(L'ELEM-5),ELEM+5                                          
         GOTO1 ATSAR,TSARD                                                      
*                                                                               
         TM    TSERRS,X'90'        END OF FILE                                  
         BNZ   PRTC50                                                           
         CLI   TSERRS,0                                                         
         BE    PRTC20                                                           
         DC    H'0'                                                             
*                                                                               
PRTC16   L     R1,ATSARFIL                                                      
         LA    R0,ELEM                                                          
         GET   (1),(0)                                                          
*                                                                               
PRTC20   CLC   ELEM+99(9),=C'REQUESTOR'                                         
         BNE   PRTC25                                                           
         MVC   ELEM+109(3),REMUSER                                              
         SR    R0,R0                                                            
         ICM   R0,3,SPOOLRPN                                                    
         EDIT  (R0),(5,ELEM+113),ALIGN=LEFT                                     
*                                                                               
PRTC25   MVC   SPACING,TSBUFSPC                                                 
*                                                                               
         CLI   TRAWRKRH+5,0        TEST OPTICA                                  
         BNE   PRTC27              YES- DO NOT PRINT 'AGENCY COPY'              
         CLC   ELEM+90(6),=C' PAGE '                                            
         BNE   *+10                                                             
         MVC   ELEM+72(17),=C'** AGENCY COPY **'                                
*                                                                               
PRTC27   CLI   TSBUFHED,C'Y'       TEST FORCE NEW PAGE                          
         BNE   PRTC30                                                           
         LA    R0,1                                                             
         CLM   R0,7,TSBUFKEY       TEST FIRST PAGE                              
         BE    PRTC30                                                           
         MVI   FORCEHED,C'Y'       THIS JUST SKIPS TO TOP OF PAGE               
*                                                                               
PRTC30   SR    R1,R1                                                            
         ICM   R1,3,TSBUFLEN                                                    
         AHI   R1,-8                                                            
         EX    R1,PRTCMVC                                                       
*                                                                               
         CLC   P,SPACES                                                         
         BNE   *+8                                                              
         MVI   P,0                                                              
*                                                                               
         MVI   LINE,10             SUPPRESS AUTO FORCEHED                       
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     PRTC10              AND BACK FOR THE NEXT LINE                   
*                                                                               
PRTCMVC  MVC   P(0),TSBUFLIN                                                    
*                                                                               
PRTC50   CLI   TRAWRKRH+5,0        TEST OPTICA                                  
         JNE   PRTC52              YES - LEAVE PRINTQ OPEN FOR 97               
*                                                                               
         MVI   SPMODE,X'FF'        CLOSE PRINTQ                                 
         GOTO1 SPOOL,DMCB,(R8)                                                  
         MVI   PQSW,1                                                           
*                                                                               
PRTC52   L     R2,SCRCOPY                                                       
         OC    8(10,R2),8(R2)                                                   
         BZ    PRTC60                                                           
         LLC   R1,SCRCOPY                                                       
         LA    R4,7(R1,R2)                                                      
PRTC54   CLI   0(R4),C' '                                                       
         BH    PRTC56                                                           
         BCT   R4,PRTC54                                                        
         DC    H'0'                                                             
PRTC56   AHI   R4,-5                                                            
         B     PRTC64                                                           
*                                                                               
PRTC60   LA    R4,8(,R2)                                                        
         MVC   0(3,R4),=C'ID='                                                  
         MVC   3(3,R4),REMUSER                                                  
*                                                                               
PRTC64   MVI   6(R4),C','                                                       
         SR    R0,R0                                                            
         ICM   R0,3,SPOOLRPN                                                    
         EDIT  (R0),(5,7(R4)),ALIGN=LEFT                                        
         OI    6(R2),X'80'                                                      
*                                                                               
         STCM  R6,3,SPOOLRPN                                                    
         XC    TSARBYTE,TSARBYTE                                                
*                                                                               
         CLI   OFFLINE,C'Y'                                                     
         BNE   PRTCX                                                            
         CLI   TRAWRKRH+5,0        TEST OPTICA                                  
         JE    PRTCX                                                            
         L     R2,ATSARFIL                                                      
         CLOSE ((R2),)                                                          
*                                                                               
         MVC   P1(26),=C'*** END OF DDS MESSAGE ***'                            
         MVI   LINE,2                                                           
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
PRTCX    J     EXITX                                                            
         DROP  R2                                                               
         EJECT                                                                  
*==================================================================             
* READ OPTICA DATA RECORDS AND APPEND TO EXISTING CLASS G REPORT                
*==================================================================             
                                                                                
PRTCOPT  NTR1                                                                   
         L     R2,ATSARFIL         CLOSE TSARFILE IF OPEN                       
         TM    48(R2),X'10'        TEST FILE OPEN                               
         JZ    EXITX               IF NOT OPEN, NOTHING IN IT                   
         CLOSE ((2),)                                                           
         LTR   RF,RF                                                            
         JNZ   *+2                                                              
*                                                                               
         L     R2,ATSARFIL         NOW REOPEN FOR INPUT                         
         OPEN  ((2),INPUT)                                                      
         LTR   RF,RF                                                            
         JNZ   *+2                                                              
*                                                                               
         BRAS  RE,SNDOPTIC         SEND OPTICA PRINT LINES                      
         J     EXITX                                                            
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
SNDOPTIC NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVI   FORCEHED,C'N'                                                    
         MVI   LINE,2              SUPPRESS HEADLINES                           
         MVC   EDIORIG,AGYORIG                                                  
         MVC   EDIHDR,=CL5'*HDR*'                                               
         MVC   EDIEDICT(13),=C'EDICT=*OPTICA'                                   
         MVI   EDIWIDE,C'L'        SET FOR LANDSCAPE                            
         MVI   EDIPAGE,C'P'                                                     
         MVI   EDITTYPE,EDITPDFQ                                                
         MVI   ALLOWLIN,0                                                       
         MVI   SPACING,1                                                        
         GOTO1 SPOOL,DMCB,(R8)    SEND SPECIAL PRINT LINE                       
*                                                                               
         MVC   EDIDDSID(5),=C'++DDS'                                            
         MVC   EDIDDSID+11(3),=C'SUB'                                           
         MVC   EDIDDSID+15(3),=C'SAI'                                           
         GOTO1 SPOOL,DMCB,(R8)    SEND SPECIAL PRINT LINE                       
*                                                                               
         MVC   EDIDDSID(5),=C'++DDS'                                            
         MVC   EDIIDEN(3),=C'UID'                                               
         MVC   EDICOMN,SVUNIQID                                                 
         GOTO1 SPOOL,DMCB,(R8)    SEND SPECIAL PRINT LINE                       
         J     EXIT                                                             
         LTORG                                                                  
*                                                                               
         GETEL (R6),DATADISP,ELCODE                                             
         EJECT                                                                  
                                                                                
*==================================================================             
*                                                                               
* SUBROUTINE TO PRINT INSTRUCTIONS FOR CURRENT SVTABLE ENTRIES                  
*                                                                               
* IO1 USED TO READ PATTERN RECORDS                                              
* IO2 USED TO FORMAT HEADLINE TEXT FOR USE BY HEADHOOK                          
* IO3 USED TO READ COMMERCIALS/FIX OVERLAID INST RECAP ELEM                     
*                                                                               
* AFTER HEADLINES PRINTED,                                                      
* IO2 + IO3 USED TO HOLD PATTERN TEXT + CMML LIST + SCHED PTTN                  
*                                                                               
*==================================================================             
                                                                                
         DS    0D                                                               
INSPRT   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
*                                                                               
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
*                                                                               
* SEE IF LOCKET HAS THIS LOCKED OUT *                                           
*                                                                               
         CLI   OFFLINE,C'Y'        IF OFFLINE                                   
         BE    PRT005               DON'T BOTHER                                
*                                                                               
         L     R3,ATWA                                                          
         USING T216FFD,R3                                                       
         NI    CONWHENH+4,X'FF'-X'20'   SET OFF VALIDATED                       
*                                                                               
         MVC   DUB,=X'E3030A220A240A25' T=TEST, 01=1 ENTRY,                     
*                                       0A22, 0A24, 0A25 RECS                   
         GOTO1 VALILOC,0                                                        
         OI    CONWHENH+4,X'20'         SET ON VALIDATED AGAIN                  
*                                                                               
         DROP  R3                                                               
*                                                                               
PRT005   L     R7,ASVTABLE                                                      
         USING SVTABLED,R7                                                      
*                                                                               
* MAKE SURE THERE IS SOMETHING USEFUL TO PRINT *                                
*                                                                               
PRT010   TM    SVTBIND,X'08'       TEST MORE THAN 27 PTTN/AUTO P/B ERR          
         BO    PRT026               YES                                         
*                                                                               
         TM    SVTBIND,X'01'       TEST NO PATTERN ERROR                        
         BZ    PRT014               MAYBE NOT                                   
*                                                                               
PRT012   OC    SVTBNXLN,SVTBNXLN                                                
         BZ    PRT026                                                           
         LA    R7,SVTBNEXT                                                      
         B     PRT010                                                           
*                                                                               
PRT014   L     R1,SVTBAPTN                                                      
         LTR   R1,R1                                                            
         BNZ   PRT016                                                           
         DC    H'0'                                                             
         CLI   PTNLTYPE-PTNLIST(R1),0  END OF LIST                              
         BNE   PRT016                                                           
         DC    H'0'                                                             
*                                                                               
PRT016   CLI   PTNLTYPE-PTNLIST(R1),0  END OF LIST                              
         BE    PRT012                                                           
         LA    RE,PTNLDSKA-PTNLIST(R1)                                          
         OC    0(4,RE),0(RE)              TEST HIATUS                           
         BZ    *+12                                                             
         CLI   PTNLTYPE-PTNLIST(R1),X'FF' MISSING PATTERN                       
         BNE   PRT018                      NO                                   
         LA    R1,L'PTNLIST(R1)                                                 
         B     PRT016                                                           
*                                                                               
PRT018   L     R7,ASVTABLE                                                      
*                                                                               
         MVI   INSPRTSW,C'Y'       PRESET 'INST PRINTED' SWITCH                 
         MVI   CONTINUE,C'Y'       SET TO PRINT 'CONTINUED'                     
*                                                                               
* READ PREVIOUS INSTRUCTIONS AND COMPARE *                                      
*                                                                               
         BRAS  RE,RDRCP                                                         
*                                                                               
         TM    SVOPT,OPTNEW        TEST NEW INST ONLY                           
         BZ    PRT020                                                           
         CLI   INSPRTSW,C'X'       TEST PREV INST FOUND                         
         JE    EXITX                                                            
                                                                                
*============================================================                   
* TEST DUPLICATE INSTRUCTIONS                                                   
*============================================================                   
                                                                                
PRT020   TM    SVOPT,OPTREV        TEST REVISIONS ONLY                          
         BZ    PRT028                                                           
*                                                                               
PRT024   TM    SVTBIND,X'80'       TEST SAME INST                               
         BZ    PRT028                                                           
         LA    R7,L'SVTBDATA(R7)                                                
         OC    SVTBLINE,SVTBLINE                                                
         BNZ   PRT024                                                           
*                                                                               
PRT026   MVI   INSPRTSW,C'N'       DO NOT PRINT INSTRUCTIONS                    
         CLI   SVPROF15,C'Y'       USE TRAFFIC                                  
         BE    PRT230               YES, NEED TBA RECORD                        
         J     EXIT                                                             
*                                                                               
PRT028   L     R7,ASVTABLE                                                      
         SR    R0,R0                                                            
         ICM   R0,3,SVTBMKST       GET MARKET/STATION                           
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  QMKT,DUB            AND SET FOR HDHK                             
*                                                                               
         GOTO1 DATCON,DMCB,(5,0),(2,TODAYP)                                     
*                                                                               
         L     RE,NEXTADDR                                                      
         MVC   0(8,RE),=C'STA ADDR'                                             
         LA    RE,8(RE)                                                         
         ST    RE,ASVSTAD                                                       
         BRAS  RE,GETSTADR         READ AND FORMAT STATION ADDRESS              
         JNE   EXITX                                                            
*                                                                               
         L     RE,ASVSTAD                                                       
         LA    RE,120(RE)                                                       
         MVC   0(8,RE),=C'FOOTNOTE'                                             
         LA    RE,8(RE)                                                         
         ST    RE,ASVFTNT          ADVANCE POINTER                              
         MVI   0(RE),0             SET EOL FLAG                                 
         LA    RE,1(RE)                                                         
         ST    RE,NEXTADDR         AND SET AS NEXT ADDRESS                      
*                                                                               
         BRAS  RE,GETFOOT          READ AND FORMAT FOOTNOTE TEXT                
*                                                                               
PRT040   BRAS  RE,INIT             INITIALIZE FOR A NEW REPORT                  
         MVI   HEADSW,C'Y'         SET TO PRINT MIDLINES                        
         EJECT                                                                  
*============================================================                   
* GET A PATTERN RECORD                                                          
*============================================================                   
                                                                                
PRT050   L     R3,SVTBAPTN                                                      
         USING PTNLISTD,R3                                                      
*                                                                               
         TM    SVOPT3,OPT3REPT     TEST REPRINT                                 
         BO    *+8                                                              
         BRAS  RE,FIXTIMES         GET PATTERNS IN TIME SEQ W/IN DAY            
*                                                                               
PRT052   L     R4,AIO2             CLEAR PRINT BUFFER (IO2)                     
         ST    R4,ACMLEXP                                                       
         L     R5,SIZEIO                                                        
         BRAS  RE,CLRBUFF                                                       
                                                                                
*===============================================================                
* NOTE THAT INVERTED PRODUCTS SWITCH ONLY PRDS AND LENGTHS                      
* COMMERCIALS ARE INPUT AS PIG-PRD                                              
* EACH COMMERCIAL HAS ITS OWN SLOT IN BUFFER                                    
* THISCML REFERS TO PATTERN ROT POSN FOR THIS CMML OR PAIR                      
*===============================================================                
                                                                                
         TM    PTNLFLAG,PTNLFIPR   TEST INVERT PRODUCTS                         
         BZ    PRT054                                                           
         LLC   R0,SVTBPRD                                                       
         MVC   SVTBPRD,SVTBPRD2                                                 
         STC   R0,SVTBPRD2                                                      
         IC    R0,SVTBSLN                                                       
         MVC   SVTBSLN,SVTBSLN2                                                 
         STC   R0,SVTBSLN2                                                      
*                                                                               
PRT054   L     R4,ACMLEXP                                                       
         USING PLINED,R4                                                        
*                                                                               
PRT054A  MVC   0(32,R4),=C'=====> ROTATE THE FOLLOWING FROM'                    
*                                                                               
         GOTO1 DATCON,DMCB,(2,PTNLSTR),(8,34(R4))                               
         MVI   42(R4),C'-'                                                      
*                                                                               
         GOTO1 (RF),(R1),(2,PTNLEND),(8,43(R4))                                 
*                                                                               
         OC    PTNLSTIM,PTNLSTIM   TEST TIMES ENTERED                           
         BZ    PRT056A                                                          
*                                                                               
         CLC   PTNLSTIM(4),=X'0001FFFF'  TEST NOT REALLY TIME                   
         BE    PRT056A                   YES                                    
         TM    PTNLFLAG,PTNLFDLY         TEST DAILY TIMES                       
         BO    PRT055                    NO                                     
*                                                                               
         MVC   WORK(8),43(R4)      SAVE THE PRINTABLE END DATE                  
         MVC   42(9,R4),SPACES                                                  
*                                                                               
         XC    DUB,DUB                                                          
         MVC   DUB(2),PTNLSTIM                                                  
         TM    PTNLFLG2,PTNLF2ST   TEST DO NOT PRINT PTNSTIM                    
         BZ    *+10                                                             
         MVC   DUB(2),=AL2(2400)   SET 12.00M                                   
         MVC   42(4,R4),=C' AT '                                                
         GOTO1 UNTIME,DMCB,DUB,46(R4)                                           
*                                                                               
PRT054C  LA    R1,46(R4)                                                        
         LA    R1,1(R1)                                                         
         CLI   0(R1),C' '                                                       
         BH    *-8                                                              
         MVC   0(7,R1),=C' UNTIL '                                              
         MVC   7(8,R1),WORK        MOVE SAVED END DATE                          
*                                                                               
         MVC   15(4,R1),=C' AT '                                                
         LA    R1,19(R1)                                                        
         ST    R1,DMCB+4                                                        
         XC    DUB,DUB                                                          
         MVC   DUB(2),PTNLETIM                                                  
*                                                                               
         TM    PTNLFLG2,PTNLF2EN   TEST DO NOT PRINT END TIME                   
         BZ    *+10                                                             
         MVC   DUB(2),=AL2(2359)   SET TO PRINT 11.59P                          
         GOTO1 UNTIME,DMCB,DUB                                                  
         B     PRT056X                                                          
*                                                                               
PRT055   MVC   52(14,R4),=C'RUN DAILY FROM'                                     
*                                                                               
         XC    DUB,DUB                                                          
         MVC   DUB(2),PTNLSTIM                                                  
         GOTO1 UNTIME,DMCB,DUB,67(R4)                                           
*                                                                               
         LA    R1,67(R4)           FIND THE END OF THE TIME                     
         LA    R1,1(R1)                                                         
         CLI   0(R1),C' '                                                       
         BH    *-8                                                              
         MVC   0(4,R1),=C' TO '                                                 
         LA    R1,4(R1)                                                         
*                                                                               
         ST    R1,DMCB+4                                                        
         MVC   DUB(2),PTNLETIM                                                  
         GOTO1 (RF),DMCB,DUB                                                    
*                                                                               
         L     R1,DMCB+4                                                        
         CLI   0(R1),C' '          FIND THE END OF END TIME                     
         BNH   PRT055A                                                          
         LA    R1,1(R1)                                                         
         B     *-12                                                             
PRT055A  MVC   1(4,R1),=C'ONLY'                                                 
         B     PRT056X                                                          
*                                                                               
PRT056A  CLI   SVPROF11,C'D'       COPY CODE DAYPART                            
         BNE   PRT056B             NO                                           
         CLI   SVTBCOPY,C' '                                                    
         BNH   PRT056X                                                          
         MVC   53(8,R4),=C'DAYPART='                                            
         MVC   61(1,R4),SVTBCOPY                                                
         B     PRT056X                                                          
*                                                                               
PRT056B  CLI   SVPROF11,C'A'       COPY CODE ADJACENCY                          
         BNE   PRT056C             NO                                           
         CLI   SVTBCOPY,0          ANY CODE                                     
         BE    PRT056C             NO                                           
         MVC   53(9,R4),=C'ADJ CODE='                                           
         MVC   62(1,R4),SVTBCOPY                                                
         B     PRT056X                                                          
*                                                                               
PRT056C  CLI   PTNLDPT,X'FF'       TEST NO DAYPART ENTRY                        
         BE    PRT056X                                                          
         CLI   PTNLDPT,0           TEST DAYPART ENTERED                         
         BE    PRT056X             NO                                           
         MVC   53(14,R4),=C'DAYPART=  ONLY'                                     
         MVC   61(1,R4),PTNLDPT                                                 
*                                                                               
PRT056X  LA    R4,132(R4)                                                       
         ST    R4,FULL             SAVE PRINT LINE ADDRESS                      
*                                                                               
         CLC   QPRD,=C'POL'                                                     
         BNE   PRT057                                                           
         MVC   0(4,R4),=C'PRD='                                                 
         LA    R1,SVTBPRD                                                       
         BRAS  RE,GPRDS                                                         
         USING SVPRDD,RF                                                        
         MVC   4(3,R4),SVPRDEBC                                                 
         MVC   8(20,R4),SVPRDNM                                                 
         LA    R4,132(R4)                                                       
         ST    R4,FULL                                                          
                                                                                
*==================================================================             
* NOW DO PATTERN CHANGE REASONS                                                 
*==================================================================             
                                                                                
PRT057   OC    SVINSDT,SVINSDT     IF ORIGINAL                                  
         BZ    PRT060                                                           
         CLI   SVINSREV,0          IF REVISION ZERO                             
         BNE   *+12                                                             
         TM    SVOPT,OPTRERUN      AND RERUN (ORIGINAL)                         
         BO    PRT060                                                           
*                                                                               
         TM    PTNLFLAG,PTNLNEW                                                 
         BZ    *+14                                                             
         MVC   0(25,R4),=C'****** NEW PATTERN ******'                           
         B     PRT058                                                           
*                                                                               
         TM    PTNLFLAG,PTNLFSTX+PTNLCHGE   TEST ANY CHANGES                    
         BNZ   *+12                                                             
         TM    SVTBIND,X'20'                                                    
         BZ    PRT060              NO                                           
*                                                                               
         MVC   0(17,R4),=C'****** CHANGES TO'                                   
         LA    R4,18(R4)                                                        
*                                                                               
         TM    PTNLFLAG,PTNLCHGE                                                
         BZ    *+14                                                             
         MVC   0(8,R4),=C'PATTERN,'                                             
         LA    R4,9(R4)                                                         
*                                                                               
         TM    PTNLFLAG,PTNLFSTX                                                
         BZ    *+14                                                             
         MVC   0(13,R4),=C'PATTERN TEXT,'                                       
         LA    R4,14(R4)                                                        
*                                                                               
         TM    SVTBIND,X'20'                                                    
         BZ    *+14                                                             
         MVC   0(9,R4),=C'SCHEDULE,'                                            
         LA    R4,10(R4)                                                        
*                                                                               
         AHI   R4,-2               BACK UP TO LAST CHAR                         
         CLI   0(R4),C','                                                       
         BNE   *+10                                                             
         MVI   0(R4),C' '                                                       
         BCTR  R4,0                                                             
         MVC   2(6,R4),=C'******'                                               
*                                                                               
PRT058   L     R4,FULL             UPDATE FIRST PRINT LINE ADDRESS              
         LA    R4,132(R4)                                                       
         ST    R4,FULL                                                          
*                                                                               
PRT060   BRAS  RE,FMTSLN                                                        
         LA    R4,132(R4)                                                       
*                                                                               
         CLI   SVTBPRD2,0          TEST REAL PIGGYBACK                          
         BE    PRT062              NO                                           
*                                                                               
         MVC   0(4,R4),=C'PRD='                                                 
         LA    R1,SVTBPRD                                                       
         BRAS  RE,GETPRD                                                        
         MVC   4(3,R4),0(RF)       MOVE PRD                                     
         LA    R4,132(R4)                                                       
*                                                                               
         MVC   0(4,R4),=C'P/B='                                                 
         LA    R1,SVTBPRD2                                                      
         BRAS  RE,GETPRD                                                        
         MVC   4(3,R4),0(RF)       MOVE PRD                                     
*                                                                               
PRT062   L     R4,FULL             RESTORE PRINT LINE ADDRESS                   
*                                                                               
         CLI   PTNLTYPE,X'FF'      FF IS TBA                                    
         BE    PRT064                                                           
         TM    PTNLFLAG,PTNLFHIA   TEST HIATUS                                  
         BO    *+14                YES                                          
         OC    PTNLDSKA,PTNLDSKA   NO DISK ADDR IS HIATUS                       
         BNZ   PRT064                                                           
*                                                                               
         MVC   PLROT(36),=C'** HIATUS-NO COMMERCIALS ASSIGNED **'               
         MVI   SPACING,2                                                        
         L     R1,ACMLEXP                                                       
         BRAS  RE,PRTBUFF          GOTO SPOOL RTN                               
         MVI   SPACING,1           RESTORE SPACING                              
         B     PRT210              AND CONTINUE                                 
*                                                                               
PRT064   TM    SVTBIND,X'01'       TEST MISSING PATTERN                         
         BO    *+12                                                             
         CLI   PTNLTYPE,X'FF'      TEST TBA PATTERN                             
         BNE   PRT070                                                           
*                                                                               
         CLI   SVT1PR3,C'S'        SUPPRESS TO BE ASSIGNED                      
         BE    PRT210                                                           
*                                                                               
         L     R4,FULL             RESTORE PL ADDRESS                           
         MVC   PLROT(32),=C'** COMMERCIALS TO BE ASSIGNED **'                   
*                                                                               
         CLI   SVPROF11,C'P'       USING 1ST CHAR OF PROG NAME                  
         BNE   *+8                 NO                                           
         BAS   RE,FMTCOD                                                        
*                                                                               
         L     R1,ACMLEXP                                                       
         MVI   SPACING,2                                                        
         BRAS  RE,PRTBUFF          GOTO SPOOL RTN                               
         MVI   SPACING,1           RESTORE                                      
         B     PRT210              AND CONTINUE                                 
         EJECT                                                                  
*=================================================================              
* SUBROUTINE TO EXTRACT PRODUCT CODES/NAMES FROM SAVE AREA                      
*=================================================================              
                                                                                
GETPRD   L     RF,ASVCLIST                                                      
*                                                                               
GETPRD2  CLC   0(1,R1),3(RF)                                                    
         BER   RE                                                               
         LA    RF,4(RF)                                                         
         CLI   0(RF),C' '                                                       
         BH    GETPRD2                                                          
         LA    RF,=C'****'                                                      
         BR    RE                                                               
         EJECT                                                                  
*==================================================================             
* PROCESS A REAL PATTERN ENTRY - NOT HIATUS/MISSING/TBA                         
*==================================================================             
                                                                                
PRT070   XC    KEY,KEY                                                          
         MVC   KEY+14(4),PTNLDSKA                                               
         L     R6,AIO1             USE IO1 FOR PATTERN REC                      
         ST    R6,AIO                                                           
         GOTO1 GETREC                                                           
*                                                                               
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   ADIDFLAG,C'N'                                                    
         TM    PATSTAT1-PATDTAEL(R6),PATSADID                                   
         BZ    *+8                                                              
         MVI   ADIDFLAG,C'Y'                                                    
*                                                                               
         BRAS  RE,BLDCMLS                                                       
         JNZ   EXITX               EXIT IF ERRORS                               
*                                                                               
         L     R6,AIO1             RESTORE AIO TO PATTERN REC ADDRESS           
         ST    R6,AIO                                                           
*                                                                               
         L     R2,ASVCMLS                                                       
         USING SVCMLD,R2                                                        
         OC    0(8,R2),0(R2)       TEST IF ENTRIES IN LIST                      
         BNZ   PRT072              YES                                          
         BRAS  RE,PTTNER           GO DO ERR MSG, RETURN IF OFFLINE             
         MVC   PLROT(34),=C'* NO CMMLS SCHEDULED-BAD PATTERN *'                 
         L     R1,ACMLEXP                                                       
         BRAS  RE,PRTBUFF                                                       
         J     EXIT                                                             
*                                                                               
PRT072   MVI   DOPCT,C'Y'          ASSUME PRINTING ROT AS PCTS                  
         BRAS  RE,SETPCTS          SET PCTS IN SVCMML BUFFER                    
*                                                                               
PRT080   CLI   SVT1PR12,C'Y'       TEST SUPPRESS PATTERN ROT                    
         BE    PRT084                                                           
         CLI   SVCMLPIG,2          IF THIS IS PRD2, NO PRINT                    
         BE    PRT084                                                           
*                                                                               
         CLI   DOPCT,C'Y'                                                       
         BNE   PRT082                                                           
         SR    R0,R0                                                            
         ICM   R0,3,SVCMLPCT                                                    
         EDIT  (R0),(3,PLROT)                                                   
         MVC   PLROT+4(3),=C'PCT'                                               
         B     PRT084                                                           
*                                                                               
PRT082   LLC   RE,SVCMLPOS         NO PCTS SO PRINT ROT LETTER                  
         LA    RE,ALPHATAB-1(RE)                                                
         MVC   PLROT+2(3),=C'( )'                                               
         MVC   PLROT+3(1),0(RE)                                                 
         B     PRT084                                                           
ALPHATAB DC    C'ABCDEFGHIJKLMNO'                                               
*                                                                               
PRT084   LA    R1,PLCML                                                         
         CLI   SVCMLPIG,2          IS THIS PARTNER ENTRY                        
         BNE   *+8                                                              
         LA    R1,1(R1)                                                         
*                                                                               
         MVC   0(12,R1),SVCMLADI                                                
         CLI   SVCMLPIG,1                                                       
         BNE   PRT086                                                           
         MVI   PLCML-1,C'('                                                     
*                                                                               
         LA    R1,11(R1)           POINT TO LAST CHAR                           
         CLI   0(R1),C' '                                                       
         BH    *+8                                                              
         BCT   R1,*-8                                                           
         MVI   1(R1),C'-'                                                       
         B     PRT090                                                           
*                                                                               
PRT086   CLI   SVCMLPIG,2                                                       
         BNE   PRT090                                                           
         LA    R1,11(R1)                                                        
         CLI   0(R1),C' '                                                       
         BH    *+8                                                              
         BCT   R1,*-8                                                           
         MVI   1(R1),C')'                                                       
                                                                                
*==============================================================                 
* PRINT LENGTH OVERRIDES OR LEN= WHEN                                           
* THERE ARE 1 FILM/2 PRDS  (SVCMLPIG=0 IF ONLY 1 FILM)                          
*        OR 2 FILMS/1 PRD  (SVCMLPIG>0 AND SVTBPRD2=0)_                         
*==============================================================                 
                                                                                
PRT090   ST    R4,FULL             SAVE PRINT LINE ADDRESS                      
         CLI   SVCMLOV1,0          TEST FOR SLN OVERRIDE                        
         BE    PRT092              NO                                           
         MVC   PLCMLNAM(9),=C'LEN OVRD='                                        
         LLC   R0,SVCMLOV1                                                      
         BAS   RE,EDTSLN                                                        
         MVC   PLCMLNAM+9(3),DUB                                                
         LLC   R0,SVCMLOV2                                                      
         BAS   RE,EDTSLN                                                        
         LA    RE,PLCMLNAM+11      POINT TO LAST CHAR                           
         CLI   0(RE),C' '                                                       
         BH    *+6                                                              
         BCTR  RE,0                                                             
         MVI   1(RE),C'/'                                                       
         MVC   2(3,RE),DUB                                                      
         B     PRT096                                                           
*                                                                               
PRT092   CLI   SVTBPRD2,0          TEST REAL P/B                                
         BE    PRT094              NO                                           
         CLI   SVCMLPIG,0          YES- TEST ONLY ONE FILM                      
         BNE   PRT100              P/B WITH 2 FILMS - LEN= NOT REQD             
*                                                                               
         LA    R1,PLCMLNAM                                                      
         MVC   0(4,R1),=C'LEN='    PRINT LEN FOR P/B WITH 1 FILM                
         LLC   R0,SVCMLSLN                                                      
         BAS   RE,EDTSLN                                                        
         MVC   4(3,R1),DUB                                                      
         B     PRT096                                                           
*                                                                               
PRT094   CLI   SVCMLPIG,1          NOT A P/B - TEST ONLY ONE FILM               
         BNE   PRT100              ONLY 1 OR 2 OF 2 - NO PRINTING               
*                                                                               
         LA    R1,PLCMLNAM                                                      
         MVC   0(4,R1),=C'LEN='    PRINT FOR FILM 1 OF 2 ONLY                   
         LLC   R0,SVCMLSLN                                                      
         BAS   RE,EDTSLN                                                        
         MVC   4(3,R1),DUB                                                      
*                                                                               
         LA    RE,SVCMLNXT         POINT TO PARTNER                             
         CLI   SVCMLPIG-SVCMLD(RE),2                                            
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LLC   R0,SVCMLSLN-SVCMLD(RE)                                           
         BAS   RE,EDTSLN                                                        
         LA    R1,6(R1)                                                         
         CLI   0(R1),C' '                                                       
         BH    *+6                                                              
         BCTR  R1,0                                                             
         MVI   1(R1),C'/'                                                       
         MVC   2(3,R1),DUB                                                      
*                                                                               
PRT096   LA    R4,132(R4)                                                       
*                                                                               
PRT100   MVC   PLCMLNAM(15),SVCMLNAM                                            
         LA    R4,132(R4)                                                       
*                                                                               
         CLC   SVCMLNM2,SPACES                                                  
         BNH   *+14                                                             
         MVC   PLCMLNAM(20),SVCMLNM2                                            
         LA    R4,132(R4)                                                       
*                                                                               
         CLC   SVCMLNM3,SPACES                                                  
         BNH   *+14                                                             
         MVC   PLCMLNAM(20),SVCMLNM3                                            
         LA    R4,132(R4)                                                       
*                                                                               
         L     R4,FULL             BACK TO FIRST PRINT LINE                     
*                                                                               
         GOTO1 CALLOV,DMCB,0,X'D9000A11'  GET UNTIME ADDRESS                    
         L     RF,0(R1)                                                         
         ST    RF,FULL             SAVE UNTIME ADDRESS                          
*                                                                               
         OC    SVCMLSTM,SVCMLSTM   TEST TIMES GIVEN                             
         BZ    PRT110              NO                                           
         CLI   SVT3PROF+2,C'Y'     PRINT TIMES ON INST?                         
         BNE   PRT110              NO                                           
         TM    SVCMLST,X'80'       TEST TIMES ARE DAILY                         
         BZ    PRT104                                                           
         GOTO1 (RF),(R1),SVCMLSTM,PLTIME                                        
         MVC   PLTIME+132(5),=C'DAILY'                                          
         B     PRT110                                                           
*                                                                               
PRT104   MVC   PLTIME(8),=C'RUN FROM'                                           
         GOTO1 DATCON,DMCB,(2,SVCMLFTD),(4,PLTIME+9)                            
         MVI   PLTIME+14,C'-'                                                   
         XC    DUB,DUB                                                          
         MVC   DUB(2),SVCMLSTM     PASS START TIME ONLY                         
         L     RF,FULL                                                          
         GOTO1 (RF),DMCB,DUB,PLTIME+15                                          
*                                                                               
         MVC   PLTIME+132(8),=C'   UNTIL'                                       
         GOTO1 DATCON,DMCB,(2,SVCMLLTD),(4,PLTIME+132+9)                        
         MVI   PLTIME+132+14,C'-'                                               
         MVC   DUB(2),SVCMLETM                                                  
         L     RF,FULL                                                          
         GOTO1 (RF),DMCB,DUB,PLTIME+132+15                                      
*                                                                               
PRT110   OC    SVCMLHDF,SVCMLHDF                                                
         BZ    PRT112                                                           
         MVC   PLOTHER(7),=C'HIDEF ='                                           
         MVC   PLOTHER+8(12),SVCMLHDF                                           
         LA    R4,132(R4)                                                       
*                                                                               
PRT112   OC    SVCMLCTR,SVCMLCTR                                                
         BZ    PRT114                                                           
         MVC   PLOTHER(7),=C'CTRCUT='                                           
         MVC   PLOTHER+8(12),SVCMLCTR                                           
         LA    R4,132(R4)                                                       
                                                                                
PRT114   OC    SVCMLCLT,SVCMLCLT    ANY CLT CML #                               
         BZ    PRT115               NO                                          
         CLI   SVT1PR7,C'A'        TEST TO PRINT CLT CML #                      
         BE    PRT115              NOT FOR A                                    
         CLI   SVT1PR7,C'N'        TEST TO PRINT CLT CML #                      
         BE    PRT115              NOT FOR N                                    
         MVC   PLOTHER(7),=C'CLT#  ='    ELSE PRINT IT!                         
         MVC   PLOTHER+8(20),SVCMLCLT    CLT CML #                              
         LA    R4,132(R4)          NEXT PRINT LINE                              
*                                                                               
PRT115   CLI   SVCMLTEL,C' '       TEST OLD TLCSTR DATA                         
         BNH   PRT116                                                           
         CLI   SVCMLTLX,C' '       TEST NEW TLCSTR DATA                         
         BH    PRT116                                                           
         MVC   PLOTHER(7),=C'TLCSTR='                                           
         MVC   PLOTHER+8(8),SVCMLTEL                                            
         LA    R4,132(R4)                                                       
*                                                                               
PRT116   CLI   SVCMLTLX,C' '       TEST FOR EXTENDED DATA                       
         BNH   PRT116A                                                          
         MVC   PLOTHER(7),=C'TC#   ='                                           
         MVC   PLOTHER+8(27),SVCMLTLX                                           
         LA    R4,132(R4)                                                       
*                                                                               
PRT116A  CLI   SVCMLTHD,C' '                                                    
         BNH   PRT116B                                                          
         MVC   PLOTHER(7),=C'TC/HD#='                                           
         MVC   PLOTHER+8(27),SVCMLTHD                                           
         LA    R4,132(R4)                                                       
*                                                                               
PRT116B  CLI   SVCMLTC1,C' '                                                    
         BNH   PRT116C                                                          
         MVC   PLOTHER(7),=C'TALCYC='                                           
         MVC   PLOTHER+8(17),SVCMLTC1                                           
         MVC   PLOTHER+26(17),SVCMLTC2                                          
         LA    R4,132(R4)                                                       
*                                                                               
PRT116C  CLI   SVCMLCBC,C' '                                                    
         BNH   PRT118                                                           
         MVC   PLOTHER(8),=C'CBC/SRC#='                                         
         MVC   PLOTHER+9(9),SVCMLCBC                                            
         LA    R4,132(R4)                                                       
*                                                                               
PRT118   BRAS  RE,CHKLINES         SET SEEMORE FLAG                             
*                                                                               
PRT120   L     R4,ACMLEXP          FIND LAST PRINT LINE                         
         SR    R5,R5                                                            
*                                                                               
PRT122   CLI   0(R4),0             MAYBE FORCING A LINE TO PRINT                
         BE    *+14                                                             
         CLC   0(132,R4),SPACES                                                 
         BNH   PRT124                                                           
         LA    R4,132(R4)                                                       
         BCT   R5,PRT122                                                        
*                                                                               
PRT124   LPR   R5,R5                                                            
         BP    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   SVCMLPIG,1          TEST FIRST OF PIGGYBACK                      
         BE    PRT136              YES - GO DO PARTNER                          
*                                                                               
         CLI   DOPCT,C'Y'          TEST ROT PRINTS AS PCTS                      
         BE    PRT130              YES                                          
         CLI   SVT1PR12,C'Y'       TEST SUPPRESS PATTERN ROT                    
         BE    PRT130                                                           
*                                                                               
         CLI   SVCMLNXT,0          TEST ANY MORE COMMERCIALS                    
         BNE   PRT132              YES - DEFER ROTATION PRINTING                
         BRAS  RE,FMTROT           ELSE FORMAT ROTATION LINE                    
         LA    R4,132(R4)                                                       
         LA    R5,1(R5)                                                         
*                                                                               
PRT130   CLI   SVCMLNXT,0          TEST ANY MORE COMMERCIALS                    
         BNE   PRT132              YES                                          
         TM    RUNFLAG,RUNFLMOR    ANY ADDITIONAL DATA TO PRINT?                
         BZ    PRT132              NO                                           
         NI    RUNFLAG,X'FF'-RUNFLMOR     RESET FLAG NOW                        
*                                                                               
         MVI   0(R4),0             SET TO SKIP A LINE BEFORE                    
         LA    R4,132(R4)                                                       
         MVC   0(SEEMOREX-SEEMORE,R4),SEEMORE                                   
         LA    R5,2(R5)            AND SEE MORE TOOK 2 LINES !                  
*                                                                               
PRT132   STC   R5,ALLOWLIN                                                      
         C     R2,ASVCMLS          TEST FIRST COMMERCIAL                        
         BNE   PRT132A                                                          
         CLI   ALLOWLIN,6                                                       
         BH    PRT132A                                                          
         MVI   ALLOWLIN,6                                                       
*                                                                               
PRT132A  L     R1,ACMLEXP                                                       
         BRAS  RE,PRTBUFF          PRINT AND CLEAR BUFFER                       
*                                                                               
PRT134   CLI   SVCMLNXT,0          TEST ANY MORE COMMERCIALS                    
         BE    PRT150              NO                                           
*                                                                               
PRT136   LA    R2,SVCMLNXT         POINT TO NEXT COMMERCIAL                     
         CLI   SVCMLPIG,2                                                       
         BE    *+8                                                              
         L     R4,ACMLEXP          RESET BUFFER POINTER                         
*                                                                               
         MVI   0(R4),0             SET TO SKIP A LINE                           
         LA    R4,132(R4)                                                       
         B     PRT080                                                           
         EJECT                                                                  
*==========================================================                     
* DEAL WITH SPECIAL PRINT ITEMS                                                 
*==========================================================                     
                                                                                
PRT150   L     R4,ACMLEXP          START OF PRINT BUFFER                        
         MVI   0(R4),0             SET TO SKIP A LINE                           
         LA    R4,132(R4)                                                       
*                                                                               
         CLI   SVPROF11,C'P'       USING 1ST CHAR OF PROG NAME                  
         BNE   PRT152              NO                                           
         BAS   RE,FMTCOD                                                        
         B     PRT160                                                           
*                                                                               
PRT152   DS    0H                                                               
         EJECT                                                                  
*==========================================================                     
* FORMAT PATTERN COMMENT (IF ANY) ON SAME LINES                                 
*==========================================================                     
                                                                                
PRT160   MVI   ELCODE,X'40'                                                     
         L     R6,AIO                                                           
         BRAS  RE,GETEL                                                         
         BNE   PRT166                                                           
*                                                                               
         USING PATCMTEL,R6                                                      
*                                                                               
         CLC   =C'BOX=',3(R6)                                                   
         BE    PRT164                                                           
         CLI   SVPROF3,C'Y'        TEST AUTO BOX                                
         BE    PRT164                                                           
                                                                                
*=========================================================                      
* NO BOXES                                                                      
*=========================================================                      
                                                                                
PRT162   LLC   RE,1(R6)                                                         
         AHI   RE,-4                                                            
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),3(R6) *EXECUTED*                                         
         LA    R4,132(R4)                                                       
         BRAS  RE,NEXTEL                                                        
         BE    PRT162                                                           
         B     PRT166                                                           
                                                                                
*=========================================================                      
* BOXES                                                                         
*=========================================================                      
                                                                                
PRT164   GOTO1 VBOXER,DMCB,(132,(R4))                                           
*                                                                               
PRT166   L     R1,ACMLEXP          FIRST PRINT LINE                             
         BRAS  RE,PRTBUFF                                                       
         EJECT                                                                  
*===============================================================                
* UPDATE PATTERN RECORD WITH USE DATE                                           
*===============================================================                
                                                                                
PRT180   MVI   ELCODE,X'10'                                                     
         L     R6,AIO1                                                          
         BRAS  RE,GETEL                                                         
*                                                                               
         USING PATDTAEL,R6                                                      
*                                                                               
         OC    PATUSED,PATUSED     TEST PATTERN USED PREVIOUSLY                 
         BNZ   PRT200                                                           
*                                                                               
         TM    SVOPT,OPTTEST       TEST TEST RUN                                
         BO    PRT200                                                           
         TM    SVOPT3,OPT3REPT     THIS A REPRINT                               
         BO    PRT200                                                           
*                                                                               
         XC    KEY,KEY             REREAD PATTERN RECORD NOW                    
         MVC   KEY+14(4),PTNLDSKA                                               
         L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         GOTO1 GETREC                                                           
*                                                                               
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   PATUSED,BTODAY                                                   
         GOTO1 PUTREC                                                           
*                                                                               
PRT200   BRAS  RE,UPSHIP           UPDATE SHIP RECAP RECORDS                    
         JNZ   EXITX               EXIT IF ERRORS                               
                                                                                
* SAVE PATTERN SPECIAL TEXT IF ANY *                                            
                                                                                
         CLI   PTNLTYPE,X'FF'      THIS A TBA (NO PATTERN)                      
         BE    PRT210                                                           
         MVI   ELCODE,X'50'                                                     
         XC    PTNLTXT,PTNLTXT                                                  
         L     R6,AIO1                                                          
         BRAS  RE,GETEL                                                         
         BNE   *+10                                                             
         MVC   PTNLTXT,3(R6)                                                    
*                                                                               
PRT210   CLI   PTNLNEXT,0          ANY MORE PATTERNS                            
         BE    PRT214              NO                                           
         LA    R3,PTNLNEXT         NEXT PATTERN                                 
         B     PRT052              YES - GO PROCESS                             
*                                                                               
PRT214   TM    PTNLFLAG,PTNLFIPR   TEST INVERT PRODUCTS                         
         BZ    PRT216                                                           
         CLI   SVCMLPIG,2          TEST THIS IS PARTNER                         
         BNE   PRT216                                                           
*                                                                               
         LLC   R0,SVTBPRD          SWITCH INVERTED PRODUCTS BACK                
         MVC   SVTBPRD,SVTBPRD2                                                 
         STC   R0,SVTBPRD2                                                      
         IC    R0,SVTBSLN                                                       
         MVC   SVTBSLN,SVTBSLN2                                                 
         STC   R0,SVTBSLN2                                                      
*                                                                               
PRT216   OC    SVTBNXLN,SVTBNXLN   ANY MORE SVTABLE ENTRIES                     
         BZ    PRT220              NO - DONE                                    
         LA    R7,SVTBNEXT         NEXT SVTABLE ENTRY                           
         B     PRT050                                                           
         DROP  R4                                                               
*                                                                               
PRT220   BRAS  RE,PRTADDL          PRINT ANY TEXT/ADDL AFTER LAST PTTN          
                                                                                
*================================================================               
* RESET COMMERCIAL FLAGS FOR ADDITIONAL DATA                                    
*================================================================               
                                                                                
         L     R4,ASHPLIST                                                      
         USING SVSHPD,R4                                                        
*                                                                               
PRT222   NI    SVSHPFLG,X'FF'-SVSHPF_YES                                        
         LA    R4,SVSHPNXT                                                      
         CLI   0(R4),0             END OF SHIP TABLE                            
         BNE   PRT222                                                           
*                                                                               
         BRAS  RE,PSTX             PRINT SPECIAL TEXT                           
*                                                                               
         BRAS  RE,REQ              GENERATE COV GEN REQUEST                     
*                                                                               
         BRAS  RE,PRTSHIP          PRINT SHIPPING LIST                          
*                                                                               
PRT224   BRAS  RE,UPDRCP           UPDATE INSTRUCTION RECAP RECORDS             
*                                                                               
PRT230   BRAS  RE,TBAUP            TO UPDATE TBA RECORDS                        
         CLI   INSPRTSW,C'N'       NO INSTRUCTIONS PRINTED                      
         BNE   PRT240              INSTR WERE PRINTED                           
         CLI   SVPROF15,C'Y'       TBUYS                                        
         JE    EXIT                YES, DONE                                    
*                                                                               
PRT240   BRAS  RE,DOFEET           PRINT FOOTLINES                              
*                                                                               
         BRAS  RE,PATREQS          DO PATTERN REQUESTS                          
         J     EXIT                                                             
         EJECT                                                                  
FMTROT   NTR1                                                                   
         MVC   0(ROTMSGX-ROTMSG,R4),ROTMSG                                      
         LA    R5,L'ROTMSG+1(R4)                                                
*                                                                               
         L     R6,AIO1                                                          
         MVI   ELCODE,X'32'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LLC   R0,1(R6)                                                         
         AHI   R0,-2               NUMBER OF LETTERS                            
         LA    RF,2                SET FOR ONE SPACE BETWEEN LETTERS            
         CHI   R0,37               IF MORE THAN 37 ENTRIES                      
         BNH   *+8                                                              
         LA    RF,1                THEN NO SPACES                               
         LA    R1,ROTMSGX-ROTMSG+1(R4) FIRST OUTPUT POSN                        
         LA    R6,2(R6)                                                         
*                                                                               
FMTROT2  MVC   0(1,R1),0(R6)                                                    
         AR    R1,RF                                                            
         LA    R6,1(R6)                                                         
         BCT   R0,FMTROT2                                                       
         J     EXIT                                                             
*                                                                               
ROTMSG   DC    C'*****'                                                         
         DC    C' ROTATE COMMERCIALS AS FOLLOWS:'                               
ROTMSGX  EQU   *                                                                
*                                                                               
FMTSLN   NTR1                                                                   
         MVC   0(4,R4),=C'LEN='                                                 
         LLC   R0,SVTBSLN                                                       
         BAS   RE,EDTSLN                                                        
         MVC   4(3,R4),DUB                                                      
         LLC   R0,SVTBSLN2                                                      
         LTR   R0,R0                                                            
         JZ    EXIT                                                             
         BAS   RE,EDTSLN                                                        
         LA    RE,6(R4)            POINT TO LAST CHAR OF LEN=999                
         CLI   0(RE),C' '                                                       
         BH    *+6                                                              
         BCTR  RE,0                                                             
         MVI   1(RE),C'/'                                                       
         MVC   2(3,RE),DUB                                                      
         J     EXIT                                                             
*                                                                               
EDTSLN   CVD   R0,DUB              RETURN PRINTABLE SLN IN DUB(3)               
         OI    DUB+7,X'0F'                                                      
         UNPK  DUB(2),DUB+6(2)                                                  
         MVI   DUB+2,C' '                                                       
         CHI   R0,100                                                           
         BLR   RE                                                               
         UNPK  DUB(3),DUB+6(2)                                                  
         BR    RE                                                               
*                                                                               
         USING PLINED,R4                                                        
FMTCOD   NTR1                                                                   
         LA    R0,FMTCODS                                                       
         L     R1,=A(FMTCODT)                                                   
         A     R1,SPTR04RR                                                      
*                                                                               
FMTCOD2  CLC   SVTBCOPY,0(R1)                                                   
         BE    FMTCOD4                                                          
         LA    R1,L'FMTCODT(R1)   NEXT ENTRY                                    
         BCT   R0,FMTCOD2                                                       
*                                                                               
         MVC   PLCODE(1),SVTBCOPY                                               
         MVC   PLCODE+1(L'FMTCODT-1),0(R1)                                      
         J     EXITX                                                            
*                                                                               
FMTCOD4  MVC   PLCODE(L'FMTCODT),0(R1)                                          
         J     EXITX                                                            
         EJECT                                                                  
*====================================================================           
* SEE IF ANY ADDITIONAL LINES ARE REQUIRED FOR THIS COMMERCIAL                  
*====================================================================           
                                                                                
CHKLINES NTR1                                                                   
*                                                                               
         TM    SVCMLST,X'40'       TEST COMMERCIAL TEXT REQUIRED                
         BO    CHKLIN10                                                         
*                                                                               
         CLC   SVCMLHSE,SPACES     TEST PRODUCTION HOUSE TO PRINT               
         BH    CHKLIN10                                                         
*                                                                               
         OC    SVCMLDDT,SVCMLDDT   TEST DESTROY DATE                            
         BNZ   CHKLIN10                                                         
*                                                                               
         CLI   SVT2PR02,C'Y'       TEST TO SUPPRESS TYPE                        
         BE    *+12                                                             
         CLI   SVCMLTYP,C' '       TEST COMMERCIAL TYPE TO PRINT                
         BH    CHKLIN10                                                         
*                                                                               
         B     CHKLINX             IDIOT!                                       
*                                                                               
CHKLIN10 OI    RUNFLAG,RUNFLMOR                                                 
*                                                                               
CHKLINX  J     EXIT                                                             
*                                                                               
SEEMORE  DC    C'+++++'                                                         
         DC    C' SEE ADDITIONAL COMMERCIAL INFORMATION BELOW '                 
         DC    C'+++++'                                                         
SEEMOREX EQU   *                                                                
         EJECT                                                                  
PRTERR   CLI   ERROPT,C'Y'                                                      
         JE    NEQXIT                                                           
         GOTO1 ERREX                                                            
*                                                                               
PRTERR2  CLI   ERROPT,C'Y'                                                      
         JE    NEQXIT                                                           
         GOTO1 ERREX2                                                           
         LTORG                                                                  
         EJECT                                                                  
*============================================================                   
* IF ONE DAY HAS MULTIPLE PATTERNS, MAKE SURE IN TIME SEQ                       
*============================================================                   
                                                                                
FIXTIMES NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R3,SVTBAPTN                                                      
         USING PTNLISTD,R3                                                      
*                                                                               
FIXTIM2  OC    PTNLSTIM,PTNLSTIM   TEST PATTERN HAS START TIME                  
         BZ    FIXTIM20            NO - NEXT PATTERN                            
         CLC   PTNLSTR,PTNLEND     TEST STARTS/ENDS SAME DAY                    
         BNE   FIXTIM20            NO - NEXT PATTERN                            
                                                                                
         ST    R3,DMCB             SAVE AS START OF SORT                        
         LA    R0,1                COUNT NUMBER OF PATTERNS                     
*                                                                               
         CLC   PTNLSTIM,=H'2400'   TEST MIDNIGHT START                          
         BNE   *+10                                                             
         XC    PTNLSTIM,PTNLSTIM   SET TO ZERO FOR SORT                         
*                                                                               
FIXTIM4  CLI   PTNLTYPE+L'PTNLIST,0    TEST MORE ENTRIES                        
         BE    FIXTIM10                                                         
*                                                                               
         LA    RE,PTNLSTR          POINT TO CURRENT START DATE                  
         LA    R3,PTNLNEXT         POINT TO NEXT PATTERN                        
*                                                                               
         CLC   PTNLSTR,0(RE)       TEST NEXT PATTERN SAME DAY                   
         BNE   FIXTIM10                                                         
*                                                                               
         CLC   PTNLSTR,PTNLEND     TEST STARTS/ENDS SAME DAY                    
         BNE   FIXTIM10            IF NOT, IT'S GONNA BE FINE                   
         OC    PTNLSTIM,PTNLSTIM   TEST HAS START TIME                          
         BZ    FIXTIM10            NO - SHOULDN'T HAPPEN, BUT ...               
*                                                                               
         AHI   R0,1                THIS ONE GETS SORTED                         
*                                                                               
         SR    RE,RE               ADJUST TO 0600-3000 CLOCK                    
         ICM   RE,3,PTNLSTIM                                                    
*&&DO                                                                           
         CHI   RE,559                                                           
         BH    *+8                                                              
         AHI   RE,2400                                                          
*&&                                                                             
         CHI   RE,2400             FOR CALENDAR DAYS                            
         BNE   *+8                                                              
         AHI   RE,-2400            MIDNIGHT BECOMES ZERO                        
         STCM  RE,3,PTNLSTIM                                                    
         B     FIXTIM4                                                          
*                                                                               
FIXTIM10 CHI   R0,1                                                             
         BNH   FIXTIM12                                                         
*                                                                               
         GOTO1 XSORT,DMCB,,(R0),L'PTNLIST,2,PTNLSTIM-PTNLISTD                   
*                                                                               
FIXTIM12 L     RE,DMCB             POINT TO FIRST SORTED ENTRY                  
         LA    RE,PTNLSTIM-PTNLISTD(RE)   POINT TO START TIME                   
         OC    0(2,RE),0(RE)       TEST IT IS ZERO (MIDNIGHT)                   
         BNZ   *+10                                                             
         MVC   0(2,RE),=H'2400'    SET IT BACK TO MIDNIGHT                      
*                                                                               
FIXTIM20 LA    R3,PTNLNEXT                                                      
*                                                                               
FIXTIM22 CLI   0(R3),0                                                          
         BNE   FIXTIM2                                                          
         J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
*======================================================================         
* UPDATE INSTRUCTION RECAP RECORDS FOR ALL SVTABLE ENTRIES                      
*======================================================================         
                                                                                
UPDRCP   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R7,ASVTABLE                                                      
         USING SVTABLED,R7                                                      
*                                                                               
         MVI   DPTINDEX,0                                                       
*                                                                               
UPDR2    L     R3,SVTBAPTN                                                      
         USING PTNLISTD,R3                                                      
                                                                                
* SET DLYFLAG IF ANY DAILY PATTERNS                                             
                                                                                
         MVI   DLYFLAG,C'N'                                                     
UPDR4A   TM    PTNLFLAG,PTNLFDLY                                                
         BO    UPDR4B                                                           
         LA    R3,PTNLNEXT                                                      
         CLI   0(R3),0                                                          
         BNE   UPDR4A                                                           
         B     UPDR4X                                                           
*                                                                               
UPDR4B   MVI   DLYFLAG,C'Y'                                                     
*                                                                               
UPDR4X   TM    SVTBIND,X'01'       TEST MISSING PATTERN                         
         BO    UPDR90                                                           
                                                                                
*=========================================================                      
* CREATE SAVE AREA IN AIO2(4000)                                                
* NOTE THERE CAN BE MULTIPLE ELEMENTS                                           
*=========================================================                      
                                                                                
         L     RE,AIO2                                                          
         ST    RE,ASVAREA          SET SAVE AREA ADDRESS                        
         ST    RE,ASVNEXT          SET FIRST SUBEL ADDRESS                      
*                                                                               
         L     R0,ASVAREA          SAVE AREA START                              
         LHI   R1,4000             SAVE AREA LEN                                
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE               CLEAR SAVE AREA                              
*                                                                               
         L     R6,AIO1             USE IO1                                      
         ST    R6,AIO                                                           
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING INSKEY,R4                                                        
         MVC   INSKID,=X'0A24'                                                  
         MVC   INSKAM(3),BAGYMD     A-M/CLT                                     
         MVC   INSKPRD(1),SVTBPRD                                               
         MVC   INSKMKT(5),SVTBMKST                                              
*                                                                               
         CLI   SVPROF11,C'E'      TEST COPY CODE = EST                          
         BNE   UPDR8                                                            
         MVC   INSKCOPY(1),QBEST                                                
*                                                                               
         CLI   DPTLIST,X'C0'       TEST DPTS W/IN EST                           
         BL    UPDR8                                                            
         LLC   RE,DPTINDEX                                                      
         LA    RE,DPTLIST(RE)                                                   
         CLI   0(RE),X'FF'         USE 0 FOR ALL DPT ENTRY                      
         BE    *+10                                                             
         MVC   INSKDPT,0(RE)       ELSE MOVE DPT                                
*                                                                               
UPDR8    CLI   SVPROF11,C'D'      TEST COPY CODE = DAYPART                      
         BNE   *+10                                                             
         MVC   INSKCOPY(1),SVTBCOPY                                             
*                                                                               
         CLI   SVPROF11,C'A'      TEST COPY CODE = ADJ                          
         BNE   *+10                                                             
         MVC   INSKCOPY(1),SVTBCOPY                                             
         DROP  R4                                                               
*                                                                               
         MVI   SVINSNO,0           SET COUNT OF RELATED ELEMS TO ZERO           
         XC    ELEM,ELEM                                                        
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    UPDR10                                                           
*==================================================================             
* CREATE NEW INSTRUCTION RECAP RECORD                                           
*==================================================================             
                                                                                
         XC    0(256,R6),0(R6)                                                  
         MVC   0(13,R6),KEYSAVE                                                 
         MVI   14(R6),24                                                        
         MVC   20(2,R6),AGENCY                                                  
         B     UPDR50                                                           
         EJECT                                                                  
*==================================================================             
* INST RECAP REC FOUND - DEL OR UPD ELEM(S) FOR THIS FLIGHT                     
*==================================================================             
                                                                                
UPDR10   GOTO1 GETREC                                                           
*                                                                               
UPDR12   MVI   ELCODE,X'10'                                                     
         XC    ELEM,ELEM           CLEAR ELEM                                   
*                                                                               
UPDR14   L     R6,AIO                                                           
         BRAS  RE,GETEL                                                         
         B     *+8                                                              
*                                                                               
UPDR20   BRAS  RE,NEXTEL                                                        
         BNE   UPDR50                                                           
*                                                                               
         USING INSDTAEL,R6                                                      
*                                                                               
UPDR22   CLC   INSPRD1(4),SVTBPRD  THIS SAME PRD/SLN/PRD2/SLN2                  
         BNE   UPDR20                                                           
*                                                                               
         CLC   INSPERST,SVFLTEND   TEST PERIOD START AFTER FLT END              
         BH    UPDR20                                                           
         CLC   INSPERND,SVFLTST    TEST PERIOD END BEFORE FLT ST                
         BL    UPDR20                                                           
*                                                                               
         NI    INSFLAG,X'FF'-INSFLCOV-INSFLMAR-INSFLTWX-INSFLCTX                
*                                                                               
         XC    ELEM,ELEM                                                        
         LLC   RE,1(R6)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   ELEM(0),0(R6)                                                    
*                                                                               
         L     RE,ASVNEXT                                                       
         LLC   R1,1(R6)                 GET ELEM LEN                            
         AHI   R1,-(INSPTTN-INSDTAEL)   LESS FIXED PORTION                      
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),INSPTTN     MOVE SUBELS TO SAVE AREA                     
*                                                                               
         LA    RE,1(RE,R1)         BUMP SAVE AREA POINTER                       
         ST    RE,ASVNEXT          AND SAVE                                     
*                                                                               
         GOTO1 VRECUP,DMCB,AIO,(R6)   DELETE ELEMENT                            
*                                                                               
         BRAS  RE,FIRSTEL          REMEMBER - POINTING AT NEXT ELEM             
         BE    UPDR22                                                           
         EJECT                                                                  
*===============================================================                
* UPDATE OR AMEND RECAP DATA ELEMENTS                                           
* FIRST GET INST START DATE-1 AND SAVE IT                                       
*===============================================================                
                                                                                
UPDR50   ICM   R3,15,SVTBAPTN                                                   
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING PTNLISTD,R3                                                      
UPDR52   CLI   DLYFLAG,C'Y'                                                     
         BE    UPDR54                                                           
*                                                                               
         LLC   RE,DPTINDEX                                                      
         LA    RE,DPTLIST(RE)                                                   
         CLC   0(1,RE),PTNLDPT     TEST PTTN ENTRY TO CURRENT DPT               
         BE    UPDR54                                                           
         LA    R3,PTNLNEXT                                                      
         CLI   PTNLTYPE,0                                                       
         BNE   UPDR52                                                           
         B     UPDR90                                                           
*                                                                               
UPDR54   GOTO1 DATCON,DMCB,(2,SVGENST),WORK                                     
         SR    R0,R0                                                            
         BCTR  R0,0                SET TO -1                                    
         GOTO1 ADDAY,(R1),WORK,WORK+6,(R0)                                      
         GOTO1 DATCON,(R1),WORK+6,(2,FULL)  SAVE INST START - 1                 
*                                                                               
         LA    R6,ELEM                                                          
         USING INSDTAEL,R6                                                      
*                                                                               
         L     R4,ASVAREA                                                       
         CLI   0(R6),0             TEST PREVIOUS ELEM FOUND                     
         BE    UPDR64               NO                                          
                                                                                
* SAVE INFO FOR X'14' ELEM                                                      
                                                                                
         LLC   RE,1(R6)            GET ELEM LEN                                 
         AR    RE,R6               POINT TO END                                 
         AHI   RE,-2               POINT TO LAST LTD                            
         MVC   SVLTD,0(RE)         SAVE LAST TELECAST DATE                      
         MVC   SVPRD1(4),INSPRD1   SAVE PRD1/LEN1 PRD2/LEN2                     
         MVC   SVINSREV,INSREV     SAVE REVISION NUMBER                         
         MVC   SVINSDT,INSDATE     SAVE DATE                                    
                                                                                
* NEED TO FIND IF CURRENT INST REPLACE OR AMEND PREVIOUS INST *                 
                                                                                
         L     R4,ASVAREA          POINT TO FIRST SUB-EL                        
         MVC   SVFTD,3(R4)         SAVE FIRST TELECAST DATE                     
*                                                                               
         CLC   3(2,R4),SVGENST     FIRST SUBEL AFTER INST START                 
         BH    UPDR64              YES - OVERWRITE                              
*                                                                               
UPDR60   CLC   5(2,R4),SVGENST     SUBEL END TO INST PER START                  
         BNL   UPDR62               IF HIGH OR EQUAL, GOT IT                    
*                                                                               
         LA    R4,7(R4)            ELSE TRY NEXT                                
         OC    0(7,R4),0(R4)       USED TO CHK 1 BYTE (HIATUS)                  
         BNZ   UPDR60                                                           
         B     UPDR64                                                           
*                                                                               
UPDR62   CLC   3(2,R4),SVGENST     SUBEL START TO INST PER START                
         BNL   UPDR64               EQUAL OR HIGH, OVERWRITE                    
*                                                                               
         MVC   5(2,R4),FULL        ELSE SET NEW END DATE (=START-1)             
         LA    R4,7(R4)                                                         
                                                                                
* UPDATE (OR CREATE) NEW SUBELS IN SAVE AREA                                    
                                                                                
UPDR64   XC    ELEM,ELEM                                                        
         MVI   BYTE,C'Y'           SET ALL TBA FLAG                             
*                                                                               
UPDR66   ST    R3,FULL             SAVE ADDR OF LAST PATTERN USED               
         CLC   PTNLREF,=X'FFFFFF'  TEST TBA PATTERN                             
         BE    *+8                 YES                                          
         MVI   BYTE,C'N'           SET ALL PTTNS NOT TBA                        
*                                                                               
         MVC   0(3,R4),PTNLREF      PATTERN REF                                 
         MVC   3(2,R4),PTNLSTR                                                  
         MVC   5(2,R4),PTNLEND                                                  
*                                                                               
         LA    R4,7(R4)                                                         
         XC    0(7,R4),0(R4)       CLEAR NEXT SUBEL                             
*                                                                               
UPDR68   LA    R3,PTNLNEXT                                                      
         CLI   0(R3),0                                                          
         BE    UPDR70                                                           
*                                                                               
         CLI   DLYFLAG,C'Y'        IF DAILY TIMES,                              
         BE    UPDR66               KEEP GOING                                  
*                                                                               
         LLC   R1,DPTINDEX                                                      
         LA    R1,DPTLIST(R1)                                                   
         CLC   PTNLDPT,0(R1)       TEST RIGHT DPT                               
         BNE   UPDR68                                                           
         B     UPDR66                                                           
                                                                                
* NOW CREATE ELEMENTS FROM SUBELS IN SAVE AREA                                  
                                                                                
UPDR70   CLI   BYTE,C'Y'           TEST ALL PTTNS TBA                           
         BE    UPDR90              YES - NO UPDATE                              
*&&DO                                                                           
* THIS CODE ACTUALLY CHANGES THE DATE INCORRECTLY TO END OF REQUEST             
* PERIOD - LAST TELECAST DATE IS A BETTER DATE!                                 
         L     R3,FULL             GET ADDR OF LAST PATTERN USED                
         AHI   R4,-7               BACK UP TO LAST ENTRY IN TABLE               
         TM    PTNLFLAG,PTNLFUFN   WAS IT A UFN PATTERN                         
         BZ    *+10                                                             
         MVC   5(2,R4),SVFLTEND    THEN MOVE FLIGHT END DATE                    
*&&                                                                             
         L     R4,ASVAREA          POINT TO FIRST SUBEL                         
*                                                                               
UPDR72   XC    ELEM,ELEM                                                        
         LA    R6,ELEM                                                          
         MVI   INSDTAEL,X'10'             SET ELEMENT CODE                      
         MVI   INSDTALN,INSPTTN-INSDTAEL  INITIALIZE LEN                        
*                                                                               
         MVC   INSPRD1(4),SVTBPRD  PRD1/SLN1/PRD2/SLN2                          
         MVC   INSPERST,SVFLTST                                                 
         MVC   INSPERND,SVFLTEND                                                
*                                                                               
         CLI   SVPROF11,C'E'       COPY CODE = EST                              
         BNE   *+8                                                              
         OI    INSFLAG,INSFLEST    SET ON COPY CODE = EST FLAG                  
*                                                                               
         TM    SVTBIND2,SVTBICGR   THIS CABLE STATION WITH GROUP CODE           
         BZ    *+8                                                              
         OI    INSFLAG,INSFLCGR    SET ON CABLE WITH GROUP CODE                 
*                                                                               
         MVC   INSPERNO,SVINSNO    ELEM SEQUENCE NUMBER                         
         LLC   RE,SVINSNO                                                       
         LA    RE,1(RE)                                                         
         STC   RE,SVINSNO                                                       
*                                                                               
         CLI   INSPERNO,0          MULTI ELEM?                                  
         BNE   UPDR73              YES                                          
*                                                                               
         OC    SVINSDT,SVINSDT     TEST NO PREVIOUS INST                        
         BZ    UPDR74                                                           
         TM    SVOPT3,OPT3REPT     THIS A REPRINT                               
         BO    UPDR74                                                           
         TM    SVOPT,OPTRERUN      THIS IS A RERUN                              
         BO    UPDR74                                                           
*                                                                               
UPDR73   LLC   RE,SVINSREV         BUMP REVISION NUMBER                         
         CLI   INSPERNO,0          TEST MULTI-ELEM                              
         BNE   *+8                 YES                                          
         LA    RE,1(RE)            BUMP REVISION NUMBER ONLY ONCE PER           
         STC   RE,SVINSREV         RECORD - SAVE NEW REVISION NUMBER            
*                                                                               
UPDR74   MVC   INSDATE,TODAYP                                                   
         MVC   INSREV,SVINSREV     SET NEW REV NUMBER IN ELEMENT                
*                                                                               
         LLC   R1,INSDTALN                                                      
         LA    RF,INSPTTN                                                       
*                                                                               
UPDR76   MVC   0(7,RF),0(R4)                                                    
*                                                                               
UPDR78A  CLC   0(3,RF),7(R4)       NEXT SUBEL SAME PATTERN                      
         BNE   UPDR78B                                                          
         MVC   5(2,RF),7+5(R4)     UPDATE THE END DATE                          
         LA    R4,7(R4)            AND SKIP TO NEXT ENTRY                       
         OC    0(7,R4),0(R4)       SEE IF MORE ENTRIES                          
         BNZ   UPDR78A             AND CHECK AGAIN                              
*                                                                               
UPDR78B  LA    R1,7(R1)                                                         
         LA    R4,7(R4)                                                         
         LA    RF,7(RF)                                                         
         OC    0(7,R4),0(R4)       ANY MORE TO MOVE                             
         BZ    *+12                                                             
         CHI   R1,249              TEST ELEM FULL                               
         BL    UPDR76                                                           
*                                                                               
         STC   R1,INSDTALN         SET NEW LENGTH                               
         BRAS  RE,LOSEM            GO SEE IF ELEM FITS                          
*                                                                               
         GOTO1 ADDELEM             THEN ADD NEW ELEM                            
*                                                                               
         OC    0(7,R4),0(R4)       TEST MORE SUBELS                             
         BNZ   UPDR72                                                           
*                                                                               
         TM    SVOPT3,OPTBPAT      WAS OPTION BPAT ENTERED                      
         BO    UPDR80              CANNOT RUN SPOT SEED                         
         CLI   SVT2PR11,C'Y'       THEY WANT SPOT SEED RUN?                     
         BNE   *+8                  NO                                          
         BRAS  RE,ASD              GO GENERATE A SPOT SEED REQUEST              
*                                                                               
UPDR80   L     RF,PUTREC                                                        
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+8                                                              
         L     RF,ADDREC                                                        
*                                                                               
         TM    SVOPT3,OPT3REPT     THIS A REPRINT                               
         BO    *+14                                                             
         TM    SVOPT,OPTTEST                                                    
         BO    *+6                                                              
         BASR  RE,RF                                                            
*                                                                               
UPDR90   CLI   DPTLIST,0           TEST DPTS ACTIVE                             
         BE    UPDR92                                                           
         LLC   RE,DPTINDEX                                                      
         LA    RE,1(RE)                                                         
         STC   RE,DPTINDEX                                                      
         CLI   DPTINDEX,L'DPTLIST  TEST REACHED END                             
         BE    UPDR92                                                           
         LA    RE,DPTLIST(RE)      POINT TO DPT ENTRY                           
         CLI   0(RE),0                                                          
         BNE   UPDR2                                                            
*                                                                               
UPDR92   OC    SVTBNXLN,SVTBNXLN   TEST MORE DATA                               
         BZ    UPDR94              NO                                           
*                                                                               
         MVI   DPTINDEX,0                                                       
         LA    R7,SVTBNEXT         PROCESS NEXT ENTRY                           
         TM    SVTBIND,X'01'       TEST MISSING PATTERN                         
         BO    UPDR92                                                           
         B     UPDR2                                                            
*                                                                               
UPDR94   J     EXIT                                                             
         EJECT                                                                  
*=============================================================                  
* CODE TO REMOVE MORE ELEMENTS IF NEW ONE WON'T FIT                             
*=============================================================                  
                                                                                
LOSEM    NTR1                                                                   
*                                                                               
LOSEM2   L     R1,AIO                                                           
         SR    R0,R0                                                            
         ICM   R0,3,13(R1)         RECORD LENGTH                                
         LLC   R1,ELEM+1           ELEMENT LENGTH                               
         AR    R0,R1                                                            
         CHI   R0,INSMAXLN         UNDER MAX LENGTH                             
         JL    EXIT                YES                                          
*                                                                               
         L     R6,AIO                                                           
         XC    WORK(4),WORK                                                     
         MVC   WORK+4(2),=XL2'FFFF'                                             
                                                                                
* FIND OLDEST ELEM AND DELETE - CHECK DEALER TAG ELEMS FIRST *                  
                                                                                
         MVI   ELCODE,X'20'                                                     
         BRAS  RE,GETEL                                                         
         BNE   LOSEM6                                                           
*                                                                               
LOSEM4   CLC   INSLTD,WORK+4       TEST LAST SUBEL END BEFORE INST ST           
         BNL   *+14                                                             
         MVC   WORK+4(2),INSLTD    SAVE LOWEST DATE                             
         ST    R6,WORK             AND ELEM ADDR                                
         BRAS  RE,NEXTEL                                                        
         BE    LOSEM4                                                           
*                                                                               
LOSEM6   MVI   ELCODE,X'10'                                                     
         L     R6,AIO                                                           
         BRAS  RE,GETEL                                                         
         BNE   LOSEM10                                                          
*                                                                               
LOSEM8   LLC   RE,1(R6)            GET ELEM LEN                                 
         AR    RE,R6               POINT TO END                                 
         AHI   RE,-2               POINT TO LAST LTD                            
         CLC   0(2,RE),WORK+4      TEST LAST SUBEL END BEFORE INST ST           
         BNL   *+14                                                             
         MVC   WORK+4(2),0(RE)     SAVE LOWEST DATE                             
         ST    R6,WORK             AND ELEM ADDR                                
         BRAS  RE,NEXTEL                                                        
         BE    LOSEM8                                                           
*                                                                               
         DROP  R6                                                               
*                                                                               
LOSEM10  L     R6,WORK                                                          
         GOTO1 VRECUP,DMCB,AIO,(R6) DELETE ELEMENT                              
         B     LOSEM2                                                           
         LTORG                                                                  
         EJECT                                                                  
*============================================================                   
* NEED TO PRINT FOOTLINES AT END OF REPORT                                      
*============================================================                   
                                                                                
DOFEET   NTR1  BASE=*,LABEL=*                                                   
         MVI   CONTINUE,C'N'                                                    
*                                                                               
         TM    SVTBIND2,SVTBICGR   THIS CABLE STATION WITH GROUP CODE           
         BO    DOFEETX              NO PRINT                                    
*                                                                               
         MVI   FORCEFUT,C'Y'                                                    
*                                                                               
         MVC   P,SPACES                                                         
         MVI   P,0                 FORCE IT TO PRINT                            
         BRAS  RE,GOSPL              GOTO SPOOL RTN                             
*                                                                               
         CLI   PQSW,1                                                           
         BE    DOFEETX                                                          
         CLI   SVTWPR1,C'N'        NO FAX                                       
         BE    DOFEETX                                                          
         CLI   SVTWPR1,C'C'        COPIES ONLY                                  
         BE    DOFEETX                                                          
         MVC   P1(26),=C'*** END OF DDS MESSAGE ***'                            
         MVI   LINE,2                                                           
         MVI   FORCEFUT,C'S'       SET SKIP FOOTLINES THIS TIME                 
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         TM    WHEN,X'20'          THIS SOON                                    
         BO    DOFEETX                                                          
*                                                                               
* SEE IF RUNNING OUT OF TSAR SPACE                                              
*                                                                               
         L     RF,TSARBYTE                                                      
         AHI   RF,20000                                                         
         C     RF,TSARBUFL                                                      
         BL    DOFEETX             TEMP FORCE PRINT OUT                         
*                                                                               
         GOTO1 VPCOPY                                                           
*                                                                               
DOFEETX  J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
PATREQS  NTR1  BASE=*,LABEL=*                                                   
         CLI   OFFLINE,C'Y'                                                     
         JNE   EXIT                                                             
*                                                                               
         TM    WHEN,X'20'          THIS A SOON REQUEST                          
         JO    EXIT                 YES, NO MULTI STEP                          
*                                                                               
* GENERATE PATTERN RECAP REQUEST RECORDS *                                      
*                                                                               
         MVC   ELEM(80),SPACES                                                  
         LA    R2,ELEM                                                          
         USING XRECD,R2                                                         
*                                                                               
         MVI   XTYPE,C'P'                                                       
         MVC   XWHEN,QUWHEN                                                     
         MVC   XMED,QMED                                                        
         MVC   XCLT,QCLT                                                        
         MVC   XSVPR011,SVPROF11   SAVE T0 PROF11 FOR COPY CODE = EST           
*                                                                               
         L     R7,ASVTABLE                                                      
         USING SVTABLED,R7                                                      
*                                                                               
PATREQ2  TM    SVTBIND,X'01'       TEST MISSING PATTERN                         
         BO    PATREQ16                                                         
*                                                                               
         OC    SVINSDT,SVINSDT     IF ORIGINAL, SHOW ALL PATTERNS               
         BZ    PATREQ6             THESE ARE ORIGINAL                           
         CLI   SVINSREV,0          TEST PREVIOUS INST WERE ORIGINAL             
         BNE   PATREQ4                                                          
         TM    SVOPT,OPTRERUN      TEST THIS IS A RERUN                         
         BO    PATREQ6             YES - TREAT THESE AS ORIGINAL                
PATREQ4  CLI   SVT1PR4,C'S'        OPTION TO SUPPRESS PATTERN RECAPS            
         BE    PATREQ16                                                         
         CLI   SVT1PR4,C'Y'        OPTION TO ONLY SHOW CHANGED PATTERNS         
         BNE   PATREQ6                                                          
         TM    SVTBIND,X'40'       TEST CHANGED PATTERN                         
         BZ    PATREQ16                                                         
*                                                                               
PATREQ6  LA    R1,SVTBPRD                                                       
         BAS   RE,GETXPRD                                                       
         MVC   XPRD,WORK                                                        
*                                                                               
         MVC   WORK,SPACES                                                      
         LA    R1,SVTBPRD2                                                      
         CLI   0(R1),0                                                          
         BE    *+8                                                              
         BAS   RE,GETXPRD                                                       
         MVC   XPRD2,WORK                                                       
*                                                                               
         MVC   XCOPY,SVTBCOPY                                                   
*                                                                               
* GENERATE ONE REQUEST FOR EACH PATTERN LIST ENTRY *                            
*                                                                               
         L     R3,SVTBAPTN                                                      
         USING PTNLISTD,R3                                                      
         CLI   0(R3),0             TEST NO DATA                                 
         BE    PATREQ16                                                         
*                                                                               
PATREQ10 CLI   PTNLTYPE,X'FF'     NO PATTERN WITH TBA                           
         BE    PATREQ14                                                         
         TM    PTNLFLAG,PTNLFHIA  NO PATTERN FOR HIATUS                         
         BO    PATREQ14                                                         
         SR    R0,R0                                                            
         ICM   R0,7,PTNLREF                                                     
         BNZ   *+6                                                              
         DC    H'0'                                                             
         X     R0,=X'00FFFFFF'     COMPLEMENT                                   
         SRL   R0,10               DROP SUBLINE                                 
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  XREF,DUB                                                         
*                                                                               
         CLC   AGYORIG,=C'DFNY'                                                 
         BNE   PATREQ12                                                         
         MVC   XREF,=C'INACT'      FORCE PTTN RECAP OF ALL (INCL INACT)         
*                                                                               
PATREQ12 GOTO1 DATCON,DMCB,(2,SVGENST),XFLTST                                   
         GOTO1 (RF),(R1),(2,SVGENEND),XFLTEND                                   
*                                                                               
         L     R1,ATR04FIL                                                      
         LA    R0,ELEM                                                          
         PUT   (1),(0)                                                          
*                                                                               
PATREQ14 LA    R3,L'PTNLIST(R3)                                                 
         CLI   0(R3),0                                                          
         BNE   PATREQ10                                                         
*                                                                               
PATREQ16 LA    R7,SVTBNEXT                                                      
         OC    SVTBLINE,SVTBLINE                                                
         BNZ   PATREQ2                                                          
         MVI   ERROR,0             RESET ERROR CODE                             
         J     EXIT                                                             
         DROP  R3                                                               
         LTORG                                                                  
         EJECT                                                                  
GETXPRD  L     RF,ASVCLIST                                                      
*                                                                               
GETXPRD2 CLC   3(1,RF),0(R1)       MATCH PRD CODE                               
         BE    GETXPRD4                                                         
         LA    RF,4(RF)                                                         
         CLI   0(RF),C' '                                                       
         BNL   GETXPRD2                                                         
*                                                                               
GETXPRD4 MVC   WORK,SPACES                                                      
         MVC   WORK(3),0(RF)                                                    
         LA    RF,WORK+2           POINT TO END                                 
         CLI   0(RF),C' '          TEST 2 CHAR PRD                              
         BNE   *+6                                                              
         BCTR  RF,0                                                             
         MVI   1(RF),C'-'                                                       
         LLC   R0,1(R1)            GET SLN                                      
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  2(3,RF),DUB                                                      
         BR    RE                                                               
         LTORG                                                                  
         EJECT                                                                  
INIT     NTR1 BASE=*,LABEL=*                                                    
         USING GEND,RC                                                          
*                                                                               
         L     R3,ATWA                                                          
         USING T216FFD,R3                                                       
*                                                                               
         MVC   SVREMUSR,REMUSER                                                 
         MVI   HEADSW,C'X'         SUPPRESS MIDLINES                            
*                                                                               
         TM    SVTBIND2,SVTBICGR   THIS CABLE STATION WITH GROUP CODE           
         BO    INIT060              NO PRINT                                    
*                                                                               
         CLI   SVT1PR9,C'Y'         IF COVER LETTER INSTR, NO PRINT             
         BE    INIT060                                                          
*                                                                               
         CLI   PQSW,1              TEST PRTQUE OPEN                             
         BNE   INIT021              YES                                         
*                                                                               
         LA    R1,ELEM                                                          
         ST    R1,SPOOLQLK                                                      
         XC    ELEM(128),ELEM                                                   
         USING PQPLD,R1                                                         
         MVI   QLEXTRA,X'FF'       NEW PARM LIST                                
         OI    SPOOLIND,X'40'      USER VALUES PRESENT                          
*                                                                               
         MVI   MAXLINES,57         SET PAGE SIZE                                
         MVI   SPACING,1           INITIALIZE SPACING                           
*                                                                               
*      SEE IF FAXING AUTO GEN TO ATT&T                                          
*      IF SO - MUST USE CLASS G AND SEND SPECIAL PRINT LINE FIRST               
*                                                                               
         TM    WHEN,X'40'          IF NOW REPORT SET ARCHIVING                  
         BZ    *+10                FOR AGENCY COPY                              
         MVC   QLTYP1,SVQLTYP1                                                  
         MVC   QLARC,SVQLARC                                                    
         MVI   QLEXTRA,X'FF'                                                    
*                                                                               
         CLI   TRAWRKRH+5,0        TEST OPTICA                                  
         JNE   INIT008             YES                                          
*                                                                               
         CLI   SVTWPR1,C'N'        SEE IF DOING FAX REPORT                      
         BE    INIT020              NO                                          
         CLI   SVTWPR1,C'C'        COPIES ONLY                                  
         BE    INIT020              YES                                         
*                                                                               
         MVC   QLTYP1,SVFAXARC     ELSE IF FAX USE FAX ARCHIVE BYTE             
         MVC   QLARC,SVQLARC                                                    
         MVI   QLEXTRA,X'FF'                                                    
*                                                                               
         MVC   QLDESC(3),QCLT                                                   
         MVC   QLDESC+3(3),QPRD                                                 
         MVC   QLDESC+6(5),SVTBSTA                                              
*                                                                               
INIT008  MVI   PLCLASS,C'G'        WESTERN UNION                                
         MVI   QLCLASS,C'G'        ALSO                                         
*                                                                               
         TM    WHEN,X'20'          IS THIS SOON                                 
         BO    INIT010                                                          
         MVI   SPMODE,X'FF'        CLOSE PRINT QUE                              
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
*  FAXING LETTERS - MUST USE CLASS G AND SEND SPECIAL PRINT LINE FIRST          
*                                                                               
INIT010  CLI   OFFLINE,C'Y'                                                     
         BNE   INIT012                                                          
*                                                                               
         L     R1,ATWA                                                          
         ICM   RE,15,TWAMASTC-T216FFD(R1)                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         L     RF,MCVREMOT-MASTD(,RE)                                           
         USING REMOTED,RF                                                       
         MVC   REMOTSYS,MCS2OSYS-MASTD(RE)                                      
         MVI   REMOTCPY,1                                                       
         MVI   REMOTCLS,C'G'                                                    
         MVC   REMOTJID,=C'AWX'                                                 
         MVC   REMOTDST,TWAORIG-T216FFD(R1)                                     
*                                                                               
         CLI   TRAWRKRH+5,0        TEST OPTICA                                  
         BNE   *+16                YES - NO ARCHIVING                           
         MVC   REMOTTY1,SVFAXARC                                                
         MVC   REMOTARC,SVQLARC                                                 
*                                                                               
         OC    TWADEST-T216FFD(,R1),TWADEST-T216FFD(R1)                         
         BZ    *+10                                                             
         MVC   REMOTDST,TWADEST-T216FFD(R1)                                     
         DROP  RF                                                               
*                                                                               
INIT012  MVC   REMUSER,=C'AWX'                                                  
         MVC   QLSUBID,=C'AWX'                                                  
         CLI   TRAWRKRH+5,0        TEST OPTICA                                  
         BNE   *+16                YES - NO ARCHIVING                           
         MVC   QLTYP1,SVFAXARC                                                  
         MVC   QLARC,SVQLARC                                                    
                                                                                
* TSAR CLEAR HERE                                                               
                                                                                
INIT012X LA    RE,TSARWK                                                        
         LR    R2,RE                                                            
         LA    RF,TSARDL2                                                       
         XCEF                                                                   
         USING TSARD,R2                                                         
         MVC   TSACOM,ACOMFACS                                                  
         MVI   TSKEYL,3                                                         
*                                                                               
         OI    TSINDS,TSINODSK                                                  
         OI    TSIND2,TSI2MANY+TSI2BIGN   USE BOTH BUFFERS                      
*                                                                               
         MVI   TSRECL+1,139                                                     
         OI    TSRECI,TSRVAR                                                    
*                                                                               
         CLI   OFFLINE,C'Y'                                                     
         BNE   INIT014                                                          
*                                                                               
         L     R0,=F'8000000'                                                   
         ST    R0,TSARBUFL                                                      
         GETMAIN RU,LV=(0),LOC=(ANY,ANY)                                        
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         ST    R1,TSARBUFF                                                      
*                                                                               
         USING TSARD,R2                                                         
         MVC   TSABUF,TSARBUFF                                                  
         MVC   TSAREC,TSARBUFL                                                  
*                                                                               
* TSAR CALLOV HERE                                                              
*                                                                               
         GOTO1 CALLOV,DMCB,0,(C'R',X'00000A7D')                                 
         ICM   RF,15,0(R1)                                                      
         BNZ   *+6                                                              
         DC    H'0'                                                             
         ST    RF,ATSAR                                                         
         MVI   TSOFFACT,TSAINI     SET 1ST TSAR FOR INIT                        
         B     INIT016                                                          
*                                                                               
INIT014  MVI   TSPAGN,20            NUMBER OF TEMPSTR PAGES TO BE USED          
*                                                                               
* TSAR CALLOV HERE                                                              
*                                                                               
         GOTO1 CALLOV,DMCB,0,(C'R',X'00000A5D')                                 
         ICM   RF,15,0(R1)                                                      
         BNZ   *+6                                                              
         DC    H'0'                                                             
         ST    RF,ATSAR                                                         
         MVI   TSACTN,TSAINI         SET 1ST TSAR FOR INIT                      
         MVC   TSARBUFL,=F'180000'                                              
*                                                                               
INIT016  GOTO1 ATSAR,TSARD                                                      
         CLI   TSERRS,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
INIT020  GOTO1 OPENPQ                                                           
         MVC   REMUSER,SVREMUSR    RESTORE REMUSER                              
         EJECT                                                                  
*  FAX ENTRIES GET A PRINT LINE WITH SOME FIXED INFO, AND                       
*   A STATION ID                                                                
*                                                                               
INIT021  CLI   SVTWPR1,C'N'       SEE IF DOING FAX REPORT                       
         BE    INIT060              NO                                          
         CLI   SVTWPR1,C'C'        COPIES ONLY                                  
         BE    INIT060              YES                                         
*                                                                               
INIT021A MVI   FORCEHED,C'N'       PREVENT HEADLINES                            
         MVI   LINE,2                                                           
*                                                                               
         MVC   EDIORIG,AGYORIG                                                  
         MVC   EDIHDR,=CL5'*HDR*'                                               
*                                                                               
         MVI   EDIWIDE,C'L'        SET FOR LANDSCAPE                            
         MVI   MAXLINES,FAXMAX     AND CORRECT PAGESIZE                         
         MVI   EDIPAGE,C'P'                                                     
*                                                                               
INIT021B MVC   EDIDESID(5),SVTBSTA                                              
*                                                                               
         CLI   SVTBSTA,C'0'           IS THIS CABLE                             
         BL    *+16                    NO                                       
         MVC   EDIDESID(1),SVTBSTA+4  SEND MEDIA FIRST                          
         MVC   EDIDESID+1(4),SVTBSTA  THEN 4 DIGITS                             
*                                                                               
         MVC   EDIBILL(4),QMED    MEDIA & CLIENT                                
         MVC   EDIBILL+4(3),QPRD    PRODUCT                                     
*                                                                               
         MVC   EDIFDEST(4),SVTBSTA                                              
         LA    R1,EDIFDEST+4                                                    
         CLI   SVTBSTA+3,C' '                                                   
         BH    *+6                                                              
         BCTR  R1,0                                                             
         MVI   0(R1),C'-'                                                       
         MVC   1(1,R1),SVTBSTA+4                                                
*MNMB                                                                           
         CLI   SVTBSTA+4,C'D'                                                   
         BE    INIT021C                                                         
*MNMB                                                                           
         CLI   SVTBSTA+4,C'T'                                                   
         BNE   INIT022                                                          
INIT021C MVI   2(R1),C'V'                                                       
         B     INIT026                                                          
*                                                                               
INIT022  CLI   SVTBSTA+4,C'S'                                                   
         BE    INIT023                                                          
         CLI   SVTBSTA+4,C'C'      IHEART RADIO CM                              
         BE    INIT023                                                          
         CLI   SVTBSTA+4,C'A'                                                   
         BE    *+12                                                             
         CLI   SVTBSTA+4,C'F'                                                   
         BNE   *+8                                                              
INIT023  MVI   2(R1),C'M'                                                       
*                                                                               
INIT026  MVI   ALLOWLIN,0                                                       
         MVI   SPACING,1                                                        
         GOTO1 SPOOL,DMCB,(R8)    SEND SPECIAL PRINT LINE                       
*                                                                               
         MVC   EDIDDSID(14),=C'++DDS SPURATRN'                                  
         MVC   EDISTTMD(4),QMED & CLT                                           
         MVC   EDISTTPR,QPRD                                                    
         MVC   EDISTTP2,QPRD2                                                   
         MVC   EDISTTES,=C'NO '                                                 
         CLI   QBEST,0                                                          
         BE    INIT028                                                          
         LLC   R0,QBEST                                                         
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  EDISTTES,DUB                                                     
*                                                                               
INIT028  GOTO1 DATCON,DMCB,(0,SVQSTART),(X'20',WORK)                            
         GOTO1 DATCON,DMCB,(0,SVQEND),(X'20',WORK+6)                            
         MVC   EDISTTDT,WORK       START AND END DATES                          
*                                                                               
*NOP     MVC   EDISTTDT,SVQSTART                                                
*        CLI   EDISTTDT,X'FA'                                                   
*        BNE   *+8                                                              
*        MVI   EDISTTDT,C'0'                                                    
*        CLI   EDISTTDT+6,X'FA'                                                 
*        BNE   *+8                                                              
*******  MVI   EDISTTDT+6,C'0'                                                  
*                                                                               
         MVC   EDISTTST,SVTBSTA                                                 
         MVC   EDISTTCT,QUESTOR                                                 
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)    SEND SPECIAL PRINT LINE                       
*                                                                               
* SET UP PRINTING ADDRESSABILITY (AFTER PRTQUE OPEN) *                          
*                                                                               
INIT060  LAY   R0,HDHK                                                          
         ST    R0,HEADHOOK                                                      
         LAY   R0,MIDHK                                                         
         ST    R0,MIDHOOK                                                       
         LAY   R0,FTHK                                                          
         ST    R0,FOOTHOOK                                                      
         LAY   R0,HDSPECS                                                       
         ST    R0,SPECS                                                         
*                                                                               
         MVI   FOOTLNS,5           ALLOW 5 LINES OF FOOTLINES                   
         MVI   FOOTSW,0            RESET SWITCH                                 
         NI    SVOPT,X'FF'-PTCMLTXT SET OFF PRINT COMML TEXT SW                 
*                                                                               
         LA    R0,14               BLANK THE HEADLINES                          
         LA    R1,H1                                                            
         MVC   0(132,R1),SPACES                                                 
         LA    R1,132(R1)                                                       
         BCT   R0,*-10                                                          
*                                                                               
         MVC   WORK(6),SVGENDTS                                                 
         CLI   SVT1PR5,C'Y'        USE FTD AND LTD, NOT FLIGHT                  
         BNE   INIT066                                                          
         MVI   WORK,X'FF'          SET HIGH START DATE                          
         MVI   WORK+2,0            AND LOW END DATE                             
         LR    RE,R7                                                            
*                                                                               
INIT064  CLC   WORK(2),SVTBSTR-SVTBDATA(RE)                                     
         BNH   *+10                                                             
         MVC   WORK(2),SVTBSTR-SVTBDATA(RE)                                     
         CLC   WORK+2(2),SVTBEND-SVTBDATA(RE)                                   
         BNL   *+10                                                             
         MVC   WORK+2(2),SVTBEND-SVTBDATA(RE)                                   
         LA    RE,L'SVTBDATA(,RE)                                               
         CLC   SVTBSTA,SVTBSTA-SVTBDATA(RE)                                     
         BE    INIT064                                                          
*                                                                               
INIT066  GOTO1 DATCON,DMCB,(2,WORK),USERQSTR                                    
         GOTO1 (RF),(R1),(2,WORK+2),USERQEND                                    
*                                                                               
         MVI   LINE,99             FORCE NEW PAGE WITHOUT FORCEHED              
         MVC   PAGE,=H'1'          WHICH CAUSES FOOTLINES TO REPRINT            
*                                                                               
         XC    STANET,STANET                                                    
         TM    SVTBIND2,SVTBICAB   IS THIS A CABLE STATION                      
         BZ    INIT068                                                          
*                                                                               
         XC    WORK,WORK                                                        
         GOTO1 MSUNPK,DMCB,(X'80',SVTBMKST),WORK,WORK+4                         
*                                                                               
         CLC   WORK+9(3),SPACES                                                 
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   STANET(4),WORK+4                                                 
         MVI   STANET+4,C'/'                                                    
         MVC   STANET+5(3),WORK+9                                               
*                                                                               
INIT068  CLI   OFFLINE,C'Y'                                                     
         BE    INIT070                                                          
                                                                                
* SET UP STORAGE *                                                              
                                                                                
         L     RE,NEXTADDR         POINT TO SHIPPING LIST                       
         MVC   0(8,RE),=C'SHIPLIST'                                             
         LA    RE,8(RE)                                                         
         ST    RE,ASHPLIST                                                      
         LA    RE,3200(RE)         ALLOW ROOM FOR 50 FILMS + X'00'              
         ST    RE,ASHPLSTX                                                      
*                                                                               
INIT070  L     RE,ASHPLIST                                                      
         MVI   0(RE),0             SET E-O-L FLAG FOR SHIPLIST                  
*                                                                               
         TM    SVTBIND2,SVTBICGR   THIS CABLE STATION WITH GROUP CODE           
         BO    INITX               YES - DON'T PRINT                            
         EJECT                                                                  
* ================================================================              
* READ HEADLINE TEXT RECORD                                                     
* ================================================================              
                                                                                
         L     RE,AIO2                                                          
         XC    0(24,RE),0(RE)      PRECLEAR                                     
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0A23'                                                  
         MVC   KEY+2(3),BAGYMD     A-M/CLT                                      
         CLI   BPRD,X'FF'          TEST POL REQUEST                             
         BE    INIT076             IF SO READ CLIENT TEXT                       
*                                                                               
         L     R1,ASVCLIST                                                      
*                                                                               
INIT072  CLC   BPRD,3(R1)                                                       
         BE    INIT074                                                          
         LA    R1,4(R1)                                                         
         CLI   0(R1),C' '                                                       
         BH    INIT072                                                          
         DC    H'0'                                                             
*                                                                               
INIT074  MVC   KEY+5(3),0(R1)                                                   
*                                                                               
INIT076  MVI   KEY+8,C'H'                                                       
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE     TEST PRD TEXT FOUND                          
         BE    INIT080                                                          
         MVC   KEY,KEYSAVE         RESTORE KEY                                  
         XC    KEY+5(3),KEY+5      CLEAR PRODUCT                                
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    INIT080                                                          
         MVC   KEY,KEYSAVE         RESTORE KEY                                  
         XC    KEY+3(2),KEY+3      CLEAR CLIENT                                 
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   INIT106                                                          
*                                                                               
INIT080  L     R6,AIO3                                                          
         ST    R6,AIO                                                           
         GOTO1 GETREC                                                           
*                                                                               
         MVI   HALF,0                                                           
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         BNE   *+10                                                             
         MVC   HALF,2(R6)          SAVE IND BYTE                                
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'40'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   =C'NONE ',3(R6)     TEST SUPPRESS COMMENT                        
         BE    INIT106                                                          
         TM    HALF,X'80'          TEST TO BOX COMMENT                          
         BZ    INIT086             NO                                           
         GOTO1 VBOXER,DMCB,(60,AIO2) FORMAT COMMENT                             
         B     INIT090                                                          
         EJECT                                                                  
*======================================================                         
* FORMAT UNBOXED COMMENT                                                        
*======================================================                         
                                                                                
INIT086  L     R4,AIO2                                                          
         L     R6,AIO                                                           
         BRAS  RE,GETEL                                                         
*                                                                               
INIT088  MVC   0(60,R4),SPACES                                                  
         LLC   RE,1(R6)                                                         
         AHI   RE,-4                                                            
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),3(R6) *EXECUTED*                                         
         LA    R4,60(R4)                                                        
         BRAS  RE,NEXTEL                                                        
         BE    INIT088                                                          
         MVI   0(R4),0             SET EOL FLAG                                 
                                                                                
*=================================================================              
* FORMAT STANDARD TEXT TO PRINT LINES                                           
*=================================================================              
                                                                                
INIT090  BRAS  RE,GOSPL            PRINT HEADLINES                              
*                                                                               
         L     R4,AIO2             FORMATTED COMMENTS ARE HERE                  
         MVI   SPACING,1                                                        
*                                                                               
INIT100  LA    R0,4                                                             
         LA    R1,P1                                                            
*                                                                               
INIT102  CLI   0(R4),0             TEST REACHED END OF COMMENTS                 
         BE    INIT104                                                          
         MVC   0(60,R1),0(R4)      MOVE COMMENT TO PRINT LINE                   
*                                                                               
         LA    R1,132(R1)                                                       
         LA    R4,60(R4)                                                        
         BCT   R0,INIT102                                                       
*                                                                               
INIT104  BRAS  RE,GOSPL                                                         
*                                                                               
         CLI   0(R4),0                                                          
         BNE   INIT100                                                          
         EJECT                                                                  
*=================================================================              
* FORMAT STEXT                                                                  
*=================================================================              
                                                                                
INIT106  TM    SVOPT2,OP2SPCMT     TEST SPECIAL COMMENTS                        
         BZ    INIT108                                                          
         CLI   SVPROF16,C'N'       NULL (NO SPECIAL TEXT)                       
         BE    INITX                                                            
         B     INIT110                                                          
*                                                                               
INIT108  CLI   SVPROF12,C'*'       TEST SPECIAL MKT/STA COMMENTS                
         BNE   INITX                                                            
                                                                                
* FIND AND PRINT SPECIAL MARKET, STATION TEXT OR STATION TYPE *                 
                                                                                
INIT110  XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0A2D'                                                  
         MVC   KEY+2(3),BAGYMD     A-M/CLT                                      
         MVC   KEY+5(1),SVPROF16                                                
         TM    SVOPT2,OP2SPCMT     TEST SPECIAL COMMENTS                        
         BO    *+10                                                             
         MVC   KEY+5(1),SVPROF12                                                
         CLI   SVPROF11,C'E'       COPY CODE = EST                              
         BNE   INIT112                                                          
         LLC   R0,SVTBCOPY                                                      
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         MVC   KEY+6(3),=C'ES='                                                 
         UNPK  KEY+9(3),DUB                                                     
         GOTO1 HIGH                                                             
         CLC   KEY(12),KEYSAVE     TEST STATION TEXT FOUND                      
         BE    INIT130                                                          
         MVC   KEY(13),KEYSAVE     RESTORE KEY                                  
         MVI   KEY+11,0                                                         
*                                                                               
INIT112  MVC   KEY+6(5),SVTBSTA                                                 
         CLI   SVTBSTA,C'0'        IF NUMERIC                                   
         BL    INIT120                                                          
         CLI   SVTBSTA,C'9'                                                     
         BH    INIT120                                                          
         MVI   KEY+10,C'/'         THEN CHK FOR LOCAL STATION                   
         B     INIT122                                                          
*                                                                               
INIT120  CLI   QMED,C'T'                                                        
         BNE   INIT122                                                          
         MVI   KEY+10,0                                                         
         CLI   KEY+9,C' '                                                       
         BNE   *+8                                                              
         MVI   KEY+9,0                                                          
*                                                                               
INIT122  GOTO1 HIGH                                                             
         CLC   KEY(10),KEYSAVE     TEST STATION TEXT FOUND                      
         BE    INIT130                                                          
*                                                                               
         MVC   KEY(13),KEYSAVE     RESTORE KEY                                  
         MVC   KEY+6(5),=C'TYPE='                                               
         MVC   KEY+11(1),SVTBTYPE   STATION TYPE                                
         MVI   KEY+12,0                                                         
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE     TEST STATION TYPE TEXT FOUND                 
         BE    INIT130                                                          
*                                                                               
         MVC   KEY(13),KEYSAVE     RESTORE KEY                                  
         SR    R0,R0                                                            
         ICM   R0,3,SVTBMKST                                                    
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  KEY+6(4),DUB                                                     
         XC    KEY+10(3),KEY+10                                                 
         GOTO1 HIGH                                                             
         CLC   KEY(11),KEYSAVE     TEST MARKET TEXT FOUND                       
         BE    INIT130                                                          
*                                                                               
         MVC   KEY(13),KEYSAVE    RESTORE KEY                                   
         XC    KEY+6(4),KEY+6     CK FOR ALL MKTS/STA                           
         GOTO1 HIGH                                                             
         CLC   KEY(10),KEYSAVE    TEST ALL MKT/STA STEXT FOUND                  
         BNE   INITX                                                            
*                                                                               
INIT130  MVI   P1,0                FORCE A BLANK LINE                           
         LA    R4,P2               FIRST PRINT LINE                             
         LA    R5,3                                                             
*                                                                               
         L     R6,AIO3                                                          
         ST    R6,AIO                                                           
         GOTO1 GETREC                                                           
*                                                                               
         MVI   ELCODE,X'40'                                                     
         BRAS  RE,GETEL                                                         
         B     *+8                                                              
*                                                                               
INIT140  BRAS  RE,NEXTEL                                                        
         BNE   INIT145                                                          
*                                                                               
INIT142  SR    RE,RE                                                            
         IC    RE,1(R6)                                                         
         AHI   RE,-4                                                            
*                                                                               
INIT144  EX    RE,MOVETEXT                                                      
         CLC   0(132,R4),SPACES                                                 
         BNE   *+8                                                              
         MVI   0(R4),0             FORCE TO PRINT                               
         LA    R4,132(R4)          NEXT PRINT LINE                              
         BCT   R5,INIT140                                                       
*                                                                               
INIT145  BRAS  RE,GOSPL            PRINT THE LINES                              
         MVI   SPACING,1           RESET                                        
*                                                                               
         CLI   0(R6),0             TEST REACHED END                             
         BE    INIT150             YES                                          
*                                                                               
         LA    R4,P1               IF NOT, PRINT 4 MORE LINES                   
         LA    R5,4                                                             
         B     INIT140                                                          
*                                                                               
INIT150  LLC   R1,KEY+12           BUMP TYP (PAGE NUMBER)                       
         LA    R1,1(,R1)                                                        
         STC   R1,KEY+12                                                        
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    INIT130                                                          
*                                                                               
INITX    LLC   RE,MAXLINES         FORCE A BLANK LINE                           
         LLC   R0,LINE                                                          
         SR    RE,R0                                                            
         CHI   RE,10               IF DON'T HAVE 10 LINES                       
         BNL   INITX2                                                           
         MVI   FORCEHED,C'Y'       START ON A NEW PAGE                          
*                                                                               
INITX2   MVI   HEADSW,C'Y'         SET TO PRINT MIDLINES                        
         MVI   FORCEMID,C'Y'                                                    
         XIT1                                                                   
*                                                                               
MOVETEXT MVC   0(0,R4),3(R6)                                                    
         DROP  R3                                                               
         LTORG                                                                  
         EJECT                                                                  
*====================================================================           
** SUBROUTINE TO UPDATE TBA RECORDS **                                          
*                                                                               
* IF EST = NO (QBEST=0) ALL ESTIMATES WILL  BE UPDATED, OTHERWISE               
* ESTIMATES FROM QBEST TO QBESTEND WILL BE UPDATED                              
*                                                                               
*  REGS - 2 BLOCK+1 FOR EQUIVALENT PRODUCTS                                     
*         3 LAST OF EQUAL PRD/PTR SERIES                                        
*         4 BLOCK+18 FOR EQUIVALENT PARTNERS                                    
*         5 1ST PRD/PTR OF EQUAL SERIES                                         
*         7 CURR SVTABLE POINTER                                                
*====================================================================           
                                                                                
TBAUP    NTR1 BASE=*,LABEL=*                                                    
*                                                                               
         L     R7,ASVTABLE                                                      
         USING SVTABLED,R7                                                      
*                                                                               
         MVC   AIO,AIO1            SET TO USE IO1                               
         NI    UPDSW,X'FF'-UPDTBA  SET TO PROCESS VALID ENTRIES                 
         LR    R5,R7               SAVE 1ST PRD/PTR OF EQ SERIES                
         LR    R3,R7               SAVE LAST OF EQ PRD/PTR SERIES               
*                                                                               
         XC    BLOCK(256),BLOCK                                                 
*                                                                               
*                                                                               
TBAUP10  CLI   SVPROF15,C'Y'       TBUYS                                        
         BE    TBAUP12             YES                                          
                                                                                
         TM    SVTBIND,X'01'       TEST MISSING PATTERN                         
         BO    TBAUP90              IGNORE IF MISSING                           
         CLI   SVPROF13,C'Y'       EQUIV PROD CLT                               
         BNE   TBAUP12                                                          
         MVC   BYTE,SVTBPRD                                                     
         LA    R4,BLOCK                                                         
         BAS   RE,GEP                                                           
         LA    R2,BLOCK+1                                                       
         CLI   SVTBPRD2,0          PTR PROD                                     
         BE    TBAUP12              NO                                          
         MVC   BYTE,SVTBPRD2                                                    
         LA    R4,BLOCK+17                                                      
         BAS   RE,GEP                                                           
         LA    R4,BLOCK+18                                                      
*                                                                               
TBAUP12  XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0A2E'                                                  
         MVC   KEY+2(3),BAGYMD     A-M/CLT                                      
         MVC   KEY+5(5),SVTBMKST   MKT/STA                                      
         MVC   BYTE,BEST           SAVE EST                                     
*                                                                               
TBAUP16  DS   0H                                                                
         MVC   KEY+10(1),SVTBPRD   PRD                                          
*                                                                               
         MVC   KEY+11(1),BYTE      EST                                          
*                                                                               
         MVC   KEY+12(1),SVTBPRD2  PTNR                                         
*                                                                               
TBAUP20  GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(13),KEYSAVE                                                  
         BE    TBAUP22                                                          
*                                                                               
         CLI   SVPROF15,C'Y'       USE TRAFFIC                                  
         BE    TBAUP24             YES, GO ADD REC                              
*                                                                               
         CLI   SVT2PR05,C'Y'       COMBINE CABLE NETWORKS                       
         BNE   TBAUP24              NO, GO ADD REC                              
*                        X'E8' NOW                                              
         CLI   KEYSAVE+7,CABLESTA  THIS A CABLE STATION                         
         BL    TBAUP24              NO, GO ADD REC                              
*                                                                               
TBAUP21  DS    0H                                                               
         CLC   KEY(9),KEYSAVE      THIS SAME CABLE STATION?                     
         BNE   TBAUP58                                                          
*                                                                               
* CABLE NET HAS BEEN USING 1 BYTE, NOT 7 BITS FOR A LONG TIME(MEL)              
* NO, NOT SO, ONLY 7 BITS AFTER ALL                                             
*                                                                               
         MVC   HALF(1),KEY+9                                                    
         NI    HALF,X'80'                                                       
*                                                                               
         CLC   HALF(1),SVTBMKST+4     THIS SAME CABLE STATION                   
         BNE   TBAUP58                                                          
*                                                                               
         MVC   KEYSAVE+9(1),KEY+9  SAVE CABLE NET                               
*                                                                               
         CLC   KEY+10(3),KEYSAVE+10   CHECK PRD/EST/PRD2 ALSO                   
         BL    TBAUP16                                                          
         BE    TBAUP22                                                          
*                                                                               
         GOTO1 SEQ                                                              
         B     TBAUP21                                                          
*                                                                               
TBAUP22  GOTO1 GETREC                                                           
         L     RF,AIO                                                           
         SR    RE,RE                                                            
         ICM   RE,3,13(RF)                                                      
         AR    RF,RE                                                            
         BCTR  RF,0                                                             
         MVI   0(RF),0                                                          
         NI    UPDSW,X'FF'-UPDTBA   *** RESET ACTIVITY SWITCH ***               
         B     TBAUP26                                                          
*                                                                               
* NOT FOUND - CREATE NEW RECORD *                                               
*                                                                               
TBAUP24  DS    0H                                                               
         CLC   SVTBEND,BTODAY      IF BEFORE TODAY, IGNORE                      
         BL    TBAUP60                                                          
*                                                                               
         CLI   BEST,1              IF ALL ESTIMATES, DON'T ADD TBAE             
         BNE   TBAUP25              NO                                          
         CLI   BESTEND,255                                                      
         BNE   TBAUP25                                                          
         CLI   SVESTAB,X'FF'                                                    
         BNE   TBAUP25                                                          
         CLC   SVESTAB+1(255),SVESTAB                                           
         BNE   TBAUP25                                                          
         CLC   KEY(11),KEYSAVE     EVERYTHING UP TO ESTIMATE EQ                 
         BNE   TBAUP60                                                          
         CLC   KEY+12(1),SVTBPRD2  PTNR                                         
         BNE   TBAUP60                                                          
         MVC   KEYSAVE(13),KEY     SET EQUAL AND GETREC                         
         B     TBAUP22                                                          
*                                                                               
TBAUP25  L     R6,AIO                                                           
         XC    0(256,R6),0(R6)                                                  
         MVC   0(13,R6),KEYSAVE                                                 
         MVI   14(R6),24                                                        
         MVC   20(2,R6),AGENCY                                                  
*                                                                               
         OI    UPDSW,UPDTBA        ***INDICATE RECORD UPDATED***                
*                                                                               
TBAUP26  L     R8,SVTBAPTN                                                      
         USING PTNLIST,R8                                                       
*                                                                               
         BAS   RE,ADJDT            GO ADJUST ANY TBA DATES                      
*                                                                               
         CLI   PTNLTYPE,0          END OF LIST                                  
         BE    TBAUP53                                                          
         EJECT                                                                  
* NOW UPDATE ANY ELEMENTS WITHIN THESE DATES *                                  
*                                                                               
TBAUP30  DS    0H                                                               
         GOTO1 DATCON,DMCB,(5,0),(2,HALF)   GET TODAY'S DATES                   
         GOTO1 DATCON,DMCB,(2,PTNLSTR),(0,WORK)                                 
         GOTO1 (RF),(R1),(2,PTNLEND),(2,FULL+2)                                 
         GOTO1 GETDAY,(R1),(0,WORK),WORK+6                                      
         CLC   WORK+6(3),=CL3' '                                                
         BNE   *+6                                                              
         DC    H'0'                                                             
         LLC   R0,DMCB             GET DAY NUMBER                               
         BCTR  R0,0                                                             
         LTR   R0,R0               IF MONDAY(ZERO), BYPASS ADJUST               
         BZ    TBAUP36                                                          
         LNR   R0,R0                                                            
         B     TBAUP34                                                          
*                                                                               
TBAUP32  LA    R0,7                                                             
*                                                                               
TBAUP34  GOTO1 ADDAY,DMCB,WORK,WORK,(R0)                                        
*                                                                               
TBAUP36  GOTO1 DATCON,(R1),(0,WORK),(2,FULL)                                    
         CLC   FULL(2),FULL+2      AFTER PTNLEND                                
         BH    TBAUP50                                                          
*                                                                               
         USING TBADTAEL,R6                                                      
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'05'                                                     
         BRAS  RE,GETEL                                                         
         B     *+8                                                              
*                                                                               
TBAUP40  BRAS  RE,NEXTEL                                                        
         BE    TBAUP42                                                          
*                                                                               
         XC    ELEM,ELEM                                                        
         LA    R6,ELEM                                                          
         MVI   ELEM,05                                                          
         MVI   ELEM+1,09                                                        
         MVC   TBADTAWK,FULL       WEEK                                         
         MVC   TBADTASL,SVTBSLN                                                 
         MVC   TBADTAS2,SVTBSLN2                                                
         CLI   PTNLTYPE,X'FF'      TBA PATTERN                                  
         BNE   *+8                                                              
         OI    TBADTAAC,X'80'+X'40' *** SET ACTIVITY + TRAFFIC FLAG ***         
*                                                                               
         OI    UPDSW,UPDTBA        ***INDICATE RECORD UPDATED***                
         CLC   TBADTAWK,HALF       DON'T CREATE NEW ELS FOR HISTORY!            
         BL    TBAUP32                                                          
         GOTO1 ADDELEM                                                          
         B     TBAUP32                                                          
*                                                                               
TBAUP42  CLC   TBADTASL,SVTBSLN    SAME SLN                                     
         BNE   TBAUP40                                                          
         CLC   TBADTAS2,SVTBSLN2   SAME PRTNR LEN                               
         BNE   TBAUP40                                                          
         CLC   TBADTAWK,FULL       CK IF THIS WEEK                              
         BNE   TBAUP40                                                          
*                                                                               
         CLI   PTNLTYPE,X'FF'      TBA PATTERN                                  
         BNE   TBAUP44                                                          
         TM    TBADTAAC,X'80'      IS ACTIVITY FLAG SET                         
         BO    TBAUP32                                                          
         OI    TBADTAAC,X'80'+X'40' *** SET ACTIVITY + TRAFFIC FLAG ***         
         B     TBAUP46                                                          
*                                                                               
TBAUP44  TM    TBADTAAC,X'80'+X'40' IS EITHER ACTIVITY FLAG SET                 
         BZ    TBAUP32                                                          
         NI    TBADTAAC,X'3F'      *** RESET ACTIVITY FLAGS ***                 
*                                                                               
TBAUP46  OI    UPDSW,UPDTBA        ***INDICATE RECORD UPDATED***                
*                                                                               
         B     TBAUP32                                                          
*                                                                               
TBAUP50  LA    R8,L'PTNLIST(,R8)                                                
*                                                                               
TBAUP52  CLI   PTNLTYPE,0                                                       
         BNE   TBAUP30                                                          
*                                                                               
TBAUP53  CLC   SVTBEND,SVGENEND    DO BUYS RUN TO END OF PERIOD                 
         BNL   TBAUP54                                                          
*                                                                               
         BAS   RE,TBACK            GO CHECK IF ANY ELEMS TO DELETE              
*                                                                               
TBAUP54  OC    SVTBNXLN,SVTBNXLN   TEST MORE DATA                               
         BZ    TBAUP56             NO                                           
         CLC   SVTBPRD,SVTBNXPR    NEXT ENTRY SAME PRD                          
         BNE   TBAUP56                                                          
         CLC   SVTBPRD2,SVTBNXP2   NEXT ENTRY SAME PARTNER                      
         BNE   TBAUP56                                                          
         LA    R7,SVTBNEXT                                                      
         LR    R3,R7                                                            
         L     R8,SVTBAPTN                                                      
         TM    SVTBIND,X'01'       TEST MISSING PATTERN                         
         BO    TBAUP54              IGNORE IF MISSING                           
         B     TBAUP52                                                          
*                                                                               
TBAUP56  L     RF,PUTREC                                                        
         SR    R0,R0                                                            
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+10                                                             
         L     RF,ADDREC                                                        
         BCTR  R0,0                                                             
*                                                                               
         TM    UPDSW,UPDTBA        TEST RECORD UPDATED                          
         BZ    TBAUP56A             NO                                          
*                                                                               
         TM    SVOPT3,OPT3REPT     THIS A REPRINT                               
         BO    TBAUP56A                                                         
         TM    SVOPT,OPTTEST       THIS A TEST RUN                              
         BO    TBAUP56A                                                         
         BASR  RE,RF                                                            
*                                                                               
TBAUP56A C     RF,ADDREC                                                        
         BNE   *+14                                                             
         L     RF,AIO                                                           
         MVC   KEY(13),0(RF)                                                    
*                                                                               
         CLI   SVT2PR05,C'Y'       COMBINE CABLE NETWORKS                       
         BNE   TBAUP57              NO                                          
*                        X'E8' NOW                                              
         CLI   KEY+7,CABLESTA      THIS A CABLE STATION                         
         BL    TBAUP57              NO                                          
         TM    SVOPT3,OPT3REPT     THIS A REPRINT                               
         BO    TBAUP57              YES                                         
         TM    SVOPT,OPTTEST       THIS A TEST RUN                              
         BO    TBAUP57              YES                                         
*                                                                               
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(13),KEYSAVE                                                  
         BE    TBAUP56C                                                         
         TM    WHEN,X'20'          THIS A SOON REQUEST                          
         BZ    *+10                 NO                                          
         LTR   R0,R0               IS THIS AN ADD                               
         BNZ   TBAUP56E                                                         
*                                                                               
         DC    H'0'                                                             
TBAUP56C DS   0H                                                                
         GOTO1 SEQ                                                              
         B     TBAUP21             TEMP ?????                                   
*                                                                               
TBAUP56E DS   0H                                                                
         CLC   KEY(9),KEYSAVE      STILL SAME STATION                           
         BNE   TBAUP57              NO                                          
*                                                                               
* CABLE NET USES 7 BITS                                                         
*                                                                               
         MVC   HALF(1),KEY+9                                                    
*                                                                               
         NI    HALF,X'80'          NOW USES 7 BITS                              
*                                                                               
         CLC   HALF(1),SVTBMKST+4     CK REST OF STATION                        
         BNE   TBAUP57                                                          
         B     TBAUP21             TEMP ?????                                   
*                                                                               
TBAUP56G DS   0H                                                                
         CLC   KEY(9),KEYSAVE                                                   
         BNE   TBAUP57                                                          
*                                                                               
         MVC   KEYSAVE+9(1),KEY+9                                               
*                                                                               
         CLC   KEY+10(3),KEYSAVE+10      PRD/EST/PTR                            
         BL    TBAUP16                    GO TRY FOR THIS PRD/EST/PTR           
*                                                                               
         BE    TBAUP22                    UPDATE REC                            
*                                                                               
TBAUP57  CLI   QBEST,0             EST = NO                                     
         BE    TBAUP58              YES                                         
*                                                                               
         CLC   QBEST,QBESTEND      ONLY 1 EST                                   
         BE    TBAUP60              YES                                         
*                                                                               
TBAUP58  DS   0H                                                                
         MVC   KEY,KEYSAVE                                                      
         CLC   BESTEND,KEY+TBAKEST-TBAKEY     THIS THE LAST EST                 
         BNH   TBAUP60                          YES                             
*                                                                               
         LLC   RE,KEY+TBAKEST-TBAKEY  GET CURRENT ESTIMATE                      
         LA    R1,SVESTAB(RE)                                                   
*                                                                               
         LA    R1,1(,R1)                                                        
         LA    R0,256                                                           
         SR    R0,RE               SUBT STARTING PT IN TABLE FROM MAX           
*                                                                               
         CLI   0(R1),0             NEXT ESTIMATE                                
         BNE   *+14                 YES                                         
         LA    R1,1(,R1)                                                        
         BCT   R0,*-12                                                          
         DC    H'0'                                                             
*                                                                               
         LA    R0,SVESTAB                                                       
         SR    R1,R0                                                            
         STC   R1,KEY+TBAKEST-TBAKEY                                            
         STC   R1,BYTE                                                          
*                                                                               
         NI    UPDSW,X'FF'-UPDTBA  *** RESET ACTIVITY SWITCH ***                
*                                                                               
         LR    R7,R5               START - THIS PROD/PTNR PAIR                  
*                                                                               
         CLI   SVT2PR05,C'Y'       COMBINE CABLE NETWORKS                       
         BNE   TBAUP20              NO                                          
*                        X'E8' NOW                                              
         CLI   KEY+7,CABLESTA      THIS A CABLE STATION                         
         BL    TBAUP20              NO                                          
         NI    KEY+9,CABLENET      GET ALL CABLE NETWORKS                       
         B     TBAUP20                                                          
         EJECT                                                                  
* DO ALL EQUIV PRODS IF PROD EQUIV CLT *                                        
*                                                                               
TBAUP60  CLI   SVPROF13,C'Y'       THIS AN EQUIV PROD CLT                       
         BNE   TBAUP90              NO                                          
*                                                                               
* 1ST PRD2 FOR ALL ENTRIES *                                                    
*                                                                               
         CLI   SVTBPRD2,0                                                       
         BE    TBAUP70                                                          
         CLI   BLOCK+17,0           NO EQUIVALENT PRODS                         
         BE    TBAUP70               YES                                        
         CLI   0(R4),0              AT END                                      
         BE    TBAUP64               YES                                        
         LR    R7,R5                                                            
         MVC   BLOCK+41(1),SVTBPRD2                                             
         MVC   SVTBPRD2,0(R4)                                                   
         LA    R4,1(,R4)                                                        
TBAUP62  OC    SVTBNXLN,SVTBNXLN   TEST MORE DATA                               
         BZ    TBAUP88                                                          
         CLC   SVTBNXPR,SVTBPRD                                                 
         BNE   TBAUP88                                                          
         CLC   SVTBNXP2,BLOCK+41                                                
         BNE   TBAUP88                                                          
         MVC   SVTBNXP2,SVTBPRD2                                                
         LA    R7,SVTBNEXT                                                      
         B     TBAUP62                                                          
*                                                                               
* RESTORE ORIGINAL PARTNER PRD *                                                
*                                                                               
TBAUP64  LR    R7,R5                                                            
         MVC   BLOCK+41(1),SVTBPRD2                                             
TBAUP66  MVC   SVTBPRD2,BLOCK+17                                                
         OC    SVTBNXLN,SVTBNXLN   TEST MORE DATA                               
         BZ    TBAUP68                                                          
         CLC   SVTBNXPR,SVTBPRD                                                 
         BNE   TBAUP68                                                          
         CLC   SVTBNXP2,BLOCK+41                                                
         BNE   TBAUP68                                                          
         LA    R7,SVTBNEXT                                                      
         B     TBAUP66                                                          
TBAUP68  LR    R7,R5                                                            
         LA    R4,BLOCK+18                                                      
         EJECT                                                                  
* THEN PRD FOR ALL ENTRIES *                                                    
*                                                                               
TBAUP70  CLI   BLOCK,0             NO EQUIVALENT PRODS                          
         BE    TBAUP90               YES                                        
         CLI   0(R2),0              AT END                                      
         BE    TBAUP80               YES                                        
         LR    R7,R5                                                            
         MVC   BLOCK+40(1),SVTBPRD                                              
*                                                                               
         MVC   SVTBPRD,0(R2)                                                    
         LA    R2,1(,R2)                                                        
*                                                                               
TBAUP72  OC    SVTBNXLN,SVTBNXLN   TEST MORE DATA                               
         BZ    TBAUP86                                                          
         CLC   SVTBNXPR,BLOCK+40                                                
         BNE   TBAUP86                                                          
         CLC   SVTBNXP2,SVTBPRD2                                                
         BNE   TBAUP86                                                          
         MVC   SVTBNXPR,SVTBPRD                                                 
         LA    R7,SVTBNEXT                                                      
         B     TBAUP72                                                          
         SPACE 2                                                                
* RESTORE ORIGINAL PRODUCT *                                                    
*                                                                               
TBAUP80  LR    R7,R5                                                            
         MVC   BLOCK+40(1),SVTBPRD                                              
*                                                                               
TBAUP84  MVC   SVTBPRD,BLOCK                                                    
         OC    SVTBNXLN,SVTBNXLN   TEST MORE DATA                               
         BZ    TBAUP90                                                          
         CLC   SVTBNXPR,BLOCK+40                                                
         BNE   TBAUP90                                                          
         CLC   SVTBNXP2,SVTBPRD2                                                
         BNE   TBAUP90                                                          
         LA    R7,SVTBNEXT                                                      
         B     TBAUP84                                                          
*                                                                               
* START AT 1ST PRD/PTR ENTRY *                                                  
*                                                                               
TBAUP86  LR    R7,R5                                                            
*                                                                               
TBAUP88  B     TBAUP12                                                          
*                                                                               
TBAUP90  LR    R7,R3               LAST ENTRY OF EQUAL PRD/PTR SERIES           
*                                                                               
         OC    SVTBNXLN,SVTBNXLN                                                
         BZ    TBAUPX                                                           
         LA    R7,SVTBNEXT                                                      
         LR    R5,R7                                                            
         LR    R3,R7                                                            
         B     TBAUP10                                                          
TBAUPX   XIT1                                                                   
         EJECT                                                                  
* GET ANY EQUIVALENT PRODUCTS *                                                 
*                                                                               
GEP      NTR1                                                                   
*                                                                               
         CLC   BYTE,0(R4)                                                       
         BE    TBAUPX                                                           
         XC    0(16,R4),0(R4)                                                   
         XC    KEY,KEY                                                          
         MVC   PEQKID-PEQKEY+KEY,=X'0A37'                                       
         MVC   PEQKAM-PEQKEY+KEY(3),BAGYMD                                      
         LA    R0,220                                                           
         L     R1,ASVCLIST                                                      
GEP04    CLC   BYTE,3(R1)                                                       
         BE    GEP06                                                            
         LA    R1,4(,R1)                                                        
         CLI   0(R1),0                                                          
         BNH   *+8                                                              
         BCT   R0,GEP04                                                         
         DC    H'0'                                                             
GEP06    MVC   PEQKPRD-PEQKEY+KEY,0(R1)                                         
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(8),KEYSAVE      THIS A BASE PRODUCT                          
         BNE   TBAUPX               NO, BYPASS BUILD TABLE                      
*                                                                               
         L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         GOTO1 GETREC                                                           
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         BNE   TBAUPX                                                           
         LA    R5,15                                                            
         MVC   0(1,R4),BYTE                                                     
         LA    R4,1(,R4)                                                        
         USING PEQDTAEL,R6                                                      
GEP10    LA    R0,220                                                           
         L     R1,ASVCLIST                                                      
*                                                                               
GEP12    CLC   PEQPROD,0(R1)                                                    
         BE    GEP16                                                            
         LA    R1,4(,R1)                                                        
         CLI   0(R1),0                                                          
         BNH   *+8                                                              
         BCT   R0,GEP12                                                         
         DC    H'0'                                                             
GEP16    MVC   0(1,R4),3(R1)                                                    
         LA    R4,1(,R4)                                                        
         BCT   R5,*+6                                                           
         DC    H'0'                                                             
GEP18    BRAS  RE,NEXTEL                                                        
         BE    GEP10                                                            
         B     TBAUPX                                                           
         EJECT                                                                  
* SEE IF ANY TBA PATTERNS - IF SO, ADJUST DATES SO ACTIVITY FOR                 
* PARTIAL WEEKS IS NOT SET OFF                                                  
*                                                                               
ADJDT    NTR1                                                                   
*                                                                               
         XC    WORK,WORK           WORK(6)    = DATE WORK AREA                  
*                                  WORK+6(3)  = DAY FROM GETDAY                 
*                                  WORK+9(3)  = SAVED TBA END DATE+1            
*                                                                               
ADJDT10  CLI   PTNLTYPE,0          END OF LIST                                  
         BE    TBAUPX                                                           
*                                                                               
         CLI   PTNLTYPE,255        TBA PATTERN                                  
         BNE   ADJDT20                                                          
*                                                                               
* FORCE END DATE TO SUNDAY OF WEEK *                                            
*                                                                               
         GOTO1 DATCON,DMCB,(2,PTNLEND),(0,WORK)                                 
         GOTO1 GETDAY,(R1),(0,WORK),WORK+6                                      
         CLC   WORK+6(3),=CL3' '                                                
         BNE   *+6                                                              
         DC    H'0'                                                             
         LLC   R0,DMCB             GET DAY NUMBER                               
         CHI   R0,7                                                             
         BE    ADJDT14                                                          
*                                                                               
* CHANGE DATE TO PREV MONDAY, THEN ADD 6 TO GET TO NEXT SUNDAY                  
*                                                                               
         BCTR  R0,0                                                             
         LNR   R0,R0                                                            
         GOTO1 ADDAY,(R1),WORK,WORK,(R0)                                        
         LA    R0,6                                                             
         GOTO1 ADDAY,(R1),WORK,WORK,(R0)                                        
         GOTO1 DATCON,(R1),(0,WORK),(2,PTNLEND)                                 
*                                                                               
* SAVE NEXT MONDAY DATE FOR FOLLOWING PTN'S                                     
*                                                                               
ADJDT14  LA    R0,1                                                             
*                                                                               
         GOTO1 ADDAY,(R1),WORK,WORK,(R0)                                        
         GOTO1 DATCON,(R1),(0,WORK),(2,WORK+9)                                  
         B     ADJDT30                                                          
*                                                                               
* SEE IF PREV TBA NEEDS TO ADJUST THIS                                          
*                                                                               
ADJDT20  CLC   WORK+9(2),PTNLSTR                                                
         BNH   *+10                                                             
         MVC   PTNLSTR,WORK+9                                                   
*                                                                               
         CLC   WORK+9(2),PTNLEND                                                
         BNH   *+10                                                             
         MVC   PTNLEND,WORK+9                                                   
*                                                                               
ADJDT30  LA    R8,L'PTNLIST(,R8)                                                
         B     ADJDT10                                                          
         EJECT                                                                  
*==============================================================                 
* SEE IF ANY ELEMS TO DELETE FROM ACTIVITY                                      
*==============================================================                 
                                                                                
TBACK    NTR1                                                                   
*                                                                               
* SET NEW END TO PERIOD END                                                     
*                                                                               
         MVC   FULL+2(2),SVGENEND                                               
*        GOTO1 DATCON,DMCB,(2,SVGENEND),(2,FULL+2)                              
*                                                                               
TBACK10  LA    R0,7                                                             
*                                                                               
         GOTO1 ADDAY,DMCB,WORK,WORK,(R0)                                        
*                                                                               
         GOTO1 DATCON,(R1),(0,WORK),(2,FULL)                                    
*                                                                               
         CLC   FULL(2),FULL+2      AFTER SVGENEND                               
         BH    TBAUPX                                                           
         USING TBADTAEL,R6                                                      
*                                                                               
TBACK20  CLC   TBADTASL,SVTBSLN    SAME SLN                                     
         BNE   TBACK30                                                          
         CLC   TBADTAS2,SVTBSLN2   SAME PRTNR LEN                               
         BNE   TBACK30                                                          
         CLC   TBADTAWK,FULL       CK IF THIS WEEK                              
         BNE   TBACK30                                                          
         OI    UPDSW,UPDTBA        ***INDICATE RECORD UPDATED***                
*                                                                               
         GOTO1 VRECUP,DMCB,AIO,(R6) DELETE ELEMENT                              
         B     TBACK10                                                          
*                                                                               
TBACK30  BRAS  RE,NEXTEL                                                        
         BE    TBACK20                                                          
         B     TBAUPX                                                           
         LTORG                                                                  
         DROP  R6                                                               
*                                                                               
         EJECT                                                                  
CLRBUFF  SR    R0,R0               R4 HAS A(BUFF), R5 HAS LEN                   
         LA    R1,X'40'                                                         
         SLL   R1,24               SET TO BLANK FILL                            
         MVCL  R4,R0                                                            
         BR    RE                                                               
                                                                                
*==========================================================                     
* COUNT NUMBER OF 132 CHAR PRINT LINES AT 0(R1)                                 
* AND PRINT THEM                                                                
*==========================================================                     
                                                                                
         USING SPOOLD,R8                                                        
PRTBUFF  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LR    R4,R1               COUNT NUMBER OF PRINT LINES                  
         SR    R5,R5                                                            
*                                                                               
PRTBUFF2 CLI   0(R4),0             TEST FORCE PRINT                             
         BE    *+14                                                             
         CLC   0(132,R4),SPACES                                                 
         BNH   PRTBUFF4                                                         
         LA    R4,132(R4)                                                       
         BCT   R5,PRTBUFF2                                                      
*                                                                               
PRTBUFF4 LPR   R0,R5                                                            
         JZ    EXIT                                                             
*                                                                               
         MVC   SVSPCING,SPACING    SAVE SPACING VALUE                           
         MVI   SPACING,1                                                        
*                                                                               
PRTBUFF6 LA    R4,P                                                             
         LA    R5,4                                                             
         CR    R5,R0               TEST MORE THAN 4 LINES REMAIN                
         BL    PRTBUFF8                                                         
         LR    R5,R0               DO THE NUMBER THAT REMAIN                    
         MVC   SPACING,SVSPCING    AND RESTORE ORIGINAL SPACING                 
*                                                                               
PRTBUFF8 MVC   0(132,R4),0(R1)                                                  
         MVC   0(132,R1),SPACES    BLANK THE SOURCE NOW                         
         LA    R1,132(R1)                                                       
         LA    R4,132(R4)                                                       
         BCT   R5,PRTBUFF8                                                      
*                                                                               
         BRAS  RE,GOSPL            PRINT 4 LINES                                
         AHI   R0,-4                                                            
         BP    PRTBUFF6                                                         
         J     EXIT                                                             
         EJECT                                                                  
*==============================================================                 
* SET PERCENTAGES IN COMMERCIAL BUFFER                                          
* PATTERN RECORD IS IN IO1                                                      
*==============================================================                 
                                                                                
         USING SVCMLD,R2                                                        
SETPCTS  NTR1  BASE=*,LABEL=*                                                   
         MVI   ELCODE,X'32'        FIRST SEE IF ONLY ONE CMML                   
         L     R6,AIO1                                                          
         BRAS  RE,GETEL                                                         
         BNE   SETPCT2                                                          
         CLI   1(R6),3             IF ONLY 1 CMML                               
         BNE   SETPCT2             THEN FORCE 100 PCT ELEMENT                   
         MVI   ELEM,X'34'                                                       
         MVI   ELEM+1,5                                                         
         MVI   ELEM+2,C'A'                                                      
         MVC   ELEM+3(2),=H'100'                                                
         B     SETPCT6                                                          
*                                                                               
SETPCT2  MVI   ELCODE,X'36'        LOOK FOR A PERCENTAGE ELEMENT                
         L     R6,AIO1                                                          
         BRAS  RE,GETEL                                                         
         BE    SETPCT4                                                          
*                                                                               
         MVI   ELCODE,X'34'                                                     
         L     R6,AIO1                                                          
         BRAS  RE,GETEL                                                         
         BE    SETPCT4                                                          
         MVI   DOPCT,C'N'                                                       
         J     EXIT                                                             
*                                                                               
SETPCT4  MVC   ELEM,0(R6)          SAVE PERCENTAGE ELEMENT                      
*                                                                               
SETPCT6  L     R2,ASVCMLS                                                       
         USING SVCMLD,R2                                                        
         OC    0(8,R2),0(R2)                                                    
         BNZ   SETPCT10                                                         
         MVI   DOPCT,C'N'                                                       
         J     EXIT                                                             
*                                                                               
SETPCT10 LA    R6,ELEM                                                          
         LLC   R0,1(R6)                                                         
         AHI   R0,-2                                                            
         SRDL  R0,32                                                            
         D     R0,=F'3'            GET NUMBER OF ENTRIES                        
         LR    R0,R1                                                            
         LA    R6,2(R6)                                                         
*                                                                               
SETPCT12 MVC   SVCMLPCT,1(R6)                                                   
*                                                                               
         LA    R2,SVCMLNXT                                                      
         CLI   SVCMLPIG,2          SKIP PIGGYBACK ENTRY                         
         BE    *-8                                                              
         LA    R6,3(R6)                                                         
         BCT   R0,SETPCT12                                                      
         J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
*=================================================================              
* READ STATION ADDRESS RECORD AND SAVE IT                                       
*=================================================================              
                                                                                
         USING SPOOLD,R8                                                        
GETSTADR NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    STCMLTYP,STCMLTYP                                                
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0A28'                                                  
         MVC   KEY+2(1),BAGYMD                                                  
         MVC   KEY+3(5),SVTBSTA                                                 
*                                                                               
         L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    GETST12                                                          
*                                                                               
         MVC   0(100,R6),SPACES    MOVE SPACES TO NOT FOUND RECORD              
         MVC   2(5,R6),SVTBSTA                                                  
*                                                                               
         MVI   ERROR,NOSTAADR                                                   
         L     R2,SVTBLINE         POSITION CURSOR                              
         A     R2,ATWA                                                          
*                                                                               
         TM    SVOPT2,OP2NOADR     TEST IGNORE MISSING ADDRESS                  
         BO    GETST14                                                          
         CLI   SVT3PROF+1,C'N'     TEST WARN IF MISSING                         
         BE    GETST14             NO - SO DON'T WARN                           
         CLI   OFFLINE,C'Y'                                                     
         JNE   PRTERR              AND SUPPRESS ERROR IF OFFLINE                
         B     GETST14                                                          
*                                                                               
GETST12  MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING STADTAEL,R6                                                      
         CLI   STADTALN,119        OLD ELEM                                     
         BE    *+10                                                             
         MVC   STCMLTYP,STACMLT                                                 
*                                                                               
GETST14  L     RE,ASVSTAD                                                       
         MVC   0(24,RE),=CL24'** TV TRAFFIC DESK **'                            
         CLI   QMED,C'T'                                                        
         BE    GETST20                                                          
         MVC   0(24,RE),=CL24'RADIO TRAFFIC DESK'                               
         CLI   QMED,C'R'                                                        
         BE    GETST20                                                          
         CLI   QMED,C'X'           NETWORK RADIO                                
         BE    GETST20                                                          
         MVC   0(24,RE),=CL24'**  TRAFFIC DESK **'                              
*                                                                               
GETST20  MVC   24(96,RE),2(R6)                                                  
         J     EQXIT                                                            
         LTORG                                                                  
         DROP  R6                                                               
         EJECT                                                                  
*=================================================================              
* UPDATE SHIP LIST RECORDS                                                      
*=================================================================              
*                                                                               
UPSHIP   NTR1  BASE=*,LABEL=*                                                   
         MVC   AIO,AIO3            ***** USE IO3 *****                          
         L     R2,ASVCMLS                                                       
         USING SVCMLD,R2                                                        
         OC    0(8,R2),0(R2)       TEST NO ENTRIES IN LIST                      
         JZ    EQXIT                                                            
                                                                                
* CREATE NEW ELEMENT IN ELEM *                                                  
                                                                                
UPSH2    NI    UPDSW,X'FF'-UPDEST   INIT UPDATED ESTIMATE ELEM                  
         XC    ELEM,ELEM                                                        
         LA    R6,ELEM                                                          
         USING SHPDTAEL,R6                                                      
                                                                                
* REMEMBER THAT AN 8 CHAR CMML WILL BE UNPACKED, AT LEAST TO START              
                                                                                
         MVI   0(R6),X'10'                                                      
         MVI   1(R6),SHPDTAX-SHPDTAEL                                           
         MVC   SHPCMML,SVCMLCOD    SET 8-BYTE CMML CODE                         
*                                                                               
         NI    SHPNOSHP,X'FF'-SHPISADI                                          
         TM    SVCMLST,X'01'       TEST CMML NATIVELY PACKED                    
         BZ    *+8                 NO                                           
         OI    SHPNOSHP,SHPISADI   YES - SET FLAG                               
*                                                                               
         CLI   SVCMLPIG,0                                                       
         BE    UPSH5X                                                           
         MVC   SHPCMML2,SVCMLCOD+L'SVCMLDTA                                     
*                                                                               
         TM    SVCMLST,X'01'                TEST CMML IS PACKED                 
         BZ    UPSH5A                       NO                                  
         TM    SVCMLST+L'SVCMLDTA,X'01'     TEST PARTNER IS PACKED              
         BO    UPSH5X                       YES - BOTH PACKED IS GOOD           
         MVC   WORK(8),SVCMLCOD+L'SVCMLDTA  TRY TO PACK PARTNER                 
         MVC   WORK+8(4),SPACES                                                 
         GOTO1 VTRPACK,DMCB,(C'P',WORK),SHPCMML2                                
         BE    UPSH5X                                                           
         DC    H'0'                CAN'T HANDLE PACKED AND UNPACKED             
*                                                                               
UPSH5A   TM    SVCMLST+L'SVCMLDTA,X'01' CMML NOT PACKED - CHECK PTNR            
         BZ    UPSH5X                   BOTH UNPACKED IS GOOD                   
         MVC   WORK(8),SVCMLCOD         PTNR PACKED-TRY TO PACK PRD             
         MVC   WORK+8(4),SPACES                                                 
         GOTO1 VTRPACK,DMCB,(C'P',WORK),SHPCMML                                 
         BE    *+6                                                              
         DC    H'0'                                                             
         OI    SHPNOSHP,SHPISADI                                                
*                                                                               
UPSH5X   GOTO1 DATCON,DMCB,(2,SVCMLFTD),(3,SHPFTD)                              
         GOTO1 (RF),(R1),(2,SVCMLLTD),(3,SHPLTD)                                
*                                                                               
         MVC   SHPQDATE,BTODAY                                                  
*                                                                               
         CLI   SHIPYORN,C'N'                                                    
         BNE   *+8                                                              
         OI    SHPNOSHP,X'80'                                                   
*                                                                               
UPSH6    CLI   SVCMLPIG,2                                                       
         BNE   UPSH10                                                           
         MVI   SHPPIG,C'Y'         SET PASSIVE PIGGYBACK                        
         LA    R6,ELEM                                                          
         B     UPSH20                                                           
                                                                                
*============================================================                   
* CREATE NEW ENTRY IN SHIP TABLE IF NOT THERE                                   
* REMEMBER - ONLY GET HERE FOR FIRST PIGGYBACK ENTRY!                           
*============================================================                   
                                                                                
UPSH10   L     R4,ASHPLIST                                                      
         USING SVSHPD,R4                                                        
*                                                                               
UPSH12   CLI   0(R4),0             TEST EOL                                     
         BE    UPSH15                                                           
*                                                                               
         CLC   SVSHPADI,SVCMLADI   MATCH ADID                                   
         BNE   UPSH12X             NO                                           
         CLI   SVCMLPIG,0          TEST PIGGYBACK                               
         BE    UPSH20              NO - THIS IS A MATCH                         
         CLC   SVSHPAD2,SVCMLADI+L'SVCMLDTA  ELSE MATCH SECOND ADID             
         BE    UPSH20                                                           
*                                                                               
UPSH12X  LA    R4,SVSHPNXT                                                      
         C     R4,ASHPLSTX         AT OR PAST END OF SHIP TABLE                 
         BL    UPSH12                                                           
*                                                                               
         L     R3,ATWA                                                          
         XC    CONHEAD-T216FFD(,R3),CONHEAD-T216FFD(R3)                         
                                                                                
         MVC   CONHEAD-T216FFD(23,R3),=C'* ERR * TOO BIG, RUN OV'               
         TM    WHEN,X'20'                                                       
         BO    *+10                                                             
         MVC   CONHEAD-T216FFD(28,R3),=C'* ERR * TOO BIG, RUN SOON/OV'          
                                                                                
         TM    WHEN,X'10'                                                       
         BZ    *+10                                                             
         MVC   CONHEAD-T216FFD(29,R3),=C'* ERR * CONTACT DDS:SHIP TBLE'         
                                                                                
         MVI   ERROR,X'FF'         SET ERROR FLAG                               
         J     PRTERR2                                                          
*                                                                               
UPSH15   XC    SVSHPENT(L'SVSHPENT+1),SVSHPENT                                  
         MVC   SVSHPCMS,ELEM+2     ADD NEW ENTRY                                
*                                                                               
UPSH20   CLI   SVCMLPIG,2                                                       
         BE    UPSH22                                                           
         MVC   SVSHPSQA,SVCMLSEQ                                                
         MVC   SVSHPPRD,SVCMLPRD                                                
         MVC   SVSHPADI,SVCMLADI                                                
*                                                                               
         TM    SVCMLST,X'40'       COMML TEXT                                   
         BZ    *+8                                                              
         OI    SVOPT,PTCMLTXT      SET TO PRINT CMML TEXT                       
*                                                                               
         MVC   SVSHPDDT,SVCMLDDT                                                
         MVC   SVSHPDTM,SVCMLDTM                                                
         MVC   SVSHPHSE,SVCMLHSE                                                
         MVC   SVSHPTYP,SVCMLTYP                                                
         B     UPSH24                                                           
*                                                                               
UPSH22   MVC   SVSHPSQB,SVCMLSEQ                                                
         MVC   SVSHPPR2,SVCMLPRD                                                
         MVC   SVSHPAD2,SVCMLADI                                                
         MVC   SVSHPDD2,SVCMLDDT                                                
         MVC   SVSHPDT2,SVCMLDTM                                                
         MVC   SVSHPHS2,SVCMLHSE                                                
         MVC   SVSHPTY2,SVCMLTYP                                                
                                                                                
*==============================================================                 
* READ SHIPPING RECAP RECORD                                                    
*==============================================================                 
                                                                                
UPSH24   OI    SVSHPFLG,SVSHPF_YES  SET FLAG FOR ACTIVE THIS MKT                
*                                                                               
         OI    UPDSW,UPDSHIP        PRESET 'UPDATED' SWITCH                     
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0A25'                                                  
         MVC   KEY+2(3),BAGYMD     A-M/CLT                                      
         MVC   KEY+5(5),SVTBMKST                                                
         MVC   KEY+11(2),SVCMLSEQ                                               
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    UPSH26                                                           
*                                                                               
* NOT FOUND - CREATE NEW RECORD *                                               
*                                                                               
         L     R6,AIO                                                           
         XC    0(256,R6),0(R6)                                                  
         MVC   0(13,R6),KEYSAVE                                                 
         MVI   14(R6),24                                                        
         MVC   20(2,R6),AGENCY                                                  
         OI    UPDSW,UPDSHPGN      MUST REQUEST SHIP GEN                        
         B     UPSH60                                                           
*                                                                               
UPSH26   DS    0H                                                               
         GOTO1 GETREC                                                           
*                                                                               
         MVI   ELCODE,X'10'                                                     
         L     R6,AIO                                                           
         BRAS  RE,GETEL                                                         
         B     *+8                                                              
*                                                                               
         USING SHPDTAEL,R6                                                      
UPSH30   BRAS  RE,NEXTEL                                                        
         BNE   UPSH60                                                           
         CLC   SHPCMML(17),ELEM+2  MATCH BOTH CMMLS                             
         BNE   UPSH30                                                           
*                                                                               
* MATCHING ELEMENT FOUND - TEST TO UPDATE TELECAST DATES *                      
*                                                                               
         NI    UPDSW,X'FF'-UPDSHIP RESET UPDATE SWITCH                          
         XC    DUB,DUB             CLEAR LAST TLCST SAVE AREA                   
*                                                                               
         OC    SHPSHPDT,SHPSHPDT   TEST SHIPPED YET                             
         BNZ   UPSH40               YES - FIRST TLCST DATE NOT RELEVANT         
*                                                                               
         LA    RE,ELEM+SHPFTD-SHPDTAEL                                          
         CLC   SHPFTD,0(RE)        TEST LOWER FIRST TLCST DATE                  
         BNH   UPSH40                                                           
         MVC   SHPFTD,0(RE)                                                     
         OI    UPDSW,UPDSHIP+UPDSHPGN UPDATE REC & REQUEST SHIP GEN             
*                                                                               
UPSH40   MVC   DUB(6),SHPFTD       SAVE OLD FIRST/LAST TLCST DATES              
*                                                                               
         LA    RE,ELEM+SHPLTD-SHPDTAEL                                          
         CLC   SHPLTD,0(RE)        HIGHER LAST TLCST DATE                       
         BNL   UPSH42                                                           
         MVC   SHPLTD,0(RE)                                                     
         OI    UPDSW,UPDSHIP                                                    
*                                                                               
UPSH42   LLC   RE,ELEM+SHPNOSHP-SHPDTAEL                                        
         LLC   RF,SHPNOSHP                                                      
         SRL   RE,7                                                             
         SRL   RF,7                                                             
         CR    RE,RF               BOTH OLD AND NEW SAME NO SHIP STA            
         BE    UPSH50                                                           
         OI    UPDSW,UPDSHIP                                                    
         TM    SHPNOSHP,X'80'      WAS PREV RUN NOSHIP                          
         BZ    UPSH50                                                           
         CLI   SHIPYORN,C'N'                                                    
         BE    *+8                                                              
         OI    UPDSW,UPDSHPGN                                                   
* NEXT INSTRUCTION SEEMS WRONG - IF IT WAS PREVIOUSLY SHIPPED,                  
* LEAVE OLD SHIP DATE THERE!  MH 22JAN09                                        
**NOP**  B     UPSH52              YES, DON'T MOVE OUT OLD ELEM                 
                                                                                
*===============================================================                
* SAVE ELEMENT (WITHOUT SHPNOSHP FLAG) AND DELETE FROM RECORD                   
*===============================================================                
                                                                                
UPSH50   MVC   ELEM(SHPNOSHP-SHPDTAEL),0(R6)                                    
*                                                                               
UPSH52   XC    BLOCK(256),BLOCK                                                 
         MVC   BLOCK(SHPDTAX-SHPDTAEL),0(R6) SAVE OLD ELEM                      
         GOTO1 VRECUP,DMCB,AIO,(R6)                                             
         LA    R6,ELEM                                                          
*                                                                               
         OC    SHPSHPDT,SHPSHPDT   TEST SHIPPED YET                             
         BZ    UPSH60                                                           
         TM    SVOPT,OPT3REPT      TEST REPRINT                                 
         BZ    UPSH56                 NO                                        
         CLC   SVINSDT,SHPSHPDT    THIS SHIPPED BEFORE INSTR DATE               
         BNH   UPSH60               YES, ACT AS IF NEEDS SHIP                   
*                                                                               
UPSH56   TM    SVOPT,OPTRERUN      TEST RERUN                                   
         BZ    UPSH58                 NO                                        
         CLC   SVOPTDTE,SHPSHPDT     COMPARE RERUN DATE TO SHIP DATE            
         BNH   UPSH60                 IF NOT HIGH, SHIP IT                      
*                                                                               
* SHIPPED PREVIOUSLY - TEST NEEDS RESHIP *                                      
*                                                                               
UPSH58   GOTO1 DATCON,DMCB,(3,DUB+3),WORK                                       
*                                                                               
         LLC   R0,SVPROF1          USER SPECIFIED RESHIP PERIOD                 
         CLI   SVPROF1,0                                                        
         BNE   *+8                                                              
         LA    R0,30               DEFAULT IS 30                                
*                                                                               
         GOTO1 ADDAY,(R1),WORK,WORK+6,(R0)                                      
*                                                                               
         GOTO1 DATCON,(R1),(5,0),WORK+12                                        
*                                                                               
         CLC   WORK+6(6),WORK+12   COMPARE TO TODAY                             
         BNL   UPSH70              DO NOT FLAG SHIP LIST                        
*                                                                               
         MVC   SHPFTD,DUB          RESET FIRST TLCST DATE FOR RESHIP            
         XC    SHPSHPDT,SHPSHPDT   AND CLEAR DATE SO IT WILL HAPPEN             
         OI    UPDSW,UPDSHIP                                                    
*                                                                               
UPSH60   CLI   SVCMLPIG,2          TEST PRD2 REC                                
         BE    UPSH70                                                           
*                                                                               
         MVI   SVSHPSHP,C'S'       SET SHIP FLAG                                
*                                                                               
         BAS   RE,PRDSHP           SET SHIP FLAG IN PROD LIST                   
         DROP  R4                                                               
*                                                                               
UPSH70   TM    UPDSW,UPDSHIP       TEST UPDATE REQUIRED                         
         BZ    UPSH78               NO                                          
*                                                                               
         L     RE,AIO                                                           
         SR    R0,R0                                                            
         ICM   R0,3,13(RE)         GET REC LEN                                  
         AR    RE,R0               POINT TO END OF REC                          
         XC    0(2,RE),0(RE)       AND CLEAR                                    
*                                                                               
         GOTO1 ADDELEM             INSERT NEW ELEM IN RECORD                    
*                                                                               
         MVC   BLOCK(SHPDTAX-SHPDTAEL),ELEM  SAVE ELEM FOR P/B                  
*                                                                               
         CLI   QBEST,0             EST = NO                                     
         BE    UPSH75              YES                                          
*                                                                               
         MVI   ELCODE,X'20'        ESTIMATE ELEMENT                             
         L     R6,AIO                                                           
         BRAS  RE,GETEL                                                         
         BNE   UPSH72                                                           
*                                                                               
         BAS   RE,ESTMASK          TEST ESTIMATE MASK/SET ON IF NOT             
         B     UPSH75                                                           
*                                                                               
* CREATE NEW ESTIMATE ELEMENT                                                   
UPSH72   XC    ELEM,ELEM                                                        
         LA    R6,ELEM                                                          
         USING SHPESTEL,R6                                                      
*                                                                               
         MVI   SHPESTEL,X'20'                                                   
         MVI   SHPESTLN,SHPESTX-SHPESTEL                                        
         BAS   RE,ESTMASK                                                       
         GOTO1 ADDELEM             INSERT NEW ELEM IN RECORD                    
         XC    ELEM,ELEM                                                        
         MVC   ELEM(SHPDTAX-SHPDTAEL),BLOCK  RESTORE ELEM                       
*                                                                               
UPSH75   L     RF,PUTREC                                                        
         CLC   KEY(13),KEYSAVE                                                  
         BE    UPSH77                                                           
         L     RF,ADDREC                                                        
         TM    UPDSW,UPDSHIP                                                    
         BZ    UPSH77                                                           
         OI    UPDSW,UPDSHPGN      MUST REQUEST SHIP GEN                        
*                                                                               
UPSH77   TM    SVOPT3,OPT3REPT     THIS A REPRINT                               
         BO    *+14                                                             
         TM    SVOPT,OPTTEST       TEST TEST RUN                                
         BO    *+6                                                              
         BASR  RE,RF                                                            
*                                                                               
         MVC   ELEM(SHPDTAX-SHPDTAEL),BLOCK  RESTORE ELEM FOR P/B               
*                                                                               
UPSH78   TM    UPDSW,UPDEST        UPDATED ESTIMATE ALREADY                     
         BO    UPSH80              YES, DONE                                    
*                                                                               
         TM    UPDSW,UPDSHIP       TEST UPDATE REQUIRED                         
         BO    UPSH80                                                           
*                                                                               
         CLI   QBEST,0             EST = NO                                     
         BE    UPSH80              YES                                          
*                                                                               
         MVC   BLOCK(SHPDTAX-SHPDTAEL),ELEM   SAVE ELEM                         
*                                                                               
         BAS   RE,AESTELEM         ADD ESTIMATE ELEM IF NEEDED                  
         TM    UPDSW,UPDEST        UPDATED EST ELEM?                            
         BO    UPSH75                                                           
*                                                                               
* NEED TO ADD RECORD UNDER PASSIVE PRD FOR PIGGYBACKS *                         
*                                                                               
UPSH80   CLI   SVCMLPIG,1          TEST P/B PRD1                                
         BNE   UPSH82               NO                                          
         NI    UPDSW,X'FF'-UPDEST  INIT UPDATED ESTIMATE                        
         LA    R2,L'SVCMLDTA(R2)   POINT TO PRD2 DATA                           
         CLI   SVCMLPIG,2                                                       
         BE    UPSH6                                                            
         DC    H'0'                                                             
*                                                                               
UPSH82   LA    R2,L'SVCMLDTA(R2)   NEXT COMMERCIAL ENTRY                        
         CLI   0(R2),0             TEST EOL                                     
         JE    EQXIT                                                            
         CLI   SVCMLPIG,1          TEST SOLO OR ACTIVE PB                       
         BH    UPSH82              NO - SKIP                                    
         B     UPSH2               ELSE GO PROCESS                              
         EJECT                                                                  
*=========================================================                      
* SET PRODUCT TO SHIP                                                           
*=========================================================                      
                                                                                
         USING SVSHPD,R4                                                        
PRDSHP   L     RF,ASVPRDS                                                       
         USING SVPRDD,RF                                                        
*                                                                               
PRDSHP10 CLC   SVPRDCD,SVSHPPRD                                                 
         JE    PRDSHP20                                                         
         LA    RF,L'SVPRDDTA(RF)                                                
         CLI   SVPRDCD,0                                                        
         JNE   PRDSHP10                                                         
         DC    H'0'                                                             
*                                                                               
PRDSHP20 OI    SVPRDSHP,X'80'      SET NEEDS TO BE SHIPPED                      
         BR    RE                                                               
         DROP  RF                                                               
         LTORG                                                                  
         EJECT                                                                  
*================================================                               
* TEST ESTIMATE MASK                                                            
* TURN ON ESTIMATE MASK IF NOT ON YET                                           
*================================================                               
         USING SHPESTEL,R6                                                      
ESTMASK  NTR1                                                                   
         SR    R0,R0                                                            
         LLC   R0,QBEST            ESTIMATE                                     
         LR    RF,R0                                                            
         SR    RE,RE                                                            
         SLDL  RE,29               GET MASK BYTE NO. INTO RE                    
         SRL   RF,29               GET MASK BIT  NO. INTO RF                    
         LA    R1,SHPESMSK(RE)     R1 = A(CORRECT BYTE IN MASK)                 
         LA    RF,BITLIST(RF)                                                   
         MVC   BYTE,0(R1)          TEST FOR THIS ESTIMATE                       
         NC    BYTE,0(RF)                                                       
         CLI   BYTE,0              ANY ESTIMATE?                                
         BNE   ESTX                 YES                                         
         OC    0(1,R1),0(RF)       TURN BIT ON IN ESTIMATE MASK                 
         CR    R1,R1               SET CC EQ                                    
ESTX     XIT1                                                                   
                                                                                
BITLIST  DC    X'8040201008040201'                                              
*                                                                               
*===============================================================                
* ADD ESTIMATE ELEMENT OR ADD NEW ESTIMATE MASK TO EXISTING ELEM                
*===============================================================                
*                                                                               
AESTELEM NTR1                                                                   
         L     R6,AIO                                                           
         MVI   ELCODE,X'10'     RESTORE X'10' ELEM IF REMOVED EARLIER           
         BRAS  RE,GETEL                                                         
         B     *+8                                                              
AEST10   BRAS  RE,NEXTEL                                                        
         BNE   AEST20              ADD BACK THE X'10' ELEM                      
*                                                                               
         CLC   0(SHPDTAX-SHPDTAEL,R6),BLOCK  SAME ELEM?                         
         BNE   AEST10                                                           
         B     AEST30              ELEMENT IN RECORD                            
                                                                                
AEST20   MVC   ELEM(SHPDTAX-SHPDTAEL),BLOCK                                     
         L     R6,AIO                                                           
         LR    RE,R6                                                            
         SR    R0,R0                                                            
         ICM   R0,3,13(RE)         GET REC LEN                                  
         AR    RE,R0               POINT TO END OF REC                          
         XC    0(2,RE),0(RE)       AND CLEAR                                    
*                                                                               
         GOTO1 ADDELEM             INSERT NEW ELEM IN RECORD                    
*                                                                               
AEST30   MVI   ELCODE,X'20'        ESTIMATE ELEMENT                             
         L     R6,AIO                                                           
         BRAS  RE,GETEL                                                         
         BNE   AEST40                                                           
*                                                                               
         BAS   RE,ESTMASK          TEST EST MASK/SET ON IF NOT FND              
         BNE   AESTX               ESTIMATE FOUND IN ELEM                       
         OI    UPDSW,UPDEST        TURN ON UPDATED ESTIMATE ELEM                
         B     AESTX                                                            
*                                                                               
* CREATE NEW ESTIMATE ELEMENT                                                   
AEST40   XC    ELEM,ELEM                                                        
         LA    R6,ELEM                                                          
         USING SHPESTEL,R6                                                      
*                                                                               
         MVI   SHPESTEL,X'20'                                                   
         MVI   SHPESTLN,SHPESTX-SHPESTEL                                        
         BAS   RE,ESTMASK                                                       
         GOTO1 ADDELEM             INSERT NEW ELEM IN RECORD                    
         XC    ELEM,ELEM                                                        
         MVC   ELEM(SHPDTAX-SHPDTAEL),BLOCK  RESTORE ELEM                       
*                                                                               
         OI    UPDSW,UPDEST        TURN ON UPDATED ESTIMATE ELEM                
AESTX    XIT1                                                                   
*                                                                               
*                                                                               
*==================================================================             
* PRINT SHIPPING LIST                                                           
*==================================================================             
                                                                                
PRTSHIP  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         SR    R0,R0               CLEAR COUNTER FOR ALL ENTRIES                
         SR    R2,R2               CLEAR SHIP COUNT                             
         L     R4,ASHPLIST                                                      
         USING SVSHPD,R4                                                        
*                                                                               
PRTS2    CLI   0(R4),0                                                          
         BE    PRTS4                                                            
         CLI   SVSHPSHP,0          TO BE SHIPPED                                
         BE    *+6                  NO                                          
         BCTR  R2,0                                                             
         LA    R4,SVSHPNXT                                                      
         BCT   R0,PRTS2                                                         
         DROP  R4                                                               
*                                                                               
PRTS4    LPR   R0,R0               SET NUMBER OF ENTRIES                        
         BZ    PRTSX               SKIP SORT/PRINT IF NONE                      
*                                                                               
         GOTO1 XSORT,DMCB,ASHPLIST,(R0),L'SVSHPENT,16,0                         
*                                                                               
PRTS10   LPR   R2,R2               SET NUMBER OF ENTRIES                        
         BZ    PRTSX               SKIP SORT/PRINT IF NONE                      
                                                                                
* GENERATE AUTO SHIPPING REQ *                                                  
                                                                                
         CLI   SHIPYORN,C'N'       IF USER SAID NOSHIP,                         
         BE    PRTS20              ADD RECORDS BUT DO NOT PRINT                 
*                                                                               
         CLI   SHIPYORN,C'A'       TEST AUTO SHIP OPTION                        
         BNE   PRTS20               NO                                          
*                                                                               
         BRAS  RE,ASQ               GENERATE AUTO SHIP REQUEST                  
*                                                                               
PRTS20   CLI   SVT1PR6,C'E'       ELIMINATE SHIP MSG                            
         BE    PRTSX              YES                                           
                                                                                
         L     R4,ASHPLIST                                                      
         USING SVSHPD,R4                                                        
                                                                                
         L     R2,ASVCMLS          BUILD TABLE HERE IN ASVCMLS                  
         USING SHPRD,R2                                                         
         XC    SHPRENT,SHPRENT                                                  
*                                                                               
PRTS22   CLI   SVSHPSHP,0          TEST TO BE SHIPPED                           
         BE    PRTS40                NO                                         
*                                                                               
PRTS24   CLI   SHPRCC,0            EMPTY ENTRY                                  
         BE    PRTS26               YES                                         
*                                                                               
         CLC   SVSHPCA,SHPRCC                                                   
         BE    PRTS22                                                           
*                                                                               
         LA    R2,SHPRNXT                                                       
         B     PRTS24                                                           
*                                                                               
PRTS26   MVC   DUB,SVSHPCA                                                      
         BAS   RE,GCMLNUM          GO GET CLT CMML/ADID                         
         XC    SHPRNXT(L'SHPRENT),SHPRNXT                                       
*                                                                               
PRTS30   CLI   SVSHPCB,0           ANY P/B CML                                  
         BE    PRTS40               NO                                          
*                                                                               
         L     R2,ASVCMLS                                                       
PRTS32   CLC   SVSHPCB,SHPRCC                                                   
         BE    PRTS40                                                           
*                                                                               
         LA    R2,SHPRNXT                                                       
         CLI   SHPRCC,0                                                         
         BNE   PRTS32                                                           
*                                                                               
         MVC   DUB,SVSHPCB                                                      
         BAS   RE,GCMLNUM          GO CLT CMML/ADID                             
         XC    SHPRNXT(L'SHPRENT),SHPRNXT                                       
*                                                                               
PRTS40   CLI   SVSHPNXT,0          TEST MORE DATA IN LIST                       
         BE    PRTS50               NO                                          
         LA    R4,SVSHPNXT                                                      
         B     PRTS22                                                           
         EJECT                                                                  
*=================================================================              
* FORMAT SHIPPING LIST TO PRINT BUFFER (IO2)                                    
*=================================================================              
                                                                                
PRTS50   L     R4,ACMLEXP          CLEAR PRINT BUFFER                           
         L     R5,SIZEIO                                                        
         BRAS  RE,CLRBUFF                                                       
*                                                                               
         L     R4,ASHPLIST                                                      
         USING SVSHPD,R4                                                        
*                                                                               
         L     RF,ACMLEXP                                                       
         ST    RF,FULL                                                          
         MVI   0(RF),0             SKIP A LINE                                  
         LA    RF,132(RF)                                                       
*                                                                               
         MVC   00(38,RF),=C'YOU SHOULD HAVE ALL COMMERCIALS EXCEPT'             
         MVC   39(35,RF),=C'THE FOLLOWING WHICH WILL BE SHIPPED'                
*                                                                               
         CLI   SVT1PR6,C'Y'        SHOW ENCLOSED                                
         BNE   *+10                                                             
         MVC   59(15,RF),=CL15'ARE ENCLOSED'                                    
*                                                                               
         LA    RF,132(RF)          SKIP A LINE                                  
         MVI   0(RF),0             SET SO IT PRINTS                             
         ST    RF,FULL                                                          
         B     PRTS120                                                          
*                                                                               
PRTS60   CLI   SVSHPSHP,0          TEST TO BE SHIPPED                           
         BE    PRTS130             NO                                           
*                                                                               
         CLI   SVSHPCB,0           TEST PIGGYBACK                               
         BNE   PRTS90                                                           
*                                                                               
         LA    RE,8(RF)            SOLO CMML IS 8                               
         OC    SVSHPADI,SVSHPADI   IS THERE AN AD-ID                            
         BZ    *+8                 NO                                           
         LA    RE,4(RE)            ADID IS 12                                   
         CR    RE,R0               TEST PAST EOL                                
         BH    PRTS110                                                          
*                                                                               
         OC    SVSHPADI,SVSHPADI   IS THERE AN ADID                             
         BZ    PRTS70              NO                                           
         MVC   0(12,RF),SVSHPADI   AD-ID                                        
         LA    RF,13(RF)                                                        
         BAS   RE,ADDCOMMA                                                      
         B     PRTS130                                                          
*                                                                               
PRTS70   CLI   SVT1PR7,C'#'        PRINT CLT CML #                              
         BE    PRTS80              NO                                           
*                                                                               
PRTS72   MVC   0(8,RF),SVSHPCA     NO ADID/NO CLT CMML --> USE ISCI             
         LA    RF,9(RF)                                                         
         BAS   RE,ADDCOMMA                                                      
         B     PRTS130                                                          
*                                                                               
PRTS80   L     R2,ASVCMLS          FIND ENTRY IN TABLE                          
*                                                                               
PRTS82   CLC   SHPRCC,SVSHPCA      SAME CMML CODE                               
         BE    PRTS84              YES, GO PRT ITS CLT CMML #                   
         LA    R2,SHPRNXT          POINT TO NEXT ENTRY                          
         CLI   0(R2),0             END OF TABLE                                 
         BNE   PRTS82                                                           
         DC    H'0'                                                             
*                                                                               
PRTS84   CLC   SHPRCCN,SPACES      IS THERE A CLT CMML NUMBER                   
         BNH   PRTS72              NO - SO JUST SHOW CMML NUMBER                
         MVC   0(20,RF),SHPRCCN    CLT CML NUMBER                               
         LA    RF,21(RF)                                                        
         BAS   RE,ADDCOMMA                                                      
         B     PRTS130                                                          
         EJECT                                                                  
*==============================================================                 
* FIGURE OUT WHAT TO PRINT FOR PIGGYBACKS                                       
*==============================================================                 
                                                                                
PRTS90   XC    WORK,WORK                                                        
         MVC   WORK(8),SVSHPCA     MOVE ISCI                                    
         CLI   SVSHPADI,C' '       TEST FOR ADID                                
         BNH   PRTS92                                                           
         MVC   WORK(12),SVSHPADI   ADID ALWAYS WINS                             
         B     PRTS100                                                          
*                                                                               
PRTS92   CLI   SVT1PR7,C'#'        PRINT CLT CML #                              
         BNE   PRTS100                                                          
*                                                                               
         L     R2,ASVCMLS                                                       
PRTS94   CLC   0(8,R2),SVSHPCA     SAME CML CODE                                
         BE    PRTS96              YES                                          
         LA    R2,SHPRNXT          POINT TO NEXT ENTRY                          
         CLI   0(R2),0             END OF TABLE                                 
         BNE   PRTS94                                                           
         DC    H'0'                                                             
*                                                                               
PRTS96   CLI   SHPRCCN,C' '        TEST CLT CMML THERE                          
         BNH   *+10                NO                                           
         MVC   WORK(20),SHPRCCN    MOVE CLT CML NUMBER                          
*                                                                               
PRTS100  MVC   WORK+22(8),SVSHPCB  MOVE SECOND ISCI                             
         CLI   SVSHPAD2,C' '       TEST FOR ADID                                
         BNH   PRTS102                                                          
         MVC   WORK+22(12),SVSHPAD2   ADID ALWAYS WINS                          
         B     PRTS108                                                          
*                                                                               
PRTS102  CLI   SVT1PR7,C'#'        PRINT CLT CML #                              
         BNE   PRTS108                                                          
*                                                                               
         L     R2,ASVCMLS                                                       
PRTS104  CLC   0(8,R2),SVSHPCB     SAME CML CODE                                
         BE    PRTS106             YES                                          
         LA    R2,SHPRNXT          POINT TO NEXT ENTRY                          
         CLI   0(R2),0             END OF TABLE                                 
         BNE   PRTS104                                                          
         DC    H'0'                                                             
*                                                                               
PRTS106  CLI   SHPRCCN,C' '          TEST CLT CMML THERE                        
         BNH   *+10                  NO                                         
         MVC   WORK+22(20),SHPRCCN   MOVE CLT CML NUMBER                        
*                                                                               
PRTS108  ST    RF,DMCB+20          SAVE CURRENT RF                              
         GOTO1 SQUASHER,DMCB,WORK,42                                            
*                                                                               
         L     RF,DMCB+20          RESTORE RF                                   
         L     RE,4(R1)            GET SQUASHED LENGTH                          
         AR    RE,RF                                                            
         CR    RE,R0               TEST TEXT FITS THIS LINE                     
         BH    PRTS110                                                          
*                                                                               
         LA    R1,WORK             INSERT - OVER FIRST SPACE                    
         CLI   0(R1),C' '                                                       
         BE    *+12                                                             
         LA    R1,1(R1)                                                         
         B     *-12                                                             
         MVI   0(R1),C'-'                                                       
*                                                                               
         MVC   0(42,RF),WORK                                                    
         LA    RF,43(RF)                                                        
         BAS   RE,ADDCOMMA                                                      
         B     PRTS130                                                          
*                                                                               
PRTS110  C     RF,FULL             TEST ANY PRINT THIS LINE                     
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     RF,FULL             START OF PRESENT LINE                        
         LA    RF,100(RF)          POINT BEYOND EOL                             
*                                                                               
PRTS112  CLI   0(RF),C','          AND SEE IF LAST CHAR IS A COMMA              
         BE    PRTS114                                                          
         CLI   0(RF),C' '                                                       
         BH    PRTS120                                                          
         BCT   RF,PRTS112                                                       
*                                                                               
PRTS114  MVI   0(RF),C' '                                                       
*                                                                               
PRTS120  L     RF,FULL             NEXT PRINT LINE                              
         LA    RF,132(RF)                                                       
         ST    RF,FULL                                                          
         LA    R0,99(RF)           SET EOL ADDRESS                              
         B     PRTS60                                                           
*                                                                               
PRTS130  LA    R4,SVSHPNXT                                                      
         CLI   0(R4),0             TEST EOT                                     
         BE    PRTS140                                                          
         CLI   SVSHPSHP,0          TEST TO SHIP                                 
         BNE   PRTS60              YES - GO PROCESS                             
         B     PRTS130             NO - TRY AGAIN                               
*                                                                               
ADDCOMMA CLI   0(RF),C' '          PUT A COMMA AFTER LAST CHAR                  
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         MVI   1(RF),C','                                                       
         LA    RF,3(RF)            SET NEXT PRINT POSN                          
         BR    RE                                                               
         DROP  R4                                                               
                                                                                
*==============================================================                 
* COUNT AND PRINT THE SHIPPING LIST ENTRIES                                     
*==============================================================                 
                                                                                
PRTS140  L     RF,FULL             NEED TO GET RID OF LAST COMMA                
         LA    RF,100(RF)          POINT BEYOND EOL                             
*                                                                               
PRTS142  CLI   0(RF),C','          AND SEE IF LAST CHAR IS A COMMA              
         BE    PRTS144                                                          
         CLI   0(RF),C' '                                                       
         BH    PRTS146                                                          
         BCT   RF,PRTS142                                                       
*                                                                               
PRTS144  MVI   0(RF),C' '                                                       
*                                                                               
PRTS146  SR    R0,R0                                                            
         L     R4,ACMLEXP                                                       
*                                                                               
PRTS148  CLC   0(132,R4),SPACES                                                 
         BNH   *+12                                                             
         LA    R4,132(R4)                                                       
         BCT   R0,PRTS148                                                       
*                                                                               
         LPR   R0,R0                                                            
         STC   R0,ALLOWLIN                                                      
         L     R1,ACMLEXP                                                       
         BRAS  RE,PRTBUFF                                                       
*                                                                               
PRTSX    J     EXIT                                                             
         EJECT                                                                  
*====================================================================           
* READ CML RECORD TO GET CLIENT COMMERCIAL NUMBER                               
*====================================================================           
                                                                                
         USING SHPRD,R2                                                         
GCMLNUM  NTR1                                                                   
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0A21'                                                  
         MVC   KEY+2(3),BAGYMD     A-M/CLT                                      
         MVC   KEY+5(8),DUB                                                     
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    GCMLN2                                                           
*                                                                               
         MVC   KEY,KEYSAVE         RESTORE KEY                                  
         MVC   KEY(2),=X'0AC1'     TRY FOR ADID                                 
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
GCMLN2   L     R6,AIO3             USE IO3 FOR COMMERCIALS                      
         ST    R6,AIO                                                           
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING CMLDTAEL,R6                                                      
         MVC   SHPRCCN,CMLCLTNO    SAVE CLIENT CML NUMBER                       
*                                                                               
GCMLN10  XIT1                                                                   
         LTORG                                                                  
         DROP  R2,R6                                                            
         LTORG                                                                  
         EJECT                                                                  
*==============================================================                 
* GENERATE COVER LETTER REQUEST                                                 
*==============================================================                 
                                                                                
         USING SPOOLD,R8                                                        
REQ      NTR1 BASE=*,LABEL=*                                                    
         USING GEND,RC                                                          
*                                                                               
         L     R3,ATWA                                                          
         USING T216FFD,R3                                                       
*                                                                               
         LA    RF,TRAFAXH                                                       
         CLI   8(RF),C'N'          IF NOT FAXING                                
         BE    REQX                THEN DONE                                    
*                                                                               
         CLI   COVGENSW,C'Y'       TEST COV GEN REQ GENERATED YET               
         BE    REQX                                                             
*                                                                               
         TM    SVOPT3,OPT3REPT     THIS A REPRINT                               
         BO    REQX                 YES                                         
*                                                                               
         CLI   SVTWPR2,C'C'        COV GEN IS SET ON?                           
         BNE   REQX                                                             
*                                                                               
         MVI   COVGENSW,C'Y'       SET REQUEST DONE                             
         XC    REQHDR,REQHDR                                                    
         MVI   REQHDR+15,X'11'     NUMBER OF REQUESTS                           
         MVC   REQUEST,SPACES                                                   
         MVC   REQUEST(2),=C'TV'                                                
         MVC   REQUEST+2(2),AGENCY                                              
*                                                                               
         L     R1,ACOMFACS                                                      
         L     RF,CGETFACT-COMFACSD(R1)                                         
         GOTO1 (RF),DMCB,0                                                      
         L     RE,DMCB             FAFACTS                                      
         LA    R2,FASIN-FACTSD(,RE)                                             
         LA    R4,REQUEST+5                                                     
*                                                                               
         EDIT  (B4,0(R2)),(6,(R4)),FILL=0                                       
         MVC   REQUEST+12(26),=CL27'0203COV 0303GEN 0506OV,T/A'                 
         MVC   REQUEST+39(4),=CL4'0901'                                         
         MVC   REQUEST+43(1),QMED                                               
         MVC   REQUEST+45(4),=CL4'1003'                                         
         MVC   REQUEST+49(3),QCLT                                               
         MVC   REQUEST+53(4),=CL4'1103'                                         
         MVC   REQUEST+57(3),QPRD                                               
         LA    R1,REQUEST+61                                                    
*                                                                               
         CLI   SVPOLQ,0            NO PARTNER FOR POL PRDLIST                   
         BNE   REQ10                                                            
         CLI   BPRD2,X'FF'         IS REQUEST NONE FOR PARTNER                  
         BNE   REQ10                                                            
         MVC   0(8,R1),=C'1204NONE'                                             
         B     REQ20                                                            
REQ10    OC    QPRD2,QPRD2         IS PARTNER PRD PRESENT                       
         BZ    REQ20                NO                                          
         MVC   0(4,R1),=CL4'1203'                                               
         MVC   4(3,R1),QPRD2                                                    
*                                                                               
REQ20    MVC   REQUEST2(12),REQUEST                                             
         MVC   REQUEST2+12(68),SPACES                                           
         LA    R1,REQUEST2+12                                                   
         CLI   QBEST,0             IS ESTIMATE PRESENT                          
         BE    REQ30                NO                                          
         MVC   0(4,R1),=CL4'1303'                                               
         LLC   R0,QBEST                                                         
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  4(3,R1),DUB                                                      
         LA    R1,8(,R1)                                                        
         B     REQ35                                                            
REQ30    MVC   0(6,R1),=CL6'1302NO'                                             
         LA    R1,7(,R1)                                                        
*                                                                               
REQ35    MVC   0(2,R1),=CL2'14'                                                 
         LLC   RF,TRAPERH+5                                                     
         CVD   RF,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  2(2,R1),DUB                                                      
         BCTR  RF,0                                                             
         LA    RE,TRAPER                                                        
         EX    RF,REQMVC                                                        
         LA    R1,6(RF,R1)                                                      
*                                                                               
         MVC   0(2,R1),=CL2'16'                                                 
         LLC   RF,TRACONTH+5       CONTACT                                      
         CVD   RF,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  2(2,R1),DUB                                                      
         BCTR  RF,0                                                             
         LA    RE,TRACONT                                                       
         EX    RF,REQMVC                                                        
         LA    R1,6(RF,R1)                                                      
         TM    SVOPT2,OP2SPCMT     SPECIAL COMMENTS                             
         BZ    REQ50                NO                                          
         MVC   0(8,R1),=CL8'1805CMT='                                           
         MVC   8(1,R1),SVPROF16                                                 
         LA    R1,10(,R1)                                                       
REQ50    BCTR  R1,0                                                             
         MVI   0(R1),C'*'          END REQUEST                                  
*                                                                               
         XC    WORK,WORK                                                        
         GOTO1 REQTWA,DMCB,(5,ATWA),REQHDR,DATAMGR,RCCOMFAC,WORK                
         CLI   DMCB+8,0                                                         
         BE    REQX                                                             
         DC    H'0'                                                             
REQX     XIT1                                                                   
*                                                                               
REQMVC   MVC   4(0,R1),0(RE)                                                    
         DROP  R3                                                               
         LTORG                                                                  
         EJECT                                                                  
* GENERATE AUTO SHIPPING REQ *                                                  
*                                                                               
ASQ      NTR1 BASE=*,LABEL=*                                                    
*                                                                               
         TM    AUTOQSW,X'80'       TEST AUTO SHIPPING GENERATED YET             
         BO    ASQX                                                             
*                                                                               
         TM    SVOPT3,OPT3REPT     THIS A REPRINT                               
         BO    ASQX                 YES                                         
         TM    SVOPT,OPTRERUN      THIS A RERUN?                                
         BZ    ASQ04                NO                                          
         TM    UPDSW,UPDSHPGN      IS THERE A GENUINE SHIP REQUEST              
         BZ    ASQX                 NO                                          
*                                                                               
ASQ04    CLI   SVPROF2,C'Y'        SHIP BY PROD                                 
         BE    ASQ06                                                            
         CLI   SVPROF2,C'A'        SHIP BY PROD (ALL)                           
         BNE   ASQ20                                                            
ASQ06    L     RE,ASVPRDS                                                       
         USING SVPRDD,RE                                                        
ASQ10    CLI   SVPRDCD,0           END OF TABLE                                 
         BE    ASQX                                                             
         TM    SVPRDSHP,X'80'      THIS PRODUCT NEED TO BE SHIPPED              
         BZ    ASQ12                NO, TRY NEXT PRODUCT                        
         TM    SVPRDSHP,X'40'      HAS THIS PRODUCT BEEN SHIPPED                
         BZ    ASQ14                NO, GO GEN SHIP REQUEST                     
ASQ12    LA    RE,L'SVPRDDTA(,RE)                                               
         B     ASQ10                                                            
ASQ14    OI    SVPRDSHP,X'40'                                                   
         B     ASQ24                NO, GO GEN SHIP REQUEST                     
         DROP  RE                                                               
*                                                                               
* GENERATE AUTO SHIPPING REQ *                                                  
*                                                                               
ASQ20    OI    AUTOQSW,X'80'       SET FLAG                                     
*                                                                               
ASQ24    CLI   OFFLINE,C'Y'                                                     
         BNE   ASQ30                                                            
*                                                                               
         TM    WHEN,X'20'          THIS A SOON REQUEST                          
         BO    ASQ30                YES, GENERATE REQUEST AS IF ONLINE          
*                                                                               
         MVC   ELEM,SPACES                                                      
         LA    R2,ELEM                                                          
         USING XRECD,R2                                                         
*                                                                               
         MVI   XTYPE,C'S'                                                       
         MVC   XQUESTOR,SVQUESTR                                                
         MVC   XWHEN,QUWHEN                                                     
         MVC   XMED,QMED                                                        
         MVC   XCLT,QCLT                                                        
         CLI   SVPROF2,C'Y'        SHIP BY PROD                                 
         BE    ASQ25                                                            
         CLI   SVPROF2,C'A'        SHIP BY PROD (ALL)                           
         BNE   ASQ26                                                            
ASQ25    MVC   XPRD(3),SVPRDEBC-SVPRDD(RE)                                      
*                                                                               
ASQ26    L     R1,ATR04FIL                                                      
         LA    R0,ELEM                                                          
         PUT   (1),(0)                                                          
         B     ASQ50                                                            
*                                                                               
ASQX     XIT1                                                                   
*                                                                               
* GENERATE REQUEST FOR OVERNIGHT PROCESSING *                                   
*                                                                               
*              1ST 26 BYTES = REQHDR, NEXT 80 = REQUEST                         
*                                                                               
ASQ30    XC    ELEM(26),ELEM                                                    
         MVC   ELEM+26(80),SPACES                                               
         MVC   ELEM+26(2),=C'TO'                                                
         MVC   ELEM+26+2(2),AGENCY                                              
         MVC   ELEM+26+4(22),=CL22'*.SHIP.GEN..OV,T/A....'                      
         MVC   ELEM+26+26(1),QMED                                               
         MVI   ELEM+26+27,C'.'                                                  
         MVC   ELEM+26+28(3),QCLT                                               
         MVI   ELEM+26+31,C'.'                                                  
         LA    R4,ELEM+58                                                       
         CLI   SVPROF2,C'Y'        SHIP BY PROD                                 
         BE    ASQ31                                                            
         CLI   SVPROF2,C'A'        SHIP BY PROD (ALL)                           
         BNE   ASQ32                                                            
ASQ31    MVC   0(3,R4),SVPRDEBC-SVPRDD(RE)                                      
         LA    R4,3(,R4)                                                        
ASQ32    MVI   0(R4),C'.'                                                       
         MVI   1(R4),C'.'                                                       
         MVC   2(L'SVQUESTR-1,R4),SVQUESTR                                      
         LA    R4,L'SVQUESTR(,R4)                                               
*                                                                               
         LR    RE,R4               BLANK OUT ANY PERIODS                        
         LA    RF,L'SVQUESTR-1                                                  
         CLI   0(RE),C'.'                                                       
         BNE   *+8                                                              
         MVI   0(RE),C' '                                                       
         BCTR  RE,0                                                             
         BCT   RF,*-14                                                          
*                                                                               
ASQ34    CLI   0(R4),C' '                                                       
         BH    ASQ36                                                            
         MVI   0(R4),C' '                                                       
         BCT   R4,ASQ34                                                         
ASQ36    MVC   1(5,R4),=C'..O/A'                                                
         LA    R4,6(,R4)        ADD 1 FOR REQ, 2 FOR PERIOD, 3 FOR T/A          
*                                                                               
         TM    SVOPT,OPTTEST       TEST RUN                                     
         BZ    ASQ40                                                            
         MVC   0(5,R4),=C',TEST'                                                
         LA    R4,5(,R4)                                                        
ASQ40    MVC   0(2,R4),=C'.*'                                                   
         L     R1,ATWA                                                          
         MVC   ELEM+11(2),17(R1)                                                
         XC    WORK,WORK                                                        
         GOTO1 DATAMGR,DMCB,=C'DMADD',=C'REQUEST',WORK,ELEM                     
         CLI   DMCB+8,0                                                         
         BE    ASQ50                                                            
         DC    H'0'                                                             
ASQ50    CLI   SVPROF2,C'A'        SHIP BY PROD (ALL)                           
         BE    ASQ06                                                            
         CLI   SVPROF2,C'Y'        SHIP BY PROD                                 
         BE    ASQ06                                                            
         B     ASQX                                                             
         LTORG                                                                  
         DROP  R2                                                               
         EJECT                                                                  
*==============================================================                 
* GENERATE SPOT SEED REQ                                                        
*==============================================================                 
                                                                                
ASD      NTR1 BASE=*,LABEL=*                                                    
*                                                                               
         L     RC,SVADGEND                                                      
         USING GEND,RC                                                          
*                                                                               
         TM    SVOPT3,OPT3REPT     THIS A REPRINT                               
         BO    ASDX                 YES                                         
*                                                                               
         CLI   SEEDFLG,C'Y'        WAS SEED REQ GENERATED FOR THIS              
         BE    ASDX                 YES, DONE                                   
*                                                                               
         CLI   OFFLINE,C'Y'                                                     
         BNE   ASD30                                                            
*                                                                               
         TM    WHEN,X'20'          THIS A SOON REQUEST                          
         BO    ASD30                YES, GENERATE REQUEST AS IF ONLINE          
*                                                                               
         MVC   ELEM,SPACES                                                      
         LA    R2,ELEM                                                          
         USING XRECD,R2                                                         
*                                                                               
         MVI   XTYPE,C'D'                                                       
         MVC   XQUESTOR,SVQUESTR                                                
         MVC   XWHEN,QUWHEN                                                     
         MVC   XMED,QMED                                                        
         MVC   XCLT,QCLT                                                        
*                                                                               
         MVC   XPRD(3),=C'ALL'                                                  
*                                                                               
         CLI   BPRD,X'FF'          ALL PRODUCTS?                                
         BE    *+10                 YES                                         
         MVC   XPRD(3),QPRD        PUT IN SPECIFIC                              
*                                                                               
*        LLC   R0,SPTSEDLN                                                      
         CLI   SVOPTLEN,0          IS THERE A LENGTH                            
         BE    ASD04                NO                                          
*                                                                               
         LLC   R0,SVOPTLEN                                                      
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  XPRD+3(3),DUB                                                    
*                                                                               
ASD04    DS    0H                                                               
         CLC   QBEST,QBESTEND      IF ESTIMATE RANGE, BYPASS                    
         BNE   ASD10                                                            
*                                                                               
         MVC   XCOPY,QBEST                                                      
*                                                                               
ASD10    DS    0H                                                               
         GOTO1 DATCON,DMCB,(2,SVGENST),XFLTST                                   
         GOTO1 (RF),(R1),(2,SVGENEND),XFLTEND                                   
*                                                                               
         TM    SVOPT,OPTTEST       TEST RUN                                     
         BZ    ASD20                                                            
*                                                                               
         MVI   XTEST,C'Y'                                                       
*                                                                               
ASD20    DS    0H                                                               
         L     R1,ATR04FIL                                                      
         LA    R0,ELEM                                                          
         PUT   (1),(0)                                                          
*                                                                               
ASDX     XIT1                                                                   
*                                                                               
* GENERATE REQUEST FOR OVERNIGHT PROCESSING *                                   
*                                                                               
*              1ST 26 BYTES = REQHDR, NEXT 80 = REQUEST                         
*                                                                               
ASD30    XC    ELEM(26),ELEM                                                    
         MVC   ELEM+26(80),SPACES                                               
         MVC   ELEM+26(2),=C'TS'                                                
         MVC   ELEM+26+2(2),AGENCY                                              
         MVC   ELEM+26+4(23),=CL23'*.SPOT.SEED..OV,T/A....'                     
         MVC   ELEM+26+27(1),QMED                                               
         MVI   ELEM+26+28,C'.'                                                  
         MVC   ELEM+26+29(3),QCLT                                               
         MVI   ELEM+26+32,C'.'                                                  
         LA    R4,ELEM+59                                                       
*                                                                               
         MVC   0(3,R4),=C'ALL'                                                  
*                                                                               
         CLI   BPRD,X'FF'          ALL PRODUCTS?                                
         BE    *+10                 YES                                         
         MVC   0(3,R4),QPRD        PUT IN SPECIFIC                              
*                                                                               
         LA    R4,3(,R4)                                                        
*                                                                               
         CLI   SVOPTLEN,0          WAS SPECIFIV LEN ENTERED                     
         BE    ASD34                NO                                          
*                                                                               
         MVI   0(R4),C'-'                                                       
*        LLC   R0,SPTSEDLN                                                      
         LLC   R0,SVOPTLEN                                                      
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  1(3,R4),DUB                                                      
         LA    R4,4(,R4)                                                        
*                                                                               
ASD34    DS    0H                                                               
         MVC   0(3,R4),=C'...'     BYPASS PTR AND MARKET                        
         LA    R4,3(,R4)                                                        
*                                                                               
         CLI   QBEST,0             THIS RUN BY ESTIMATE                         
         BE    ASD36                NO                                          
*                                                                               
         LLC   R0,QBEST                                                         
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  0(3,R4),DUB                                                      
         LA    R4,3(,R4)                                                        
*                                                                               
ASD36    DS    0H                                                               
         MVI   0(R4),C'.'                                                       
         LA    R4,1(,R4)                                                        
*                                                                               
         GOTO1 DATCON,DMCB,(2,SVGENST),(5,0(R4))                                
         MVI   8(R4),C'-'                                                       
         GOTO1 (RF),(R1),(2,SVGENEND),(5,9(R4))                                 
*                                                                               
         LA    R4,17(,R4)                                                       
*                                                                               
         TM    SVOPT,OPTTEST       TEST RUN                                     
         BZ    ASD40                                                            
*                                                                               
         MVI   0(R4),C'.'                                                       
         LA    R4,1(,R4)                                                        
         MVC   0(4,R4),=C'TEST'                                                 
         LA    R4,4(,R4)                                                        
ASD40    MVI   0(R4),C'.'                                                       
         MVI   1(R4),C'*'                                                       
         L     R1,ATWA                                                          
         MVC   ELEM+11(2),17(R1)                                                
         XC    WORK,WORK                                                        
         GOTO1 DATAMGR,DMCB,=C'DMADD',=C'REQUEST',WORK,ELEM                     
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVI   SEEDFLG,C'Y'        SEED REQ GENERATED FOR THIS                  
         B     ASDX                                                             
         LTORG                                                                  
         DROP  R2,R7                                                            
         EJECT                                                                  
*================================================================               
* FIND, SAVE, AND PRINT ANY PATTERN SPECIAL TEXT                                
*================================================================               
                                                                                
         USING SVTABLED,R7                                                      
                                                                                
         USING PTNLISTD,R3                                                      
PSTX     NTR1 BASE=*,LABEL=*                                                    
         MVI   SPACING,1                                                        
*                                                                               
         L     R7,ASVTABLE                                                      
         L     R4,AIO1                                                          
         XC    0(256,R4),0(R4)                                                  
*                                                                               
PSTX10   TM    SVTBIND,X'01'       TEST NO PATTERN ERROR                        
         BO    PSTX40                                                           
         L     R3,SVTBAPTN                                                      
*                                                                               
PSTX20   OC    PTNLTXT,PTNLTXT     ANY SPECIAL TEXT FOR PATTERN                 
         BZ    PSTX36                                                           
         LA    R0,42                                                            
         L     R4,AIO1                                                          
PSTX30   OC    0(6,R4),0(R4)       EMPTY ENTRY                                  
         BZ    PSTX34                                                           
         CLC   PTNLTXT,0(R4)                                                    
         BE    PSTX36                                                           
         LA    R4,6(,R4)                                                        
         BCT   R0,PSTX30                                                        
         DC    H'0'                                                             
PSTX34   MVC   0(6,R4),PTNLTXT                                                  
*                                                                               
PSTX36   LA    R3,L'PTNLIST(R3)    NEXT PATTERN                                 
*                                                                               
         CLI   0(R3),0             ANY MORE                                     
         BNE   PSTX20               YES - GO PROCESS                            
*                                                                               
PSTX40   OC    SVTBNXLN,SVTBNXLN   ANY MORE                                     
         BZ    PSTX44                                                           
         LA    R7,SVTBNEXT         NEXT SVTABLE ENTRY                           
         B     PSTX10                                                           
*                                                                               
PSTX44   L     R4,AIO1                                                          
         OC    0(6,R4),0(R4)       EMPTY ENTRY                                  
         BZ    PSTX80                                                           
*                                                                               
         MVI   HEADSW,0            SET FIRST TIME FLAG                          
         L     R4,AIO1                                                          
*                                                                               
PSTX50   XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0A2D'                                                  
         MVC   KEY+2(3),BAGYMD     A-M/CLT                                      
         MVI   KEY+5,C'-'                                                       
         MVC   KEY+6(6),0(R4)                                                   
         MVI   KEY+12,C'L'                                                      
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   PSTX70                                                           
*                                                                               
         CLI   HEADSW,C'T'         ONE TIME ONLY, TRY TO FORCE HEADS            
         BE    PSTX51                                                           
         MVI   HEADSW,C'T'         SET TO SUPPRESS MIDLINES                     
         MVI   FORCEHED,C'Y'                                                    
         MVI   P,0                                                              
         BRAS  RE,GOSPL                                                         
*                                                                               
PSTX51   MVC   P+25(23),=C'** SPECIAL COMMENTS FOR'                             
         L     R7,ASVTABLE                                                      
*                                                                               
PSTX52   TM    SVTBIND,X'01'       TEST NO PATTERN ERROR                        
         BO    PSTX60                                                           
         L     R3,SVTBAPTN                                                      
*                                                                               
PSTX54   CLC   0(6,R4),PTNLTXT     TEST RIGHT ENTRY                             
         BNE   PSTX58                                                           
*                                                                               
         LA    R1,SVTBPRD                                                       
         BRAS  RE,GPRDS                                                         
         MVC   P+53(3),1(RF)                                                    
         MVC   P+58(20),4(RF)                                                   
*                                                                               
         GOTO1 DATCON,DMCB,(2,PTNLSTR),(8,P+80)                                 
         MVI   P+88,C'-'                                                        
         GOTO1 (RF),(R1),(2,PTNLEND),(8,P+89)                                   
*                                                                               
         LA    R1,SVTBPRD2                                                      
         CLI   0(R1),0                                                          
         BE    PSTX56                                                           
         BAS   RE,GPRDS                                                         
         MVC   P+53+132(3),1(RF)                                                
         MVC   P+58+132(20),4(RF)                                               
*                                                                               
PSTX56   MVI   SPACING,2                                                        
         BRAS  RE,GOSPL              GOTO SPOOL RTN                             
         MVI   SPACING,1                                                        
*                                                                               
PSTX58   LA    R3,L'PTNLIST(R3)    NEXT PATTERN                                 
*                                                                               
         CLI   0(R3),0             ANY MORE                                     
         BNE   PSTX54               YES - GO PROCESS                            
*                                                                               
PSTX60   OC    SVTBNXLN,SVTBNXLN    TEST ANY MORE                               
         BZ    PSTX62                                                           
         LA    R7,SVTBNEXT          NEXT SVTABLE ENTRY                          
         B     PSTX52                                                           
*                                                                               
PSTX62   L     R6,AIO3                                                          
         ST    R6,AIO                                                           
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         MVI   ELCODE,X'40'                                                     
         BRAS  RE,GETEL                                                         
         B     *+8                                                              
*                                                                               
PSTX64   BRAS  RE,NEXTEL                                                        
         BNE   PSTX66                                                           
         LLC   RE,1(R6)                                                         
         AHI   RE,-4                                                            
         EX    RE,PSTMVC           MOVE COMMENT TO PRINT                        
         BRAS  RE,GOSPL            GOTO SPOOL RTN                               
         B     PSTX64                                                           
PSTMVC   MVC   P+25(0),3(R6)                                                    
*                                                                               
PSTX66   LLC   R1,KEY+12           BUMP TYP (PAGE NUMBER)                       
         LA    R1,1(,R1)                                                        
         STC   R1,KEY+12                                                        
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    PSTX62                                                           
*                                                                               
PSTX70   LA    R4,6(R4)            NEXT STEXT KEY                               
         CLI   0(R4),0                                                          
         BE    PSTX80                                                           
         MVI   P,0                                                              
         BRAS  RE,GOSPL              GOTO SPOOL RTN                             
         B     PSTX50                                                           
*==============================================                                 
* PRINT END OF INSTRUCTIONS TEXT                                                
*==============================================                                 
                                                                                
PSTX80   XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0A2D'                                                  
         MVC   KEY+2(1),BAGYMD                                                  
         MVI   KEY+3,C'*'                                                       
         MVC   KEY+4(1),SVCLTOFF                                                
         MVI   KEY+12,C'L'                                                      
*                                                                               
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(13),KEYSAVE                                                  
         BE    PSTX90                                                           
*                                                                               
         MVC   KEY,KEYSAVE                                                      
         XC    KEY+3(2),KEY+3                                                   
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(13),KEYSAVE                                                  
         BNE   PSTXX                                                            
*                                                                               
PSTX90   L     R6,AIO3                                                          
         ST    R6,AIO                                                           
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
                                                                                
* SEE HOW MANY LINES WE NEED FOR COMMENTS *                                     
                                                                                
         SR    R5,R5               CLEAR COUNTER                                
         L     R6,AIO3                                                          
         MVI   ELCODE,X'40'                                                     
*                                                                               
         BRAS  RE,GETEL                                                         
         BNE   PSTX96                                                           
*                                                                               
         AHI   R5,1                                                             
         BRAS  RE,NEXTEL                                                        
         BE    *-8                                                              
*                                                                               
         MVI   SPACING,1                                                        
         LLC   R0,LINE                                                          
         LLC   RE,FOOTLNS                                                       
         AR    R0,RE                                                            
         AR    R0,R5                                                            
         CLM   R0,1,MAXLINES                                                    
         BL    *+8                                                              
         MVI   FORCEHED,C'Y'       FORCE NEW PAGE                               
*                                                                               
         L     R6,AIO3                                                          
         BRAS  RE,GETEL                                                         
         B     *+8                                                              
*                                                                               
PSTX94   BRAS  RE,NEXTEL                                                        
         BNE   PSTX96                                                           
         LLC   RE,1(R6)                                                         
         AHI   RE,-4                                                            
         EX    RE,PSTMVC                                                        
         BRAS  RE,GOSPL              GOTO SPOOL RTN                             
         B     PSTX94                                                           
*                                                                               
PSTX96   DS   0H                                                                
         LLC   R1,KEY+12           BUMP TYP (PAGE NUMBER)                       
         LA    R1,1(,R1)                                                        
         STC   R1,KEY+12                                                        
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    PSTX90                                                           
*                                                                               
PSTXX    DS    0H                                                               
         MVI   FORCEHED,C'N'       RESET IN CASE NOTHING PRINTED                
         XIT1                                                                   
         LTORG                                                                  
                                                                                
*==================================================================             
* SUBROUTINE TO EXTRACT PRODUCT CODES/NAMES FROM SAVE AREA                      
*==================================================================             
                                                                                
GPRDS    L     RF,ASVPRDS                                                       
         USING SVPRDD,RF                                                        
*                                                                               
GPRDS10  CLC   0(1,RF),0(R1)                                                    
         BER   RE                                                               
         LA    RF,L'SVPRDDTA(RF)                                                
         CLI   0(RF),0                                                          
         JNE   GPRDS10                                                          
*                                                                               
         L     RE,ATWA                                                          
         AHI   RE,CONHEADH-T216FFD   POINT TO MESSAGE FIELD                     
         XC    8(60,RE),8(RE)                                                   
         LARL  RF,MAXPRDER                                                      
         MVC   8(L'MAXPRDER,RE),0(RF)   REMEMBER NO BASE REG!                   
         OI    6(RE),X'80'                                                      
         DC    H'0',C'$ABEND'                                                   
MAXPRDER DC    C'* ERROR * TOO MANY PRDS. RUN SOON REQUEST'                     
         DROP  RF                                                               
         EJECT                                                                  
GOSPL    NTR1  BASE=*,LABEL=*                                                   
         USING GEND,RC                                                          
*                                                                               
         TM    SVTBIND2,SVTBICGR   THIS CABLE STATION WITH GROUP CODE           
         BO    GOSPL10                                                          
*                                                                               
         CLI   SVT1PR9,C'Y'         IF COVER LETTER INSTR, NO PRINT             
         BE    GOSPL10                                                          
*                                                                               
         L     RE,APRTSAVE         SAVE THE PRINT LINES                         
         LA    RF,528                                                           
         LA    R0,P                                                             
         LA    R1,528                                                           
         MVCL  RE,R0                                                            
*                                                                               
         LLC   R0,SPACING                                                       
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)     PRINT THE LINES                              
*                                                                               
         STC   R0,SPACING          RESTORE ORIGINAL SPACING                     
         L     R3,APRTSAVE         POINT TO SAVED PRINT LINES                   
         LA    R4,4                                                             
         BRAS  RE,GOTSR                                                         
         J     GOSPLX                                                           
*                                                                               
GOSPL10  LA    R0,4                                                             
         LA    R1,P                                                             
         MVC   0(132,R1),SPACES                                                 
         LA    R1,132(R1)                                                       
         BCT   R0,*-10                                                          
         B     GOSPLX                                                           
*                                                                               
GOSPLX   XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*===================================================================            
* HEADHOOK ROUTINE                                                              
* NOTE THAT THIS ROUTINE USES IO1+4000 AS A SAVE AREA FOR IO1                   
*===================================================================            
                                                                                
HDHK     NTR1 BASE=*,LABEL=*                                                    
         MVI   HEADFLAG,C'H'       SET FLAG FOR GOTSR                           
*                                                                               
         L     R0,AIO1                                                          
         AHI   R0,4000             MOVE TO END OF AIO1                          
         L     RE,AIO1             FROM AIO1                                    
         LA    RF,2000             LEN=2000                                     
         LR    R1,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         LLC   R2,SPACING                                                       
*                                                                               
         CLI   OFFLINE,C'Y'                                                     
         BNE   HDHK06                                                           
         LH    RE,SEQNUM                                                        
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  H1+104(4),DUB                                                    
         LA    RE,1(RE)                                                         
         STH   RE,SEQNUM                                                        
*                                                                               
HDHK06   TM    SVOPT2,OP2NOAGY    BLANK AGENCY NAME/ADDRESS                     
         BZ    *+16                                                             
         MVC   H1+2(33),SPACES                                                  
         MVC   H1+2(33),SPACES                                                  
*                                                                               
         MVC   H1+38(40),SPACES                                                 
         MVC   H1+39(10),=C'SPOT RADIO'                                         
         CLI   QMED,C'R'                                                        
         BE    HDHK08                                                           
         MVC   H1+38(13),=C'NETWORK RADIO'                                      
         CLI   QMED,C'X'                                                        
         BE    HDHK08                                                           
         MVC   H1+38(15),=C'SPOT TELEVISION'                                    
         CLI   QMED,C'T'                                                        
         BE    HDHK08                                                           
         MVC   H1+36(18),=C'NETWORK TELEVISION'                                 
         CLI   QMED,C'N'                                                        
         BE    HDHK08                                                           
         DC    H'0'                                                             
HDHK08   MVC   H1+55(23),=C'COMMERCIAL INSTRUCTIONS'                            
         GOTO1 SQUASHER,DMCB,H1+38,40                                           
         GOTO1 CENTER,(R1),,40                                                  
         GOTO1 UNDERLIN,(R1),(40,H1+38),H2+38                                   
*                                                                               
         OC    SVINSDT,SVINSDT     TEST PREVIOUS INST                           
         BZ    HDHK10                                                           
         GOTO1 DATCON,(R1),(2,SVINSDT),(8,H4+67)                                
*                                                                               
HDHK10   MVC   H3+99(5),SVTBSTA    CREATE PURCHASE ORDER NUM                    
         GOTO1 DATCON,(R1),(5,0),WORK                                           
         MVC   H3+104(4),WORK+2    MOVE MMDD                                    
*                                                                               
         CLI   SVT1PR5,C'B'        TEST SUPPRESS PERIOD                         
         BNE   *+10                                                             
         MVC   H3+45(26),SPACES                                                 
*                                                                               
         CLI   SVPROF4,C'N'        TEST SUPPRESS PURCHASE ORDER                 
         BNE   *+10                                                             
         MVC   H3+84(26),SPACES                                                 
*                                                                               
         LLC   RE,SVINSREV                                                      
         TM    SVOPT3,OPT3REPT     THIS A REPRINT                               
         BO    HDHK11                                                           
         TM    SVOPT,OPTRERUN                                                   
         BO    HDHK11                                                           
         LA    RE,1(RE)                                                         
HDHK11   DS   0H                                                                
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  H4+94(3),DUB                                                     
*                                                                               
         OC    SVINSDT,SVINSDT     TEST NO PREVIOUS INST                        
         BZ    HDHK12               THESE ARE ORIGINAL                          
         CLI   SVINSREV,0          TEST PREVIOUS INST WERE ORIGINAL             
         BNE   HDHK14               NO                                          
         TM    SVOPT3,OPT3REPT     THIS A REPRINT                               
         BO    HDHK13               THESE WERE ORIGINAL                         
         TM    SVOPT,OPTRERUN      TEST THIS IS A RERUN                         
         BZ    HDHK14               NO - TREAT THESE AS REVISIONS               
*                                                                               
HDHK12   MVC   H4+38(40),SPACES    OVERWRITE SUPERSEDES LINE                    
*                                                                               
HDHK13   MVC   H4+84(21),=C'ORIGINAL INSTRUCTIONS'                              
*                                                                               
HDHK14   DS    0H                                                               
         TM    RUNFLAG,RUNFLTBA    WERE ALL RECAPS TBA                          
         BZ    HDHK15                                                           
         CLI   SVT2PR10,C'Y'       NO SUPERSEDES LINE                           
         BNE   HDHK15                                                           
*                                                                               
         TM    SVOPT3,OPT3REPT     IF REPRINT                                   
         BZ    HDHK14F                                                          
         MVC   H4+40(10),=C'REPRINT OF'                                         
         GOTO1 DATCON,DMCB,(2,SVOPTDTE),(8,H4+51)                               
         GOTO1 CENTER,DMCB,H4+40,34                                             
         B     HDHK15                                                           
*                                                                               
HDHK14F  MVC   H4+38(40),SPACES    OVERWRITE SUPERSEDES LINE                    
*                                                                               
HDHK15   DS   0H                                                                
         TM    SVOPT,OPTTEST       TEST OPTION TEST                             
         BZ    *+10                                                             
         MVC   H5+50(16),=C'*** TEST RUN ***'                                   
*                                                                               
         TM    SVOPT3,OPT3REPT     IF REPRINT                                   
         BZ    HDHK15B                                                          
         MVC   H4+39(40),SPACES                                                 
         MVC   H4+43(28),=C'REPRINT OF LAST INSTRUCTIONS'                       
         BO    HDHK15D                                                          
                                                                                
HDHK15B  CLI   SVTWPR1,C'C'        OR IF FOLLOW UP TO FAX SENT                  
         BNE   HDHK15F                                                          
*                                                                               
*DHK15A  TM    UPDSW,UPDATE        AND 14 ELEMENT FOUND                         
*******  BO    HDHK15F             THEN USE SUPERSEDE MESSAGE                   
HDHK15D  OC    SVOPTDTE,SVOPTDTE                                                
         BZ    HDHK15F                                                          
         GOTO1 DATCON,DMCB,(2,SVOPTDTE),(8,H4+51)                               
         GOTO1 CENTER,DMCB,H4+40,34                                             
*                                                                               
HDHK15F  TM    SVOPT,OPTRERUN                                                   
         BZ    HDHK15G                                                          
         MVC   H4+40(40),SPACES                                                 
         MVC   H4+106(3),=C'(R)'                                                
*                                                                               
HDHK15G  TM    SVOPT,OPTREV        TEST REVISIONS ONLY                          
         BZ    *+10                                                             
         MVC   H4+106(3),=C'(V)'                                                
*                                                                               
         CLI   SVTWPR1,C'C'         TEST IF FOLLOW UP TO FAX SENT               
         BNE   HDHK16                                                           
         MVC   H4(17),=C'** AGENCY COPY **'                                     
*                                                                               
         CLI   SVPROF5,C'N'        TEST SUPPRESS REQUESTOR/REPORT               
         BNE   HDHK16                                                           
         MVC   H1+84(14),SPACES    REPORT (LEAVE SEQUENCE NUMBER)               
         MVC   H5+92(18),SPACES    REQUESTOR                                    
*                                                                               
HDHK16   CLC   PAGE,=H'1'                                                       
         BE    *+10                                                             
         MVC   H6+85(21),=C'***** CONTINUED *****'                              
*                                                                               
         MVC   H7+2(6),=C'CLIENT'                                               
         MVC   H7+11(3),QCLT                                                    
         MVC   H7+16(20),CLTNM                                                  
*                                                                               
         MVC   H8+2(7),=C'PRODUCT'                                              
*                                                                               
         L     RE,ASVPRDS                                                       
         USING SVPRDD,RE                                                        
*                                                                               
HDHK20   CLI   BPRD,X'FF'          TEST POL REQUEST                             
         BNE   HDHK22                                                           
*                                                                               
         MVC   H8+11(3),QPRD       POOL REQUEST                                 
         MVC   H8+16(20),PRDNM                                                  
         B     HDHK26                                                           
*                                                                               
HDHK22   CLC   SVPRDCD,SVTBPRD     NON-POL REQUEST                              
         BE    HDHK24                                                           
         LA    RE,L'SVPRDDTA(RE)                                                
         CLI   0(RE),0                                                          
         BNE   HDHK20                                                           
         DC    H'0'                                                             
*                                                                               
HDHK24   MVC   H8+11(3),SVPRDEBC                                                
         MVC   H8+16(20),SVPRDNM                                                
         DROP  RE                                                               
*                                                                               
HDHK26   MVC   H9+2(8),=C'ESTIMATE'                                             
         CLI   SVPROF11,C'E'       COPY CODE=ESTIMATE                           
         BNE   HDHK27                                                           
         EDIT (B1,QBEST),(3,H9+11),ALIGN=LEFT                                   
         CLI   SVT1PR14,C'Y'       PRINT EST HDR DESC                           
         BNE   *+10                                                             
         MVC   H9+16(20),QESTDESC                                               
         B     HDHK28                                                           
*                                                                               
HDHK27   MVC   H9+11(7),=C'VARIOUS'                                             
*                                                                               
HDHK28   MVC   H10+2(6),=C'MARKET'                                              
         MVC   H10+11(4),QMKT                                                   
         MVC   H10+16(24),MKTNM                                                 
*                                                                               
         LA    RE,H10+39           POINT TO LAST CHAR OF MKTNM                  
         CLI   0(RE),C' '                                                       
         BH    *+8                                                              
         BCT   RE,*-8                                                           
*                                                                               
         MVI   2(RE),C'-'                                                       
*                                                                               
         MVC   4(4,RE),SVTBSTA                                                  
         LA    RE,7(RE)            POINT TO LAST CALL LETTER                    
         CLI   0(RE),C' '                                                       
         BH    *+6                                                              
         BCTR  RE,0                                                             
         MVI   1(RE),C'-'                                                       
         MVC   2(1,RE),SVTBSTA+4                                                
         MVI   3(RE),C'V'                                                       
         CLI   SVTBSTA+4,C'T'                                                   
         BE    HDHK30                                                           
*MNMB                                                                           
         CLI   SVTBSTA+4,C'D'                                                   
         BE    HDHK30                                                           
*MNMB                                                                           
         MVI   3(RE),C'M'                                                       
*                                                                               
* FORMAT MARKET GROUP TO HEADLINES *                                            
*                                                                               
HDHK30   CLI   SVT2PR01,0          MARKET GROUP OPTION                          
         BE    HDHK36               NO                                          
         CLI   SVT2PR01,C'0'       MARKET GROUP OPTION                          
         BE    HDHK36               NO                                          
*                                                                               
         CLI   SVT2PR01,C'*'       MORE THAN ONE MARKET GROUP                   
         BNE   *+12                 NO                                          
         CLI   SVT2PR14,X'FF'      CONVERTED MKT GROUP BJ = '*'                 
         BNE   HDHK36               NO, ITS A MULTY MGROUP                      
*                                                                               
         MVC   H11+2(12),=C'MARKET GROUP'                                       
         MVC   ACLWORK,KEY                                                      
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D03'                                                  
         MVC   KEY+8(3),BAGYMD                                                  
         MVC   KEY+11(2),SVTBMKST                                               
         MVI   RDUPDATE,C'N'                                                    
         MVC   FILENAME,=CL8'SPTDIR' SWITCH TO SPOT                             
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    HDHK32                                                           
*                                                                               
         MVC   KEY(13),KEYSAVE                                                  
         XC    KEY+9(2),KEY+9      TRY NON-CLIENT SPECIFIC                      
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    HDHK32                                                           
HDHK31   MVC   H11+15(7),=C'UNKNOWN'                                            
         B     HDHK35C                                                          
*                                                                               
HDHK32   L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         MVI   RDUPDATE,C'N'                                                    
         MVC   FILENAME,=CL8'SPTFIL' SWITCH TO SPOT                             
         GOTO1 GETREC                                                           
         LA    R6,24(,R6)                                                       
         CLI   0(R6),05                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
HDHK33   CLC   SVT2PR01,5(R6)      MARKET GROUP OPTION                          
         BE    HDHK34                                                           
         LLC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),05                                                         
         BE    HDHK33                                                           
         B     HDHK31                                                           
*                                                                               
HDHK34   DS    0H                  LOOK UP 1 CHAR CODE IN TABLE                 
         L     R1,=A(SPMGRTAB)                                                  
         A     R1,SPTR04RR                                                      
         LHI   RF,(SPMGRTBX-SPMGRTAB)/3                                         
*                                                                               
HDHK34C  CLC   5(1,R6),2(R1)      IS THIS IT                                    
         BE    HDHK34F                                                          
         LA    R1,3(R1)                                                         
         BCT   RF,HDHK34C                                                       
         DC    H'0'                SHOULD NOT BE (NOT IN TABLE)???              
*                                                                               
HDHK34F  DS    0H                                                               
         MVC   H11+15(2),0(R1)     MOVE 2 CHAR MKT GROUP                        
*                                                                               
         UNPK  DUB(5),6(3,R6)                                                   
         CLI   H11+16,X'40'        1 CHAR MKT GROUP ?                           
         BE    *+14                                                             
         MVC   H11+17(4),DUB                                                    
         B     *+10                                                             
         MVC   H11+16(4),DUB                                                    
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D02'                                                  
         MVC   KEY+2(3),KEYSAVE+8  MOVE IN BAGYMD/CLT (CLT MAY BE ZERO)         
         MVC   KEY+8(3),5(R6)                                                   
         MVC   FILENAME,=CL8'SPTDIR' SWITCH TO SPOT                             
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    HDHK35                                                           
         CLC   KEY(5),KEYSAVE                                                   
         BNE   HDHK35C                                                          
         OC    KEY+5(3),KEY+5      IS THIS A PRODUCT GROUP                      
         BZ    HDHK35C                                                          
         MVC   KEYSAVE+5(3),KEY+5                                               
         MVC   KEY(13),KEYSAVE                                                  
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   HDHK35C                                                          
*                                                                               
HDHK35   MVC   FILENAME,=CL8'SPTFIL' SWITCH TO SPOT                             
         GOTO1 GETREC                                                           
         L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         LA    R6,24(,R6)                                                       
HDHK35A  CLI   0(R6),X'10'                                                      
         BE    HDHK35B                                                          
         LLC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         BNE   HDHK35A                                                          
         B     HDHK35C                                                          
HDHK35B  MVC   H11+15(24),2(R6)                                                 
*                                                                               
HDHK35C  DS   0H                                                                
         XC    FILENAME,FILENAME                                                
         MVC   KEY,ACLWORK                                                      
*                                                                               
HDHK36   MVC   H7+50(7),=C'CONTACT'                                             
         MVC   H7+58(L'QUESTOR),QUESTOR                                         
*                                                                               
         OC    CONEMAIL,SPACES          FORCE NULLS TO SPACES                   
         CLC   CONEMAIL,SPACES          ANY E-MAIL                              
         BNH   *+10                      NO                                     
         MVC   H8+58(50),CONEMAIL                                               
*                                                                               
         MVC   H9+50(5),=C'PHONE'                                               
         MVC   H9+58(12),CONTEL                                                 
         OC    CONTEL+13(5),CONTEL+13   ANY EXTENSION                           
         BZ    HDHK37                    NO                                     
         MVC   H9+71(3),=C'EXT'                                                 
         MVC   H9+75(5),CONTEL+13                                               
*                                                                               
HDHK37   OC    CONFAX(18),CONFAX        ANY FAX                                 
         BZ    HDHK38                    NO                                     
         MVC   H10+50(3),=C'FAX'                                                
         MVC   H10+58(12),CONFAX                                                
         OC    CONFAX+13(5),CONFAX+13   ANY EXTENSION                           
         BZ    HDHK38                    NO                                     
         MVC   H10+71(3),=C'EXT'                                                
         MVC   H10+75(5),CONFAX+13                                              
*                                                                               
HDHK38   TM    UPDSW,UPDPGSW       IS THERE A PIGGYBACK PRODUCT                 
         BZ    *+10                 NO                                          
         MVC   H12+67(26),=C'*** PIGGYBACKS PRESENT ***'                        
*                                                                               
         LA    R3,H1                                                            
         LA    R4,13                                                            
         BRAS  RE,GOTSR                                                         
* NEED A BLANK LINE AFTER HEADS                                                 
         LA    R3,SPACES                                                        
         LA    R4,1                                                             
         BRAS  RE,GOTSR                                                         
                                                                                
* RESTORE RECORD TO AIO1                                                        
                                                                                
HDHKX    L     RE,AIO1                                                          
         AHI   RE,4000             MOVE FROM END OF AIO1                        
         L     R0,AIO1             TO AIO1                                      
         LA    RF,2000             LEN=2000                                     
         LR    R1,RF                                                            
         MVCL  R0,RE                                                            
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*=============================================================                  
* MIDLINE HOOK                                                                  
*=============================================================                  
                                                                                
MIDHK    NTR1  BASE=*,LABEL=*                                                   
         MVC   MID1,SPACES                                                      
         MVC   MID2,SPACES                                                      
*                                                                               
MIDHK2   CLI   HEADSW,C'A'         TEST PRINTING ADD'L DATA                     
         BE    MIDHK10                                                          
         CLI   HEADSW,C'T'         IF HEADSW = T, PRINTING PATTERN TEXT         
         BE    MIDHKX                                                           
         CLI   HEADSW,C'Y'                                                      
         BE    *+12                                                             
         CLI   HEADSW,C'C'                                                      
         BNE   MIDHKX                                                           
*                                                                               
         LA    RE,MIDLINEX-MIDLINE   SET LEN OF ENTIRE MIDLINE                  
         CLI   SVT3PROF+2,C'Y'       TEST PRINT CMML RUN TIMES                  
         BE    *+8                                                              
         LA    RE,MIDLINE1-MIDLINE   SET LEN WITHOUT RUN TIMES                  
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     MIDHK20                                                          
         MVC   MID1(0),MIDLINE                                                  
*                                                                               
MIDHK10  MVC   MID1(33),=C'ADDITIONAL COMMERCIAL INFORMATION'                   
         MVC   MID2(33),=C'---------------------------------'                   
         CLI   CONTINUE,C'Y'                                                    
         BE    MIDHK12                                                          
         MVI   CONTINUE,C'Y'                                                    
         B     MIDHK20                                                          
*                                                                               
MIDHK12  MVC   MID1+33(10),=C' CONTINUED'                                       
         MVC   MID2+33(10),=C'----------'                                       
*                                                                               
MIDHK20  LA    R3,MID1                                                          
         LA    R4,2                                                             
         BRAS  RE,GOTSR                                                         
*                                                                               
         LA    R3,SPACES           NEED A BLANK LINE AFTER MIDS                 
         LA    R4,1                                                             
         BRAS  RE,GOTSR                                                         
*                                                                               
MIDHKX   XIT1                                                                   
*                                                                               
MIDLINE  DC    C'---------- ROTATION  COMMERCIAL ---- COMMERCIAL'               
         DC    C' TITLE --- --------- OTHER ----------'                         
MIDLINE1 EQU   *                                                                
         DC    C' CMML RUN TIMES'                                               
MIDLINEX EQU   *                                                                
         LTORG                                                                  
         EJECT                                                                  
*=================================================================              
* FOOTLINE HOOK ROUTINE                                                         
* PUT FOOTLINE DATA IN P WHICH HAS BEEN SAVED BY SPOOL                          
* WHEN THIS ROUTINE RETURNS P=SPACES, IT WILL NOT BE CALLED AGAIN               
*=================================================================              
                                                                                
FTHK     NTR1 BASE=*,LABEL=*                                                    
*                                                                               
         CLI   FOOTSW,5            TEST DONE 5 LINES YET                        
         BE    FTHK10              YES - DONE                                   
         LLC   RE,FOOTSW                                                        
         LA    RE,1(RE)                                                         
         STC   RE,FOOTSW                                                        
         MVI   HEADFLAG,C'F'       SET FOOTLINES PRINTING                       
*                                                                               
FTHK1    MVC   P,SPACES            P DOES NOT GET CLEARED                       
         MVI   P,0                 FORCE RETURN                                 
*                                                                               
         CLI   FOOTSW,1            TEST FIRST LINE                              
         BNE   FTHK1C                                                           
*                                                                               
         OC    STANET,STANET       THIS A CABLE HEAD STATION                    
         BZ    FTHK1A                                                           
         MVC   P+50(L'STANET),STANET                                            
         B     FTHK1B                                                           
*                        X'E8' NOW                                              
FTHK1A   CLI   SVTBMKST+2,CABLESTA THIS A CABLE WITHOUT NETWORK                 
         BL    *+10                                                             
         MVC   P+50(4),SVTBSTA     SHOW AS STATION                              
*                                                                               
FTHK1B   CLI   CONTINUE,C'N'       TEST FINISHED                                
         BE    FTHK1C              YES                                          
         MVI   P,0                 FORCE RETURN                                 
         MVC   P(40),=C'** INSTRUCTIONS CONTINUE ON NEXT PAGE **'               
*                                                                               
FTHK1C   CLI   FOOTSW,3            REACHED LINE 3 YET                           
         BL    FTHK4               NO - SKIP FOOTNOTE                           
*                                                                               
* CHECK FOR FOOTNOTE LINE *                                                     
*                                                                               
         L     R2,ASVFTNT                                                       
         LLC   R0,FOOTSW                                                        
         AHI   R0,-2                                                            
         B     FTHK2X                                                           
FTHK2    CLI   0(R2),0             TEST MORE DATA                               
         BE    FTHK4                                                            
         LA    R2,60(R2)                                                        
FTHK2X   BCT   R0,FTHK2                                                         
         CLI   0(R2),0                                                          
         BE    FTHK4                                                            
         MVC   P(60),0(R2)                                                      
*                                                                               
FTHK4    CLI   CONTINUE,C'N'       TEST LAST PAGE                               
         BE    FTHK6               YES - PRINT STATION ADDRESS                  
         CLI   FOOTSW,2            ONLY PRINT 2 STATION ADDRESS LINES           
         BH    FTHK20              IF NOT LAST PAGE                             
         EJECT                                                                  
* FORMAT STATION ADDRESS DATA *                                                 
*                                                                               
FTHK6    L     R2,ASVSTAD                                                       
         LLC   R0,FOOTSW                                                        
         BCTR  R0,0                                                             
         MHI   R0,24                                                            
         AR    R2,R0                                                            
         MVC   P+75(24),0(R2)                                                   
         B     FTHKX                                                            
*                                                                               
FTHK10   MVI   FOOTSW,0                                                         
         MVC   P,SPACES                                                         
         B     FTHKX                                                            
*                                                                               
FTHK20   MVI   P+75,C'*'                                                        
         MVC   P+76(23),P+75                                                    
*                                                                               
FTHKX    SR    R0,R0                                                            
         LA    R3,P1                                                            
         LA    R4,1                                                             
         BRAS  RE,GOTSR                                                         
*                                                                               
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*===================================================================            
* READ COMMERCIAL RECORDS NEEDED FOR A PATTERN AND SAVE                         
* ON ENTRY PATTERN RECORD IS IN IO1                                             
*===================================================================            
                                                                                
BLDCMLS  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R2,ASVCMLS                                                       
         USING SVCMLD,R2                                                        
         BRAS  RE,CLRSVCML                                                      
*                                                                               
         MVI   THISCML,1          SET CMML ROTATION POSN                        
                                                                                
         XC    FULL,FULL                                                        
         TM    SVOPT3,OPTBPAT                                                   
         BZ    BLDCML0                                                          
*                                                                               
         L     R6,AIO1                                                          
         MVI   ELCODE,X'31'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         ST    R6,FULL                                                          
         B     BLDCML1                                                          
                                                                                
BLDCML0  L     R6,AIO1                                                          
         MVI   ELCODE,X'30'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
BLDCML1  LLC   R0,1(R6)                                                         
         SRL   R0,4                16 BYTES PER PATTERN ENTRY                   
         LA    R5,2(R6)            R5 POINTS TO PATTERN ENTRY                   
                                                                                
         CLI   ELCODE,X'31'                                                     
         BNE   *+8                                                              
         LA    R5,1(R5)                                                         
*                                                                               
BLDCML2  CLC   =X'5C00',0(R5)      TEST COMMERCIAL DELETED                      
         BE    BLDCML34            YES - NEXT COMMERCIAL                        
         CLC   =X'5C00',8(R5)      JUST IN CASE ONLY P/B IS DELETED             
         BE    BLDCML34                                                         
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0A21'                                                  
         MVC   KEY+2(3),BAGYMD     A-M/CLT                                      
         MVC   KEY+5(8),0(R5)                                                   
*                                                                               
         CLI   ADIDFLAG,C'Y'                                                    
         BNE   *+10                                                             
         MVC   KEY(2),=X'0AC1'                                                  
*                                                                               
BLDCML4  GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R6,AIO3             USE IO3 FOR COMMERCIALS                      
         ST    R6,AIO                                                           
         GOTO1 GETREC                                                           
*                                                                               
         CLI   OFFLINE,C'Y'                                                     
         BE    BLDCML6                                                          
*                                                                               
         L     RE,ASVCMLX          GET A(END OF SVCMLS)                         
         AHI   RE,-L'SVCMLDTA                                                   
         CR    R2,RE                                                            
         BNH   *+6                                                              
         DC    H'0'                                                             
*                                                                               
BLDCML6  BRAS  RE,CLRSVCML                                                      
*                                                                               
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING CMLDTAEL,R6                                                      
*                                                                               
         TM    CMLSTAT,X'80'       TEST DELETED COMMERCIAL                      
         BZ    BLDCML8             NO                                           
         MVC   0(2,R5),=X'5C00'    SET FLAG IN PTTN CMML LIST ELEM              
*                                                                               
         AHI   R2,-L'SVCMLDTA      BACK UP TO PREVIOUS ENTRY                    
         CLI   SVCMLPIG,1          WAS THIS 2ND OF PIGGYBACK PAIR?              
         BNE   BLDCML32            NO                                           
         BRAS  RE,CLRSVCML                                                      
         B     BLDCML34            AND REUSE PREVIOUS ENTRY                     
*                                                                               
BLDCML8  DS    0H                                                               
         GOTO1 DATCON,DMCB,(2,PTNLSTR),(3,DUB)                                  
         GOTO1 (RF),(R1),(2,PTNLEND),(3,DUB+3)                                  
*                                                                               
         CLC   CMLRLSE,DUB         SEE IF THIS CMML WILL START IN TIME          
         BH    CMLDTERA             NO, CMLRLSE AFTER PAT START                 
         CLC   CMLRCL,DUB          IS CML RECALL BEFORE PAT START               
         BL    CMLDTERC             YES, ERROR                                  
         CLC   CMLRCL,DUB+3        SEE IF THIS CMML WILL LAST THRU PAT          
         BNL   *+14                YES, OK                                      
         CLC   PTNLEND,=XL2'FFFF' IS PAT UFN                                    
         BNE   CMLDTERB                                                         
*                                                                               
         L     RE,AIO3                                                          
         MVC   SVCMLCOD(8),5(RE)     MOVE CMML CODE FROM *RECORD*               
         MVC   SVCMLCOD+8(4),SPACES  UNTIL CML IS ADID!                         
         MVC   SVCMLNAM,CMLTITLE                                                
         MVC   SVCMLTYP,CMLTYPE                                                 
         OC    SVCMLST,CMLSTAT     SAVE STATUS (POSSIBLE TEXT)                  
*                                                                               
         CLI   SVT1PR11,C'Y'       STATION CML TYP OVERRIDES CML                
         BNE   BLDCML10                                                         
         OC    STCMLTYP,STCMLTYP   STATION HAVE CML TYPE                        
         BZ    *+10                NO                                           
         MVC   SVCMLTYP,STCMLTYP                                                
*                                                                               
BLDCML10 MVC   SVCMLADI,SVCMLCOD   USE ISCI AS ADID IN CASE NO ADID             
         L     RE,AIO                                                           
         TM    15(RE),CMLKSTA_PCKD     TEST CMML IS PACKED                      
         BZ    BLDCML12                NO                                       
         GOTO1 VTRPACK,DMCB,(C'U',SVCMLCOD),SVCMLADI                            
         OI    SVCMLST,X'01'           SET PACKED CMML FLAG                     
*                                                                               
BLDCML12 MVC   SVCMLSLN,CMLSLN                                                  
         MVC   SVCMLOV1,CMLOVRD1                                                
         MVC   SVCMLOV2,CMLOVRD2                                                
         MVC   SVCMLSEQ,CMLSEQ+1                                                
         MVC   SVCMLPOS,THISCML    SAVE POSITION NUMBER                         
         MVC   SVCMLCLT,CMLCLTNO                                                
         MVC   SVCMLFTD,PTNLSTR                                                 
         MVC   SVCMLLTD,PTNLEND                                                 
         MVC   SVCMLPRD,SVTBPRD                                                 
         MVI   SVCMLPIG,0                                                       
         OC    8(8,R5),8(R5)       TEST PIGGYBACK                               
         BZ    BLDCML20                                                         
         MVI   SVCMLPIG,1          SET PRD 1                                    
         CLC   0(8,R5),KEY+5       TEST THIS IS PRD1                            
         BE    BLDCML20                                                         
         MVC   SVCMLPRD,SVTBPRD2                                                
         MVI   SVCMLPIG,2          SET PRD 2                                    
*                                                                               
BLDCML20 MVI   ELCODE,X'30'        GET TITLES 2/3                               
         BRAS  RE,NEXTEL                                                        
         BNE   BLDCML22                                                         
         MVC   SVCMLNM2,3(R6)                                                   
*                                                                               
         BRAS  RE,NEXTEL                                                        
         BNE   BLDCML22                                                         
         MVC   SVCMLNM3,3(R6)                                                   
*                                                                               
BLDCML22 MVI   ELCODE,X'40'        LOOK FOR TELECASTER                          
         L     R6,AIO3                                                          
         BRAS  RE,GETEL                                                         
         BNE   BLDCML23                                                         
         MVC   SVCMLTEL,2(R6)                                                   
*                                                                               
BLDCML23 MVI   ELCODE,X'45'        EXTENDED TELECASTER                          
         L     R6,AIO3                                                          
         BRAS  RE,GETEL                                                         
         BNE   BLDCML24                                                         
         USING CMLXTLEL,R6                                                      
         MVC   SVCMLTLX,CMLXTLNM                                                
         MVC   SVCMLTHD,CMLXTLHD                                                
         DROP  R6                                                               
*                                                                               
BLDCML24 MVI   ELCODE,X'46'        SPOT BROADCAST CBC                           
         L     R6,AIO3                                                          
         BRAS  RE,GETEL                                                         
         BNE   BLDCML25                                                         
         USING CMLCBCEL,R6                                                      
         MVC   SVCMLCBC,CMLCBCNM                                                
         DROP  R6                                                               
*                                                                               
BLDCML25 MVI   ELCODE,X'47'        CANADIAN TALENT CYCLE                        
         L     R6,AIO3                                                          
         BRAS  RE,GETEL                                                         
         BNE   BLDCML26                                                         
         USING CMLTCYEL,R6                                                      
         MVC   SVCMLTC1,CMLTCYC1                                                
         MVC   SVCMLTC2,CMLTCYC2                                                
         DROP  R6                                                               
                                                                                
BLDCML26 CLI   SVCMLADI+8,C' '     TEST WE HAVE A REAL ADID ALREADY             
         BH    BLDCML27            YES                                          
         MVI   ELCODE,X'A0'        LOOK FOR AD-ID                               
         L     R6,AIO3                                                          
         BRAS  RE,GETEL                                                         
         BNE   BLDCML27                                                         
         USING CMLADIEL,R6                                                      
         MVC   SVCMLADI,CMLADID                                                 
         DROP  R6                                                               
*                                                                               
BLDCML27 MVI   ELCODE,X'24'        LOOK FOR EXTENDED DATA EL                    
         L     R6,AIO3                                                          
         BRAS  RE,GETEL                                                         
         BNE   BLDCML28                                                         
         USING CMLXDTEL,R6                                                      
*                                                                               
         MVC   SVCMLHDF,CMLXHDEF                                                
         MVC   SVCMLCTR,CMLXCNTR                                                
         CLI   CMLXSWAP,C'Y'                                                    
         BNE   *+16                                                             
         MVC   SVCMLHDF,CMLXCNTR                                                
         MVC   SVCMLCTR,CMLXHDEF                                                
*                                                                               
         MVC   SVCMLPRN,CMLXPRNT                                                
         MVC   SVCMLHSE,CMLXPRHS                                                
         MVC   SVCMLDDT,CMLXDSDT                                                
         MVC   SVCMLDTM,CMLXDSTM                                                
         DROP  R6                                                               
*                                                                               
BLDCML28 MVI   ELCODE,X'B0'        MATCHING DATA ELEMENT                        
         L     R6,AIO3                                                          
         BRAS  RE,GETEL                                                         
         BNE   BLDCML30                                                         
         USING CMLMATEL,R6                                                      
         MVC   SVCMLSTM,CMLMSTIM                                                
         MVC   SVCMLETM,CMLMETIM                                                
         TM    CMLMFLAG,CMLMFDAY   TEST DAILY TIMES FLAG                        
         BZ    *+8                                                              
         OI    SVCMLST,X'80'       SAVE FLAG                                    
         DROP  R6                                                               
*                                                                               
BLDCML30 CLI   SVCMLPIG,1          TEST PIGGYBACK                               
         BNE   BLDCML32                                                         
         LA    R2,L'SVCMLDTA(R2)   POINT TO NEXT ENTRY                          
         MVC   KEY+5(8),8(R5)      SET TO PROCESS SECOND CMML                   
         B     BLDCML4                                                          
*                                                                               
BLDCML32 LA    R2,L'SVCMLDTA(R2)   POINT TO NEXT ENTRY                          
*                                                                               
BLDCML34 LA    R5,16(R5)           16 BYTES PER ENTRY                           
         LLC   RE,THISCML                                                       
         AHI   RE,1                                                             
         STC   RE,THISCML                                                       
         BCT   R0,BLDCML2                                                       
                                                                                
         L     R6,FULL             LOOK FOR MORE 31 ELEMENTS                    
         MVI   ELCODE,X'31'                                                     
         BRAS  RE,NEXTEL                                                        
         BNE   BLDCXIT                                                          
         ST    R6,FULL                                                          
         B     BLDCML1                                                          
                                                                                
BLDCXIT  J     EQXIT                                                            
*                                                                               
CMLDTERA MVI   ERROR,BDCMLDTS      CML RELEASE DTE AFTER PAT START              
         B     CMLDTERX                                                         
*                                                                               
CMLDTERB MVI   ERROR,BDCMLDTE      CML RECALL DTE BEFORE PAT END                
         B     CMLDTERX                                                         
*                                                                               
CMLDTERC MVI   ERROR,BDCMLRCD      CML RECALL DTE BEFORE PAT START              
*                                                                               
CMLDTERX L     R2,SVTBLINE         POSITION CURSOR                              
         A     R2,ATWA                                                          
         CLI   ERROPT,C'Y'                                                      
         JE    NEQXIT                                                           
         GOTO1 ERREX                                                            
*                                                                               
CLRSVCML NTR1                                                                   
         LR    R0,R2                                                            
         LA    R1,L'SVCMLDTA+1                                                  
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         XIT1                                                                   
         EJECT                                                                  
*================================================================               
* READ AND FORMAT FOOTNOTE TEXT RECORD                                          
*================================================================               
                                                                                
GETFOOT  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0A23'                                                  
         MVC   KEY+2(3),BAGYMD     A-M/CLT                                      
*                                                                               
         CLI   BPRD,X'FF'          TEST POL REQUEST                             
         BE    GETFT6              YES - USE CLT TEXT                           
*                                                                               
         L     R1,ASVCLIST                                                      
GETFT2   CLC   BPRD,3(R1)                                                       
         BE    GETFT4                                                           
         LA    R1,4(R1)                                                         
         CLI   0(R1),C' '                                                       
         BH    GETFT2                                                           
         DC    H'0'                                                             
GETFT4   MVC   KEY+5(3),0(R1)                                                   
*                                                                               
GETFT6   MVI   KEY+8,C'F'                                                       
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE     TEST PRD TEXT FOUND                          
         BE    GETFT10                                                          
*                                                                               
         MVC   KEY,KEYSAVE         RESTORE KEY                                  
         XC    KEY+5(3),KEY+5      CLEAR PRODUCT                                
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    GETFT10                                                          
*                                                                               
         MVC   KEY,KEYSAVE         RESTORE KEY                                  
         XC    KEY+3(2),KEY+3      CLEAR CLIENT                                 
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   GETFTX                                                           
*                                                                               
GETFT10  L     R6,AIO3                                                          
         ST    R6,AIO                                                           
         GOTO1 GETREC                                                           
*                                                                               
         MVI   ELCODE,X'40'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   =C'NONE ',2(R6)     TEST SUPPRESS COMMENT                        
         BE    GETFTX                                                           
*                                                                               
         L     R4,ASVFTNT          NOW FORMAT COMMENT                           
*                                                                               
GETFT20  MVC   0(60,R4),SPACES                                                  
         LLC   RE,1(R6)                                                         
         AHI   RE,-4                                                            
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),3(R6) *EXECUTED*                                         
         LA    R4,60(R4)                                                        
         BRAS  RE,NEXTEL                                                        
         BE    GETFT20                                                          
*                                                                               
         MVI   0(R4),0                                                          
         LA    R4,1(R4)                                                         
         ST    R4,NEXTADDR         SET POINTER                                  
*                                                                               
GETFTX   J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
*=================================================================              
* SEND ALL PRINT LINES TO TSAR TO PRINT COPY LATER                              
*=================================================================              
                                                                                
GOTSR    NTR1 BASE=*,LABEL=*                                                    
                                                                                
         L     RC,SVADGEND                                                      
*                                                                               
         L     RE,ATWA                                                          
         LA    RE,TRAWRKRH-T216FFD(RE)                                          
         CLI   5(RE),0             TEST OPTICA                                  
         BNE   *+12                OPTICA ALWAYS GETS A PDF COPY                
         CLI   SVTWPR1,C'2'        THIS REQUIRE A COPY                          
         BNE   GOTSRX                                                           
                                                                                
* TSAROFF OR TSAR WRITE TO HIGH STORAGE HERE                                    
                                                                                
         LA    R2,TSARWK                                                        
         USING TSARD,R2                                                         
*                                                                               
         LA    R7,ELEM                                                          
         USING TSBUFD,R7                                                        
*                                                                               
         STC   R4,BYTE             SAVE MAX PRINT LINES                         
                                                                                
* FIND ADDRESS OF LAST LINE TO PRINT                                            
                                                                                
GOTSR10  LLC   R6,BYTE             GET NUMBER OF PRINT LINES                    
         BCTR  R6,0                                                             
         MHI   R6,132                                                           
         AR    R6,R3                                                            
*                                                                               
GOTSR20  CLC   0(132,R6),SPACES    TRAILING BLANK LINES DON'T PRINT             
         BNE   GOTSR30                                                          
         AHI   R6,-132                                                          
         BCT   R4,GOTSR20                                                       
*                                                                               
         LA    R4,1                IF ALL BLANK, SET FOR 1 LINE                 
*                                                                               
         LA    R0,1                IGNORE FIRST LINE IF BLANK                   
         CLM   R0,7,TSARCT                                                      
         BE    GOTSRX                                                           
*                                                                               
GOTSR30  XC    TSBUFD(TSBUFX-TSBUFD),TSBUFD  CLEAR TSAR RECORD                  
*                                                                               
         CLI   FORCEHED,C'Y'                                                    
         BNE   *+8                                                              
         MVI   TSBUFHED,C'Y'                                                    
*                                                                               
         CLI   HEADFLAG,C'H'       TEST PRINTING HEADLINES                      
         BNE   GOTSR32                                                          
         LA    R0,10               HEADS TAKE MORE THAN 10 LINES                
         CLM   R0,7,TSARCT         SKIP FORCEHED FOR VERY FIRST PAGE            
         BH    *+8                                                              
         MVI   TSBUFHED,C'Y'                                                    
         NI    HEADFLAG,X'FF'-X'40' DO THIS ONLY ONCE                           
*                                                                               
GOTSR32  LR    RF,R3                                                            
*                                                                               
         OC    0(132,RF),SPACES    MAKE COMPARE BELOW SIMPLER                   
         LA    RE,132              MAX PRINT POSITIONS                          
         LA    RF,131(,R3)         POINT TO END OF PRINT LINE                   
*                                                                               
GOTSR34  CLI   0(RF),C' '          WORK OUT LINE LENGTH                         
         BH    GOTSR40                                                          
*                                                                               
         BCTR  RF,0                                                             
         BCT   RE,GOTSR34                                                       
*                                                                               
GOTSR40  LTR   RE,RE                                                            
         BZ    *+6                                                              
         BCTR  RE,0                                                             
         EX    RE,GOTSRMVC                                                      
*                                                                               
         LA    RE,7+1(,RE)                                                      
         STCM  RE,3,TSBUFLEN       RECORD LENGTH                                
*                                                                               
         A     RE,TSARBYTE                                                      
         ST    RE,TSARBYTE         SAVE TOTAL BYTES USED                        
*                                                                               
         SR    R1,R1               COUNTER IS KEY                               
         ICM   R1,7,TSARCT                                                      
         LA    R1,1(,R1)                                                        
         STCM  R1,7,TSARCT                                                      
         STCM  R1,7,TSBUFKEY                                                    
*                                                                               
         LA    R1,ELEM                                                          
         ST    R1,TSAREC                                                        
*                                                                               
         MVI   TSBUFSPC,1          USE THIS AS DEFAULT                          
         CLI   HEADFLAG,C'F'       TEST FOOTLINES PRINTING                      
         BE    GOTSR50                                                          
         CR    R3,R6               THIS LAST LINE TO PRINT                      
         BL    GOTSR50              NO                                          
         MVC   TSBUFSPC,SPACING    THEN PASS ORIGINAL SPACING VALUE             
*                                                                               
GOTSR50  DS    0H                                                               
         L     RE,ATWA                                                          
         LA    RE,TRAWRKRH-T216FFD(RE)                                          
         CLI   5(RE),0             TEST OPTICA                                  
         BNE   GOTSR58             YES                                          
         TM    WHEN,X'20'          THIS A SOON REQUEST                          
         BO    GOTSRX                                                           
         CLI   OFFLINE,C'Y'        TEST OFFLINE                                 
         BE    GOTSR54                                                          
         MVI   TSACTN,TSAADD                                                    
         B     GOTSR56                                                          
*                                                                               
GOTSR54  DS   0H                                                                
         MVI   TSOFFACT,TSAADD      SET TSAROFF ADD                             
*                                                                               
GOTSR56  DS   0H                                                                
         GOTO1 ATSAR,TSARD                                                      
         CLI   TSERRS,0                                                         
         BE    GOTSR60                                                          
         DC    H'0'                                                             
*                                                                               
GOTSR58  DS    0H                                                               
         XC    TSBUFKEY,TSBUFKEY   CLEAR 3 BYTES FOLLOWING RECLEN               
         L     R1,ATSARFIL                                                      
         LA    R0,ELEM                                                          
         PUT   (1),(0)                                                          
*                                                                               
GOTSR60  LA    R3,132(R3)                                                       
         CR    R3,R6               TEST PRINTED ALL                             
         BH    GOTSRX              YES - EXIT                                   
         BCT   R4,GOTSR30                                                       
*                                                                               
GOTSRX   XIT1                                                                   
*                                                                               
GOTSRMVC MVC   ELEM+7(0),0(R3)                                                  
         DROP  R2,R7                                                            
         EJECT                                                                  
*======================================================================         
* PRINT ALL COMMERCIALS FROM SHIP LIST WITH ALL CANADIAN INFORMATION            
*======================================================================         
*&&DO                                                                           
         USING SVTABLED,R7                                                      
PCC      NTR1 BASE=*,LABEL=*                                                    
*                                                                               
         L     R4,ASHPLIST                                                      
         USING SVSHPD,R4                                                        
*                                                                               
         L     R2,AIO1                                                          
         XC    0(256,R2),0(R2)                                                  
PCC10    CLI   0(R2),0                                                          
         BE    PCC12                                                            
         CLC   SVSHPCA,0(R2)                                                    
         BE    PCC14                                                            
         LA    R2,8(,R2)                                                        
         B     PCC10                                                            
PCC12    MVC   0(8,R2),SVSHPCA                                                  
         MVI   8(R2),0                                                          
PCC14    CLI   SVSHPCB,0           PIGGYBACK                                    
         BE    PCC18               NO                                           
         L     R2,AIO1                                                          
PCC16    CLI   0(R2),0                                                          
         BE    PCC17                                                            
         CLC   SVSHPCB,0(R2)                                                    
         BE    PCC18                                                            
         LA    R2,8(,R2)                                                        
         B     PCC16                                                            
PCC17    MVC   0(8,R2),SVSHPCB                                                  
         MVI   8(R2),0                                                          
PCC18    LA    R4,SVSHPNXT                                                      
         DROP  R4                                                               
         L     R2,AIO1                                                          
         CLI   0(R4),0                                                          
         BNE   PCC10                                                            
         SR    R4,R4                                                            
         CLI   0(R2),0                                                          
         BE    *+12                                                             
         LA    R2,8(,R2)                                                        
         BCT   R4,*-12                                                          
         LPR   R4,R4                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         L     R2,AIO1                                                          
         GOTO1 XSORT,DMCB,(R2),(R4),8,8,0                                       
         EJECT                                                                  
* READ COMMERCIAL RECORD *                                                      
*                                                                               
         XC    KEY,KEY                                                          
PCC20    LA    R1,KEY                                                           
         USING CMLKEY,R1                                                        
         MVC   CMLKID,=X'0A21'                                                  
         MVC   CMLKAM(3),BAGYMD     A-M/CLT                                     
         MVC   CMLKCML,0(R2)                                                    
         DROP  R1                                                               
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R6,AIO3             USE IO3 FOR COMMERCIALS                      
         ST    R6,AIO                                                           
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVI   ALLOWLIN,5                                                       
         USING CMLDTAEL,R6                                                      
         MVC   P+2(8),0(R2)                                                     
         MVC   P+13(15),CMLTITLE                                                
         BRAS  RE,GOSPL                                                         
*                                                                               
         MVI   ELCODE,X'50'        REGISTRATION ELEMENT                         
         BRAS  RE,NEXTEL                                                        
         BNE   PCC50                                                            
         USING CMLNOEL,R6                                                       
         LA    R3,P1+3                                                          
         MVC   0(8,R3),=C'CRTC NO.'                                             
         OC    CMLNOCR,CMLNOCR     CRTC NO                                      
         BZ    PCC32                                                            
         MVC   10(L'CMLNOCR,R3),CMLNOCR                                         
         MVC   132(8,R3),=C'END DATE'                                           
         GOTO1 DATCON,DMCB,(3,CMLNOCRD),(5,143(R3))                             
PCC32    LA    R3,19(,R3)                                                       
*                                                                               
         MVC   0(8,R3),=C'REG. NO.'                                             
         OC    CMLNORE,CMLNORE     REGISTRATION NO.                             
         BZ    PCC34                                                            
         MVC   10(L'CMLNORE,R3),CMLNORE                                         
PCC34    LA    R3,19(,R3)                                                       
*                                                                               
         MVC   0(8,R3),=C'T.C. NO.'                                             
         OC    CMLNOTC,CMLNOTC     T.C. NO                                      
         BZ    PCC36                                                            
         MVC   10(L'CMLNOTC,R3),CMLNOTC                                         
PCC36    LA    R3,19(,R3)                                                       
*                                                                               
         MVC   0(7,R3),=C'CBC NO.'                                              
         OC    CMLNOCB,CMLNOCB     CBC NO                                       
         BZ    PCC38                                                            
         MVC   9(L'CMLNOCR,R3),CMLNOCR                                          
PCC38    LA    R3,19(,R3)                                                       
*                                                                               
         MVC   0(7,R3),=C'ASC NO.'                                              
         OC    CMLNOAS,CMLNOAS     ASC NO                                       
         BZ    PCC40                                                            
         MVC   11(L'CMLNOAS,R3),CMLNOAS                                         
         MVC   132(8,R3),=C'END DATE'                                           
         GOTO1 DATCON,DMCB,(3,CMLNOASD),(5,143(R3))                             
*                                                                               
PCC40    BRAS  RE,GOSPL              GOTO SPOOL RTN                             
*                                                                               
PCC50    MVC   P1,SPACES                                                        
         MVC   P2,SPACES                                                        
         L     R6,AIO3             GET TALENT CYCLE ELEMENT                     
         MVI   ELCODE,X'52'                                                     
         BRAS  RE,GETEL                                                         
         BNE   PCC70                                                            
         USING CMLTCEL,R6                                                       
         LLC   R5,CMLTCLN                                                       
         BCTR  R5,0                                                             
         BCTR  R5,0                                                             
         LA    R6,CMLTLCYS                                                      
         MVC   P+3(12),=C'TALENT CYCLE'                                         
         LA    R3,P+20                                                          
PCC60    GOTO1 DATCON,DMCB,(3,0(R6)),(5,(R3))                                   
         MVI   8(R3),C'-'                                                       
         GOTO1 (RF),(R1),(3,3(R6)),(5,9(R3))                                    
         LA    R3,22(,R3)                                                       
         LA    R6,6(,R6)                                                        
         AHI   R5,-6                                                            
         BP    PCC60                                                            
         MVI   SPACING,2                                                        
         BRAS  RE,GOSPL              GOTO SPOOL RTN                             
         MVI   SPACING,1                                                        
PCC70    LA    R2,8(R2)                                                         
         BCT   R4,PCC20                                                         
         XIT1                                                                   
         LTORG                                                                  
*&&                                                                             
         EJECT                                                                  
*===================================================================            
* PRINT COMMERCIAL TEXT AND ANY OTHER ADDITIONAL DATA                           
* BUILD TABLE IN AIO1 FOR ANY COMMLS THAT HAVE ADDITIONAL DATA                  
*                                                                               
* ADDITIONAL COMMERCIAL DATA IS SAVED IN THE SHIPLIST                           
* SINCE THE SVCMLTAB IS REBUILT FOR EACH PATTERN :(                             
*===================================================================            
                                                                                
         USING SVTABLED,R7                                                      
PRTADDL  NTR1 BASE=*,LABEL=*                                                    
*                                                                               
         L     R4,ASHPLIST                                                      
         USING SVSHPD,R4                                                        
*                                                                               
         L     R0,AIO1             BUILD A TABLE HERE                           
         LHI   R1,4000                                                          
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
PRTA10   L     R2,AIO1                                                          
         USING PRTSHIPD,R2                                                      
*                                                                               
         TM    SVSHPFLG,SVSHPF_YES  TEST ACTIVE THIS MKT                        
         BZ    PRTA30                                                           
         OC    SVSHPSQA,SVSHPSQA   IF SEQ ZERO, NO CML TEXT                     
         BNZ   PRTA12                                                           
         OC    SVSHPDDT,SVSHPDDT   ANY DESTROY DATE                             
         BNZ   PRTA12                                                           
         CLI   SVSHPHSE,C' '       OR HOUSE                                     
         BH    PRTA12                                                           
         CLI   SVSHPTYP,C' '       OR TYPE                                      
         BH    PRTA12                                                           
         B     PRTA30                                                           
*                                                                               
PRTA12   CLI   0(R2),0             AT END OF TABLE                              
         BE    PRTA14                                                           
         CLC   SVSHPCA,0(R2)       ALREADY IN TABLE                             
         BE    PRTA20                                                           
         LA    R2,40(R2)                                                        
         B     PRTA12                                                           
*                                                                               
PRTA14   MVC   PRTSHCML,SVSHPCA     COMML CODE                                  
         MVC   PRTSHDTA,SVSHP1     MOVE CMML1 DETAILS                           
         MVI   40(R2),0                                                         
*                                                                               
PRTA20   CLI   SVSHPCB,0           PIGGYBACK                                    
         BE    PRTA30              NO                                           
*                                                                               
         L     R2,AIO1                                                          
PRTA24   OC    SVSHPSQB,SVSHPSQB   IF SEQ ZERO, NO CML TEXT                     
         BNZ   PRTA26                                                           
         OC    SVSHPDD2,SVSHPDD2                                                
         BNZ   PRTA26                                                           
         CLI   SVSHPHS2,C' '                                                    
         BH    PRTA26                                                           
         CLI   SVSHPTY2,C' '                                                    
         BH    PRTA26                                                           
         B     PRTA30                                                           
*                                                                               
PRTA26   CLI   0(R2),0             END OF TABLE                                 
         BE    PRTA28                                                           
         CLC   SVSHPCB,0(R2)       ALREADY IN TABLE                             
         BE    PRTA30                                                           
         LA    R2,40(R2)                                                        
         B     PRTA26                                                           
*                                                                               
PRTA28   MVC   PRTSHCML,SVSHPCB    P/B COMML CODE                               
         MVC   PRTSHDTA,SVSHP2     MOVE CMML2 DETAILS                           
         MVI   40(R2),0                                                         
*                                                                               
PRTA30   LA    R4,SVSHPNXT                                                      
         CLI   0(R4),0             END OF SHIP TABLE                            
         BNE   PRTA10                                                           
         DROP  R4                                                               
         EJECT                                                                  
*==================================================================             
* COUNT AND SORT TABLE OF COMMERCIALS                                           
*==================================================================             
                                                                                
         L     R2,AIO1                                                          
         SR    R4,R4                                                            
         CLI   0(R2),0                                                          
         BE    *+12                                                             
         LA    R2,40(R2)                                                        
         BCT   R4,*-12                                                          
         LPR   R4,R4                                                            
         BZ    PRTAX               THIS SHOULD NOT HAPPEN                       
*                                                                               
         L     R2,AIO1                                                          
         GOTO1 XSORT,DMCB,(R2),(R4),40,8,0                                      
*                                                                               
         MVI   HEADSW,C'A'         PRINT ADD'TL DATA AS MIDLINE                 
         MVI   CONTINUE,C'N'                                                    
         MVI   FORCEMID,C'Y'                                                    
                                                                                
*=============================================================                  
* READ COMMERCIAL TEXT RECORD                                                   
*=============================================================                  
                                                                                
         L     R2,AIO1                                                          
         USING PRTSHIPD,R2                                                      
*                                                                               
PRTA40   LA    R4,KEY                                                           
         XC    KEY,KEY                                                          
*                                                                               
         TM    SVOPT,PTCMLTXT      TEST PRINT COMML TEXT SW                     
         BZ    PRTA44                                                           
*                                                                               
         OC    PRTSHSEQ,PRTSHSEQ    TEST TEXT FOR THIS CMML                     
         BZ    PRTA44                                                           
*                                                                               
         USING CMTKEY,R4                                                        
         MVC   CMTKID,=X'0A35'                                                  
         MVC   CMTKAM(3),BAGYMD     A-M/CLT                                     
         MVC   CMTKPRD,PRTSHPRD                                                 
         MVC   CMTKMKT(5),SVTBMKST                                              
         MVC   CMTKSEQ,PRTSHSEQ                                                 
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    PRTA42                                                           
*                                                                               
         MVC   KEY(13),KEYSAVE                                                  
         XC    CMTKSTA,CMTKSTA                                                  
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    PRTA42                                                           
*                                                                               
         MVC   KEY(13),KEYSAVE                                                  
         XC    CMTKMKT,CMTKMKT                                                  
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    PRTA42                                                           
*                                                                               
         MVC   KEY(13),KEYSAVE                                                  
         MVI   CMTKPRD,0                                                        
         MVC   CMTKMKT(5),SVTBMKST                                              
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    PRTA42                                                           
*                                                                               
         MVC   KEY(13),KEYSAVE                                                  
         XC    CMTKSTA,CMTKSTA                                                  
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    PRTA42                                                           
*                                                                               
         MVC   KEY(13),KEYSAVE                                                  
         XC    CMTKMKT,CMTKMKT                                                  
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         MVI   RDUPDATE,C'N'                                                    
         CLC   KEY(13),KEYSAVE                                                  
         BE    PRTA42                                                           
         XC    KEY,KEY             INDICATE NO TEXT FOUND                       
         B     PRTA44                                                           
         DROP  R4                                                               
*                                                                               
PRTA42   L     R6,AIO3             USE IO3                                      
         ST    R6,AIO                                                           
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
*                                                                               
PRTA44   L     R4,ACMLEXP                                                       
         L     R5,SIZEIO                                                        
         BRAS  RE,CLRBUFF                                                       
*                                                                               
         L     R4,ACMLEXP                                                       
         USING PLINED,R4                                                        
*                                                                               
         MVI   ALLOWLIN,0          CLEAR LINE COUNTER                           
*                                                                               
         CLI   SVT2PR02,C'Y'       TEST TO SUPPRESS TYPE                        
         BE    PRTA46                                                           
         CLI   PRTSHTYP,C' '                                                    
         BNH   PRTA46                                                           
         MVC   15(5,R4),=C'TYPE='                                               
         MVC   20(4,R4),PRTSHTYP                                                
         LA    R4,132(R4)                                                       
         IC    R0,ALLOWLIN                                                      
         AHI   R0,1                                                             
         STC   R0,ALLOWLIN                                                      
*                                                                               
PRTA46   DS    0H                                                               
*                                                                               
         CLI   KEY,0               TEST ANY TEXT THIS CMML                      
         BE    PRTA50                                                           
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'40'                                                     
         BRAS  RE,GETEL                                                         
         BNE   PRTA50                                                           
*                                                                               
PRTA48   LLC   RE,1(R6)                                                         
         AHI   RE,-4                                                            
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   15(0,R4),3(R6)      DO NOT MOVE COMMENT NUMBER                   
         CLC   0(132,R4),SPACES                                                 
         BNE   *+8                                                              
         MVI   0(R4),0             FORCE LINE TO PRINT                          
         LA    R4,132(R4)                                                       
         IC    R0,ALLOWLIN                                                      
         AHI   R0,1                                                             
         STC   R0,ALLOWLIN                                                      
         BRAS  RE,NEXTEL                                                        
         BE    PRTA48                                                           
*                                                                               
PRTA50   CLI   PRTSHHSE,C' '       TEST ANY PROD HOUSE                          
         BH    PRTA56                                                           
         CLI   PRTSHDDT,0          TEST ANY DESTROY DATE                        
         BE    PRTA58              NOS                                          
*                                                                               
PRTA56   IC    R0,ALLOWLIN         THEN NEED ONE MORE LINE                      
         AHI   R0,1                                                             
         STC   R0,ALLOWLIN                                                      
         LA    R4,15(R4)                                                        
*                                                                               
PRTA58   CLI   PRTSHHSE,C' '       ANY PROD HOUSE                               
         BNH   PRTA60                                                           
         MVC   0(11,R4),=C'PROD HOUSE:'                                         
         MVC   12(6,R4),PRTSHHSE                                                
         LA    R4,20(R4)                                                        
*                                                                               
PRTA60   CLI   PRTSHDDT,0          ANY DESTROY DATE                             
         BE    PRTA62                                                           
         MVC   0(14,R4),=C'PLEASE DESTROY'                                      
         GOTO1 DATCON,DMCB,(3,PRTSHDDT),(8,16(R4))                              
*                                                                               
         CLI   PRTSHDTM,0          ANY DESTROY TIME                             
         BE    PRTA62              NO                                           
*                                                                               
         XC    DUB,DUB                                                          
         MVC   DUB(2),PRTSHDTM                                                  
         GOTO1 CALLOV,DMCB,0,X'D9000A11'  GET UNTIME ADDRESS                    
         L     RF,0(R1)                                                         
         GOTO1 (RF),DMCB,DUB,26(R4)                                             
*                                                                               
PRTA62   OC    SVINSDT,SVINSDT     IF ORIGINAL, NO CHANGE POSSIBLE              
         BZ    PRTA64                                                           
         CLI   SVINSREV,0          IF REVSION ZERO                              
         BNE   *+12                                                             
         TM    SVOPT,OPTRERUN      AND RERUN                                    
         BO    PRTA64                                                           
*                                                                               
         L     R6,AIO3                                                          
         MVI   ELCODE,X'F1'                                                     
         BRAS  RE,GETEL                                                         
         BNE   PRTA72                                                           
*                                                                               
         CLC   SVINSDT,2(R6)                                                    
         BNL   PRTA64                                                           
         MVC   PLPRD+29(6),=C'(ADDED'                                           
         MVC   PLPRD+36(24),=C'SINCE LAST INSTRUCTIONS)'                        
         B     PRTA70                                                           
*                                                                               
PRTA64   CLC   SVINSDT,8(R6)                                                    
         BNL   PRTA70                                                           
         MVC   PLPRD+29(8),=C'(CHANGED'                                         
         MVC   PLPRD+38(24),=C'SINCE LAST INSTRUCTIONS)'                        
*                                                                               
PRTA70   CLI   ALLOWLIN,0          TEST ANYTHING TO PRINT                       
         BE    PRTA80              NO                                           
*                                                                               
PRTA72   L     R4,ACMLEXP                                                       
         MVC   0(8,R4),PRTSHCML   MOVE ISCI CMML TO PRINT                       
         CLI   PRTSHADI,C' '                                                    
         BNH   *+10                                                             
         MVC   0(12,R4),PRTSHADI  USE ADID IF IT'S THERE                        
*                                                                               
         L     R1,ACMLEXP          PRINT CONTENTS OF BUFFER                     
         BRAS  RE,PRTBUFF                                                       
*                                                                               
PRTA80   LA    R2,PRTSHNXT                                                      
         CLI   0(R2),0                                                          
         BNE   PRTA40                                                           
*                                                                               
PRTAX    MVI   HEADSW,C'X'         RESTORE HEADSW                               
         XIT1                                                                   
         LTORG                                                                  
         DROP  R2                                                               
                                                                                
*===============================================================                
* WRITE THE SVTABLE BUFFER TO AN 0A7F RECORD ON THE TRFFIL                      
*===============================================================                
                                                                                
WRTSVTAB NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R4,KEY                                                           
         USING XSAVKEY,R4                                                       
*                                                                               
         XC    KEY,KEY                                                          
         MVI   XSAVTYPE,XSAVTYPQ   X'0A'                                        
         MVI   XSAVSBTY,XSAVSBTQ   X'7F'                                        
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(2),KEYSAVE                                                   
         BE    WRTSV2                                                           
         MVC   KEY,KEYSAVE         AND RESTORE KEY                              
         LA    R0,1                                                             
         B     WRTSV4                                                           
*                                                                               
WRTSV2   ICM   R0,12,XSAVSEQ                                                    
         SRA   R0,16                                                            
         X     R0,=X'FFFFFFFF'     UNCOMPLEMENT                                 
         AHI   R0,1                                                             
*                                                                               
WRTSV4   X     R0,=X'FFFFFFFF'     GET THE FF COMPLEMENT                        
         STCM  R0,3,XSAVSEQ                                                     
*                                                                               
         GOTO1 DATCON,DMCB,(5,0),(3,XSAVDTB)                                    
*                                                                               
         MVC   XSAVAGMD,BAGYMD     SET A-M                                      
         MVC   XSAVCLT,BCLT        SET CLT                                      
         MVI   XSAVMULT,0          CLEAR                                        
                                                                                
         L     R4,ATIA             NOTE SVTABLE STARTS AT +24                   
         XC    0(24,R4),0(R4)                                                   
         MVC   0(13,R4),KEY                                                     
         MVC   XSAVAGY,AGENCY                                                   
         DROP  R4                                                               
*                                                                               
WRTSV10  AHI   R4,XSAVFRST-XSAVRECD POINT TO FIRST ENTRY                        
         LA    R0,XSAVFRST-XSAVRECD SET RECLEN SO FAR                           
*                                                                               
* USE FIRST TWO BYTES OF EACH ENTRY AS ELEMENT CODE/LEN                         
*                                                                               
WRTSV12  MVI   0(R4),X'01'                                                      
         MVI   1(R4),L'SVTBDATA                                                 
         LA    R4,L'SVTBDATA(R4)   POINT TO NEXT ENTRY                          
         AHI   R0,L'SVTBDATA       BUMP RECORD LEN                              
         OC    0(4,R4),0(R4)                                                    
         JZ    WRTSV14                                                          
         CHI   R0,5972-L'SVTBDATA   TEST REC FULL                               
         JL    WRTSV12                                                          
                                                                                
* WRITE THE RECORD NOW                                                          
                                                                                
WRTSV14  L     RE,ATIA             POINT TO START OF REC                        
         STCM  R0,3,XSAVLEN-XSAVKEY(RE)                                         
                                                                                
* DO NOT CALL ADDREC HERE BECAUSE DIES ON HELLO CALL IN GENCON                  
                                                                                
         GOTO1 DATAMGR,DMCB,=C'ADDREC',=C'TRFFILE',KEY,ATIA,DMWORK              
         CLI   8(R1),0                                                          
         JNE   *+2                                                              
*                                                                               
         OC    0(2,R4),0(R4)       TEST MORE TO WRITE                           
         JZ    EXIT                                                             
                                                                                
* MOVE THE REMAINING ENTRIES TO TIA+24                                          
                                                                                
         LHI   R5,TWAMAX                                                        
         SR    R5,R4               GIVES LEN TO MOVE                            
         L     R0,ATIA                                                          
         AHI   R0,24                                                            
         LHI   R1,TWAMAX                                                        
         MVCL  R0,R4               SHIFT THE BUFFER UP!                         
*                                                                               
         L     R4,ATIA             POINT TO START                               
         J     WRTSV10                                                          
         LTORG                                                                  
         EJECT                                                                  
*===============================================================                
* PRINT PATTERN ERROR - ALL CMLS DELETED FROM PATTERN                           
*===============================================================                
                                                                                
         USING SVTABLED,R7                                                      
PTTNER   NTR1 BASE=*,LABEL=*                                                    
         L     RC,SVADGEND                                                      
         USING GEND,RC                                                          
         CLI   OFFLINE,C'Y'                                                     
         BE    PTTNER10                                                         
*                                                                               
* ON-LINE ERROR *                                                               
*                                                                               
         L     R3,ATWA                                                          
         XC    CONHEAD-T216FFD(,R3),CONHEAD-T216FFD(R3)                         
         MVC   CONHEAD-T216FFD(36,R3),=CL36'* ERR-PATTERN HAS ALL CMLS C        
               DELETED *'                                                       
         L     R2,SVTBLINE         PRESET CURSOR FOR ERROR                      
         AR    R2,R3                                                            
         LA    RF,CONOTHH-T216FFD(,R3)                                          
         LLC   R0,0(RF)            MOVE TO TITLE                                
         AR    RF,R0                                                            
         LLC   R0,0(RF)            MOVE TO MEDIA INPUT                          
         AR    RF,R0                                                            
         CLI   0(RF),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         NI    4(RF),X'FF'-X'20'   SET OFF VALIDATION IN MEDIA FIELD            
*                                                                               
         MVI   ERROR,X'FF'         SET ERROR FLAG                               
         CLI   ERROPT,C'Y'                                                      
         BE    PTTNERX                                                          
         GOTO1 ERREX2                                                           
*                                                                               
* BUILD ERROR MESSAGE FOR OFF-LINE ERROR FILE *                                 
*                                                                               
PTTNER10 XC    ELEM,ELEM                                                        
         LA    R6,ELEM                                                          
         USING EFRECD,R6                                                        
         MVI   EFREASON,3          SET ERR LIST TO PTTN NO CMLS                 
*                                                                               
         LA    R1,SVTBPRD                                                       
         LA    R4,EFPRDLN                                                       
         BAS   RE,FMTPTN                                                        
         CLI   SVTBPRD2,0          TEST PIGGYBACK                               
         BE    PTTNER20                                                         
*                                                                               
         LA    R1,SVTBPRD2                                                      
         LA    R4,EFPRDLN                                                       
         BAS   RE,FMTPTN                                                        
*                                                                               
PTTNER20 MVC   EFQUESTR,QUESTOR                                                 
         MVC   EFAGY,AGENCY                                                     
         MVC   EFMED,QMED                                                       
         MVC   EFCLT,QCLT                                                       
         LA    R1,SVTBPRD                                                       
         LA    R4,EFPRDLN                                                       
         BAS   RE,FMTPTN                                                        
*                                                                               
         LA    R1,SVTBPRD2                                                      
         LA    R4,EFPRDLN2                                                      
         CLI   0(R1),0                                                          
         BE    *+8                                                              
         BAS   RE,FMTPTN                                                        
*                                                                               
         MVC   EFCOPY,SVTBCOPY                                                  
         SR    R0,R0                                                            
         ICM   R0,3,SVTBMKST                                                    
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  EFMKT,DUB                                                        
         MVC   EFSTA,SVTBSTA                                                    
         MVC   EFAFFL,SVTBAFFL                                                  
         MVC   EFTYPE,SVTBTYPE                                                  
         MVC   EFERRDT,PARAS                                                    
*                                                                               
         MVC   EFBAGYMD(3),BAGYMD  A-M/CLT                                      
         MVC   EFBPRD(4),SVTBPRD   PRD/SLN/PRD2/SLN2                            
         MVC   EFBMKST(5),SVTBMKST                                              
         MVC   EFPAGE,SEQNUM                                                    
*                                                                               
         L     R2,AERRFILE                                                      
*                                                                               
         PUT   (R2),(R6)                                                        
PTTNERX  XIT1                                                                   
         EJECT                                                                  
FMTPTN   L     RF,ASVCLIST                                                      
*                                                                               
FMTPTN10 CLC   0(1,R1),3(RF)                                                    
         BE    FMTPTN20                                                         
         LA    RF,4(RF)                                                         
         CLI   0(RF),C' '                                                       
         BH    FMTPTN10                                                         
         DC    H'0'                                                             
*                                                                               
FMTPTN20 MVC   0(3,R4),0(RF)                                                    
         LA    R4,2(,R4)            POINT TO END OF PRD                         
         CLI   0(R4),C' '                                                       
         BNE   *+6                                                              
         BCTR  R4,0                                                             
         MVI   1(R4),C'-'                                                       
*                                                                               
         LLC   R0,1(R1)                                                         
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  2(3,R4),DUB                                                      
         LA    R4,6(R4)            POINT 1 BEYOND END                           
         BR    RE                                                               
         DROP  R6                                                               
         LTORG                                                                  
         EJECT                                                                  
HDSPECS  DS    0D                                                               
         SSPEC H1,3,AGYNAME                                                     
         SSPEC H2,3,AGYADD                                                      
*                                                                               
*NOTE*   SSPEC H1,39,C'SPOT TELEVISION COMMERCIAL INSTRUCTIONS'                 
*NOTE*   SSPEC H2,39,C'---------------------------------------'                 
         SSPEC H3,46,PERIOD                                                     
         SSPEC H4,39,C' (SUPERSEDES INSTRUCTIONS OF OCT10/83)'                  
*                                                                               
         SSPEC H1,85,REPORT                                                     
         SSPEC H2,85,RUN                                                        
         SSPEC H3,85,C'PURCHASE ORDER'                                          
         SSPEC H4,85,C'REVISION'                                                
         SSPEC H5,85,PAGE                                                       
         SSPEC H5,93,REQUESTOR                                                  
*                                                                               
         DC    X'00'               END OF TABLE                                 
FMTCODT  DS   0CL42                                                             
         DC    CL42'A/=ACTION ADVENTURE                       '                 
         DC    CL42'C/=CARTOONS && ANIMATION                  '                 
         DC    CL42'E/=TEEN TALK AND ENTERTAINMENT SHOWS      '                 
         DC    CL42'K/=SITCOMS FOR KIDS                       '                 
         DC    CL42'M/=TEEN MINORITY AUDIENCES                '                 
         DC    CL42'N/=NEWS AND ENTERTAINMENT                 '                 
         DC    CL42'O/=OTHER                                  '                 
         DC    CL42'P/=ADULT PROGRAMS                         '                 
         DC    CL42'R/=TEEN TALK SHOWS                        '                 
         DC    CL42'S/=SPORTS                                 '                 
         DC    CL42'T/=PROGRAMS FOR GENERAL TEENS             '                 
         DC    CL42'W/=DRAMAS FOR TEENS AND OLDER TEEN SITCOMS'                 
FMTCODS  EQU   (*-FMTCODT)/L'FMTCODT                                            
         DC    CL42' /=UNKNOWN  '                                               
         SPACE 2                                                                
* DCB FOR OFF-LINE ERROR LOGGING - READ BY NEXT JOBSTEP *                       
* NOTE DDNAME MODIFIED WHEN RUNNING SOON TO --> TALWRK <--                      
*                                                                               
ERRFILE  DCB   DDNAME=ERRFILE,DSORG=PS,RECFM=FB,BLKSIZE=2000,          X        
               LRECL=100,MACRF=PM                                               
         SPACE 2                                                                
* MODEL DCB FOR AUTO REQUEST FILE. COPIED INTO CORE RESIDENT AREA               
* BEFORE OPEN                                                                   
*                                                                               
TR04FIL  DCB   DDNAME=TR04FIL,DSORG=PS,RECFM=FB,BLKSIZE=3200,          X        
               LRECL=64,MACRF=PM                                                
*                                                                               
TR07AMS  DCB   DDNAME=TR07AMS,DSORG=PS,RECFM=FB,BLKSIZE=7200,          X        
               LRECL=240,MACRF=PM                                               
*                                                                               
TSARFILE DCB   DDNAME=TSARFIL,DSORG=PS,RECFM=VB,BLKSIZE=13200,         X        
               LRECL=140,MACRF=(GM,PM),EODAD=PRTC50                             
         EJECT                                                                  
       ++INCLUDE SPMGRTAB                                                       
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
*                                                                               
         ORG   P                                                                
       ++INCLUDE EDIDDSHD                                                       
*                                                                               
       ++INCLUDE DMPRTQL                                                        
       ++INCLUDE DDREPMASTD                                                     
       ++INCLUDE DDREMOTED                                                      
         EJECT                                                                  
       ++INCLUDE DDSPLWORKD                                                     
         EJECT                                                                  
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
       ++INCLUDE SPTRKEYS                                                       
         EJECT                                                                  
       ++INCLUDE SPTRAFFD                                                       
         PRINT ON                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE SPTRAF7D                                                       
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
       ++INCLUDE SPTRAWORKD                                                     
         EJECT                                                                  
       ++INCLUDE SPTRAINSTC                                                     
         EJECT                                                                  
TWADCOND DSECT                                                                  
       ++INCLUDE DDTWADCONS                                                     
         EJECT                                                                  
SPOOLD   DSECT                                                                  
         ORG   P                                                                
       ++INCLUDE EDIDESTD                                                       
         ORG   P                                                                
       ++INCLUDE EDILINKD                                                       
         PRINT ON                                                               
TSBUFD   DSECT                                                                  
TSBUFLEN DS    XL2                                                              
TSBUFKEY DS    XL3                                                              
TSBUFHED DS    XL1                                                              
TSBUFSPC DS    XL1                                                              
TSBUFLIN DS    CL132                                                            
TSBUFX   EQU   *                                                                
*                                                                               
CMLBUFFD DSECT                                                                  
CMLBSLN  DS    CL6                                                              
         DS    CL3                                                              
CMLBROT  DS    CL8                                                              
         DS    CL2                                                              
CMLBCML  DS    CL12                                                             
         DS    CL2                                                              
CMLBTTL  DS    CL20                                                             
         DS    CL2                                                              
CMLBOTHR DS    CL24                                                             
         DS    CL2                                                              
CMLBTIME DS    CL11                                                             
         ORG   CMLBSLN+80                                                       
CMLBPRD  DS    CL6                                                              
       ++INCLUDE DDGLOBEQUS                                                     
       ++INCLUDE DDGLVXCTLD                                                     
       ++INCLUDE DDGLVSPTRF                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'248SPTRA04   06/06/18'                                      
         END                                                                    
