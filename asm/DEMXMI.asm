*          DATA SET DEMXMI     AT LEVEL 011 AS OF 01/20/21                      
*PROCESS USING(WARN(15))                                                        
*PHASE DEMXMIA                                                                  
*---------------------------------------------------------------------*         
* NATIONAL MINUTE BY MINUTE CONVERSION: INPUT PHASE.                  *         
*                                                                     *         
* IPHASE: DEMXMI                                                      *         
* OPHASE: DEMXMO                                                      *         
*                                                                     *         
* THIS CONVERSION TAKES AS INPUT A TEXT FILE PRODUCED BY A JAVA       *         
* EXTRACT FROM THE AMRLD RELATIONAL DATABASE.                         *         
*---------------------------------------------------------------------*         
         TITLE '- DEMO CONVERSION - NATIONAL MINUTE BY MINUTE'                  
                                                                                
DEMXMI   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,DEMXMI,RA,RC                                                   
         USING DPRINT,R7           R7 -> PRINT CSECT                            
         USING DEMCOND,R8          R8 -> GLOBAL WORKING STORAGE                 
         L     R5,AIREC            R5 -> INTERIM RECD  (INTERD)                 
         USING INTERD,R5           KEY, INFO, AND DEMO VALUES                   
         LA    RE,MAXIRECL                                                      
         CHI   RE,2000                                                          
         BNH   *+6                 INTERIM RECORD LENGTH OVER MAX               
         DC    H'0'                ALLOWED OF 2000                              
*                                                                               
         MVC   ASRTCARD,=A(SORTCARD) OVERRIDE SORTER "SORT FIELDS" CARD         
         LHI   RE,L'INTKEY                                                      
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  SORTCRDL,DUB                                                     
*                                                                               
         B     *+4(R1)             ROUTINE HOOK                                 
         B     READ                PROCESS INPUT TAPE                           
         B     CNVWR               SORT RECORD HOOK                             
         B     ENDJOB              E-O-F ON INPUT                               
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* READ - GET INPUT RECORDS ONE AT A TIME AND PROCESS. BUILD INTERIM             
*        RECORDS.                                                               
***********************************************************************         
READ     DS    0H                                                               
*                                                                               
         CLI   MYMODE,RELMEND      COMING BACK FROM LAST RECD RELEASE           
         BE    ENDJOB              END THE JOB                                  
*                                                                               
         L     R9,ARREC            INPUT RECORD AREA                            
*                                                                               
         CLI   INTAPESW,1          TEST FOR OPENING INPUT TAPE                  
         BE    READ10                                                           
         OPEN  (IN1,(INPUT))                                                    
*                                                                               
         MVC   P(49),=C'** SEE SYSPRIN2 IN JOB OUTPUT FOR MORE DETAILS '        
               **'                                                              
         GOTO1 VPRINTER                                                         
         MVC   P,SPACES                                                         
         MVC   P(24),=C'** STARTING INPUT PHASE:'                               
         GOTO1 VDEPRNT2                                                         
         MVC   P(28),=C'MED SRC BTYP YYYY/WW STATION'                           
         GOTO1 VDEPRNT2                                                         
*                                                                               
READ10   CLI   MYMODE,RELMRET      COMING BACK FROM RELEASE MODE                
         BNE   READ12                                                           
         MVI   MYMODE,0            RESET MODE                                   
         B     READ20              AND CONTINUE FROM WHERE I LEFT OFF           
*                                                                               
READ12   LR    RE,R9               PADD RECORD AREA WITH SPACES                 
         LA    RE,4(RE)            NEEDED FOR VARIABLE LENGTH RECORDS           
         LA    RF,MAXRRECL                                                      
         LA    R2,WORK             DUMMY ADDRESS                                
         SR    R3,R3               FILL CHARACTER                               
         ICM   R3,8,=C'    '                                                    
         MVCL  RE,R2               MOVE SPACES IN RECORD AREA                   
*                                                                               
         GET   IN1,(R9)            GET A RECORD                                 
*                                                                               
         CLI   PRVRTYP,MDRTYPQ     CK IF NEED TO RELEASE MINUTE RECORD          
         BNE   READ20                                                           
         CLI   PRVRSTYP,MDRSTYPQ   IF PREVIOUS IS 'MD' RECORD                   
         BNE   READ20                                                           
         CLI   4(R9),MDRTYPQ       BUT CURRENT ISNT 'MD' RECORD                 
         BNE   READ15                                                           
         CLI   5(R9),MDRSTYPQ                                                   
         BE    READ20                                                           
READ15   MVI   MYMODE,RELMRET                                                   
         B     RELEASE             RELEASE PREVIOUS MINUTE DATA                 
*                                                                               
READ20   CLI   4(R9),DELRTYPQ      DELETION RECORD                              
         BE    READDEL                                                          
         CLI   4(R9),MIRTYPQ       MINUTE RECORD                                
         BE    READMIN                                                          
         CLI   4(R9),PTRTYPQ       PASSIVE RECORD                               
         BE    READPAS                                                          
         DC    H'0'                INVALID RECORD TYPE                          
*                                                                               
READDEL  BAS   RE,DELR             PROCESS DELETION RECORD                      
         B     RELEASE                                                          
*                                                                               
READMIN  CLI   5(R9),MIRSTYPQ                                                   
         BNE   READMIND                                                         
         BAS   RE,MIREC            PROCESS MINUTE INFORMATION RECORD            
         B     READNXT                                                          
READMIND CLI   5(R9),MDRSTYPQ                                                   
         BNE   READMINX                                                         
         BAS   RE,MDREC            PROCESS MINUTE DEMO VALUES RECORD            
         B     READNXT                                                          
READMINX DC    H'0'                INVALID RECORD SUBTYPE                       
*                                                                               
READPAS  CLI   5(R9),PTRSTYPQ                                                   
         BNE   READPASD                                                         
         BAS   RE,PTREC            PROCESS MINUTE INFORMATION RECORD            
         B     RELEASE                                                          
READPASD CLI   5(R9),PDRSTYPQ                                                   
         BNE   READPASX                                                         
         BAS   RE,PDREC            PROCESS MINUTE DEMO VALUES RECORD            
         B     RELEASE                                                          
READPASX DC    H'0'                                                             
*                                                                               
READNXT  BAS   RE,SAVEPREV         SAVE PREVIOUS RECORD TYPE & SUBTYPE          
         B     READ12                                                           
*                                                                               
RELEASE  BAS   RE,SAVEPREV                                                      
         CLI   INTRTYP,MXMCODEQ                                                 
         BNE   *+12                                                             
         BAS   RE,HOMSHARE         COMPUTE HOME SHARE FOR THE 'M' REC           
         BAS   RE,COMPVPH          COMPUTE VPHS                                 
         B     EXIT                                                             
*                                                                               
SAVEPREV MVC   PRVRTYP,4(R9)       SAVE PREVIOUS RECORD TYPE                    
         MVC   PRVRSTYP,5(R9)      SAVE PREVIOUS RECORD SUBTYPE                 
         BR    RE                                                               
         EJECT                                                                  
**********************************************************************          
* CNVWR - SORT RECDS HOOK.  AFTER ALL IRECS BUILT DEMCNV COMES HERE             
* BEFORE IT GOES TO THE OUTPUT PHASE.  LAST CHANCE TO PROCESS BEFORE            
* OUTPUT PHASE HANDLING.                                                        
**********************************************************************          
CNVWR    L     RE,ASREC                                                         
         LA    RE,4(RE)            GO PAST RECORD LENGTH                        
         CLI   0(RE),MXMCODEQ      'M' RECORDS                                  
         BNE   CNVWRX                                                           
         USING MXMKEY,RE                                                        
*                                                                               
* DEIS APR/2018:                                                                
*                                                                               
*   NOTE: THERE IS AN ISSUE HERE THAT MAY EVENTUALLY NEED TO BE                 
*   RESOLVED, IF/WHEN NATIONAL DEMOS ARE PORTED TO VSAM. IT PERTAINS            
*   TO THE POST-CONVERSION PROCESS.                                             
*                                                                               
*   THE 'MXMMMORE' FLAG (X'08') IN THE LOW-ORDER NIBBLE OF THE 2-BYTE           
*   MINOR KEY TAGS THE *LAST* PROGRAM FOR A PARTICULAR MINUTE. (SEE             
*   DEDEMFILE FOR DETAILS.) IN VIRTUALLY ALL CASES, THE 'M' RECORDS             
*   HAVE THIS BIT SET ON, BECAUSE THERE IS ONLY ONE PROGRAM WITHIN A            
*   GIVEN MINUTE. IF THERE ARE TWO PROGRAMS WITHIN THE SAME MINUTE,             
*   THEN THE FIRST MINOR KEY HAS THE BIT OFF, AND THE SECOND HAS THE            
*   BIT ON. *BUT*: THE LOGIC BELOW CAUSES THE *LAST* RECORD TO BE PUT           
*   TO THE OUTPUT FILE *FIRST*, FOLLOWED BY THE *FIRST* RECORD. IN              
*   OTHER WORDS, THE RECORD WITH THE *HIGHER* MINOR KEY IS WRITTEN TO           
*   THE OUTPUT FILE *BEFORE* THE RECORD WITH THE *LOWER* MINOR KEY.             
*   THERE'S NOTHING WRONG WITH THAT, BECAUSE THIS CONVERSION OBVIOUSLY          
*   RELIES UPON A POST-CONVERSION SORT TAKING PLACE (WHICH IT DOES)             
*   PRIOR TO THE UPDATE. BUT THAT MEANS THAT THE PRE-VSAM-UPDATE                
*   *INTEGRITY CHECK* THAT WE WISH TO PERFORM *BEFORE* DOING THE                
*   POST-CONVERSION SORT WON'T WORK. IT WILL DETECT A "MINOR KEY                
*   SEQUENCE ERROR" AND GENERATE AN ERROR.                                      
*   EVENTUALLY, WE'LL HAVE TO FIGURE OUT HOW TO DEAL WITH THIS, IF WE           
*   DECIDE TO CONVERT THE NTIDIR/FIL TO VSAM.                                   
*                                                                               
         CLC   PRVMKEY,0(RE)       CHECK FOR IDENTICAL KEYS (MULTIPLE           
         BE    CNVWRX              PROGS IN THE SAME MIN). FLAG OFF             
*                                                                               
CNVW20   MVC   PRVMKEY,0(RE)                                                    
         OI    MXMMIN+1,MXMMMORE   FLAG ON IF NEW MINUTE                        
*                                                                               
         DROP  RE                                                               
*                                                                               
CNVWRX   MVI   BYPSORT,0           RELEASE RECORD FLAG                          
         L     RE,ASREC            MOVE DATA TO WREC                            
         L     RF,AWREC                                                         
         LA    R1,2000                                                          
         MOVE  ((RF),(R1)),(RE)                                                 
         B     XIT                                                              
         EJECT                                                                  
**********************************************************************          
* EOF ON INPUT FILE                                                             
**********************************************************************          
*                                                                               
DONE     DS    0H                                                               
         CLI   PRVRTYP,MDRTYPQ     CK IF NEED TO RELEASE MINUTE RECORD          
         BNE   ENDJOB                                                           
         CLI   PRVRSTYP,MDRSTYPQ   IF PREVIOUS IS 'MD' RECORD                   
         BNE   ENDJOB                                                           
         MVI   MYMODE,RELMEND      RELEASE AND END THE JOB                      
         B     RELEASE                                                          
*                                                                               
ENDJOB   DS    0H                                                               
         CLOSE (IN1,REWIND)                                                     
         MVI   INTAPESW,X'02'                                                   
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
EXIT     XMOD1 1                                                                
XIT      XIT1                      END OF PROCEDURE- RETURN TO CALLER           
         EJECT                                                                  
**********************************************************************          
* PROCESS DELETION RECORDS 'D'                                                  
**********************************************************************          
DELR     NTR1                                                                   
*                                                                               
DELR10   LA    R9,4(R9)                                                         
         USING DELRECD,R9                                                       
         LA    RE,INTERD           CLEAR INTERIM RECORD AREA                    
         LA    RF,MAXIRECL                                                      
         XCEF                                                                   
*                                                                               
* BUILD INTERIM RECORD KEY                                                      
         USING IDELKEY,INTKEY                                                   
         MVI   IDRTPE,IDRTPEQ      RECORD TYPE                                  
         MVI   IDSRTPE,IDSRTPEQ    RECORD SUB-TYPE                              
*                                                                               
         GOTO1 VNETWEEK,DMCB,DELRWEEK,VGETDAY,VADDAY   GET BOOK                 
         MVC   IDBOOK+0(1),4(R1)   YEAR                                         
         MVC   IDBOOK+1(1),8(R1)   WEEK #                                       
*                                                                               
         LA    RE,DMEDTAB          GET MEDIA                                    
         USING DMEDTABD,RE                                                      
DELR20   CLI   0(RE),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                INVALID INPUT MEDIA                          
         CLC   DELRMED,DMDINREC    MEDIA ON THE INPUT RECORD                    
         BE    DELR25                                                           
         LA    RE,DMEDTABL(RE)                                                  
         B     DELR20                                                           
DELR25   MVC   IDMEDIA,DMDITREC    MEDIA ON THE INTERIM RECORD                  
         DROP  RE                                                               
*                                                                               
         CLC   DELRSTA,SPACES      STATION                                      
         BE    DELR28                                                           
         MVC   IDSTATN(4),DELRSTA                                               
         TM    DELRSTA+4,X'F0'                                                  
         BNO   DELR26                                                           
         XC    IDSTATN(4),IDSTATN  SAVE 5-DIGIT STATION CODE AS BINARY          
         PACK  DUB,DELRSTA                                                      
         CVB   R1,DUB                                                           
         STCM  R1,15,IDSTATN                                                    
*                                                                               
DELR26   CLI   DELRMED,DELRMEDB                                                 
         BNE   DELR27                                                           
         MVI   IDSTATN+4,C'T'                                                   
         B     DELR28                                                           
DELR27   CLI   DELRMED,DELRMEDC                                                 
         BE    *+6                                                              
         DC    H'0'                INVALID MEDIA INPUT                          
         MVI   IDSTATN+4,C'C'                                                   
*                                                                               
* BUILD INTERIM RECORD FILTERABLE VALUES                                        
DELR28   MVI   INTRTYP,IDRTPEQ                                                  
*                                                                               
         OC    IDSTATN,IDSTATN     SPECIFIC STATION?                            
         BZ    DELR40                                                           
         LA    RE,BRDDNETT         CHECK SPECIAL BROADCAST STATIONS             
         USING BRDDNETD,RE                                                      
DELR29   CLI   0(RE),X'FF'                                                      
         BE    DELR29A                                                          
         CLC   BRDNIELS,IDSTATN                                                 
         BE    *+12                                                             
         LA    RE,BRDDNETL(RE)                                                  
         B     DELR29                                                           
         MVC   IDSTATN(4),BRDDDS   MOVE IN MODIFIED CALL LETTERS                
         DROP  RE                                                               
DELR29A  DS    0H                                                               
         MVC   INTSTA(4),IDSTATN   YES. FILL IN INTSTA                          
         CLI   IDMEDIA,IMEDBRO                                                  
         BNE   DELR30                                                           
         MVI   INTSTA+4,C'T'                                                    
         B     DELR40                                                           
DELR30   CLI   IDMEDIA,IMEDCAB                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   INTSTA+4,C'C'       INVALID INPUT MEDIA                          
*                                                                               
DELR40   MVC INTBOOK,IDBOOK                                                     
*                                                                               
* BUILD INTERIM RECORD DATA                                                     
         MVC   INTMEDIA,IDMEDIA                                                 
*                                                                               
         B     XIT                                                              
         DROP  R9                                                               
         EJECT                                                                  
**********************************************************************          
* PROCESS MINUTE INFORMATION RECORDS 'MI'                                       
**********************************************************************          
MIREC    NTR1                                                                   
         LA    R9,4(R9)                                                         
         USING MIRECD,R9                                                        
*                                                                               
         LA    RE,INTERD           CLEAR INTERIM RECORD AREA                    
         LA    RF,MAXIRECL                                                      
         XCEF                                                                   
*                                                                               
* BUILD INTERIM RECORD KEY                                                      
         USING MXMKEY,INTKEY                                                    
         MVI   MXMCODE,MXMCODEQ    MINUTE BY MINUTE 'M' RECORD                  
*                                                                               
         CLI   MIRSRC,MIRSRCP      CHECK SOURCE IS PROGRAM AVERAGE              
         BE    *+6                                                              
         DC    H'0'                ONLY SUPPORT PROG AVG FOR NOW                
*                                                                               
         CLI   MIRVCR,MIRVCRY      CHECK VCR FLAG                               
         BE    *+6                                                              
         DC    H'0'                ONLY SUPPORT VCR-INCLUDE FOR NOW             
*                                                                               
         CLI   MIRMED,MIRMEDB      FOR MEDIA BROADCAST                          
         BNE   MIR10                                                            
         MVI   MXMMEDIA,IMEDBRO    FILL IN KEY MEDIA                            
         MVI   MXMSTAT+4,C'T'      FILL IN STATION+4                            
         MVI   MXMBTYP,C'A'        BOOKTYPE FOR VCR-INCLUDE                     
         B     MIR20                                                            
MIR10    CLI   MIRMED,MIRMEDC      FOR MEDIA CABLE                              
         BNE   MIR15                                                            
         MVI   MXMMEDIA,IMEDCAB    FILL IN KEY MEDIA                            
         MVI   MXMSTAT+4,C'C'      FILL IN STATION+4                            
         B     MIR20                                                            
MIR15    DC    H'0'                INVALID INPUT MEDIA                          
*                                                                               
MIR20    LA    RE,VTYPTAB                                                       
         USING VTYPTABD,RE                                                      
MIR25    CLI   0(RE),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                INVALID INPUT VIEWING TYPE                   
         CLC   MIRVTYP,VTINREC                                                  
         BE    MIR27                                                            
         LA    RE,VTYPTABL(RE)                                                  
         B     MIR25                                                            
MIR27    MVC   MXMSRC,VTDEMREC     ADD VIEWING TYPE TO DEMO RECORD              
         DROP  RE                                                               
*                                                                               
MIR30    CLI   MIRMED,MIRMEDB      GET STATION CALL LETTERS                     
         BNE   MIR50                                                            
         LA    RE,BRDDNETT         CHECK SPECIAL BROADCAST STATIONS             
         USING BRDDNETD,RE                                                      
MIR35    CLI   0(RE),X'FF'                                                      
         BE    MIR50                                                            
         CLC   BRDNIELS,MIRSTA                                                  
         BE    MIR40                                                            
         LA    RE,BRDDNETL(RE)                                                  
         B     MIR35                                                            
MIR40    MVC   MXMSTAT(4),BRDDDS   MOVE IN MODIFIED CALL LETTERS                
         B     MIR60                                                            
         DROP  RE                                                               
MIR50    MVC   MXMSTAT(4),MIRSTA   MOVE CALL LETTERS/NUMERIC AS GIVEN           
         TM    MIRSTA+4,X'F0'                                                   
         BNO   MIR60                                                            
         XC    MXMSTAT(4),MXMSTAT  SAVE 5-DIGIT STATION CODE AS BINARY          
         PACK  DUB,MIRSTA                                                       
         CVB   R1,DUB                                                           
         STCM  R1,15,MXMSTAT                                                    
*                                                                               
MIR60    GOTO1 VNETWEEK,DMCB,MIRWEEK,VGETDAY,VADDAY   GET BOOK                  
         MVC   MXMBOOK+0(1),4(R1)  YEAR                                         
         MVC   MXMBOOK+1(1),8(R1)  WEEK #                                       
*                                                                               
         LA    RE,DAYTAB           GET INTERNAL DAY REPRESENTATION              
         USING DAYTABD,RE                                                       
MIR75    CLI   0(RE),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                INVALID INPUT DAY                            
         CLC   MIRDAY,DYINREC                                                   
         BE    MIR77                                                            
         LA    RE,DAYTABL(RE)                                                   
         B     MIR75                                                            
MIR77    MVC   MXMDAY,DYDEMREC                                                  
         DROP  RE                                                               
*                                                                               
         PACK  DUB,MIRMINUT        GET THE MINUTE NUMBER                        
         CVB   R0,DUB                                                           
         STCM  R0,3,INTMIN         MINUTE NUMBER (HHMM)                         
         SLL   R0,4                                                             
         STCM  R0,3,MXMMIN         FIRST 12 BITS IN THE KEY FIELD               
*                                                                               
         MVC   INTKSRC,MXMSRC      SOURCE                                       
*                                                                               
* BUILD INTERIM RECORD FILTERABLE VALUES                                        
         MVI   INTRTYP,MXMCODEQ    RECORD TYPE                                  
         MVC   INTMRKT,=AL2(103)   DCON SETTING FOR AMRLD                       
         MVC   INTSTA,MXMSTAT      STATION                                      
         MVC   INTBOOK,MXMBOOK     BOOK                                         
         MVC   INTBTYP,MXMBTYP     BOOK TYPE                                    
*                                                                               
* BUILD INTERIM RECORD DATA                                                     
         MVC   INTMEDIA,MXMMEDIA   MEDIA                                        
*                                                                               
         MVC   INTPNM10(3),=C'000'                                              
         MVC   INTPNM10+3(7),MIRPRGCD 10-CHAR PROGRAM CODE                      
*                                                                               
         MVC   PACK16(L'MIRPRGCD),MIRPRGCD                                      
         MVI   PACK16+L'MIRPRGCD,C'0'                                           
         PACK  DUB,PACK16(L'MIRPRGCD+1)                                         
         MVC   INTPNUM,DUB+2       5 CHAR PWOS                                  
*                                                                               
         PACK  DUB,MIRTKNUM                                                     
         CVB   R1,DUB                                                           
         STCM  R1,3,INTTRK         TRACK NUMBER                                 
*                                                                               
         PACK  DUB,MIRTCAST                                                     
         CVB   R1,DUB                                                           
         STCM  R1,15,INTTNUM       TELECAST NUMBER                              
*                                                                               
         MVC   INTDAY,MXMDAY       DAY                                          
*                                                                               
         PACK  DUB,MIRSTIME                                                     
         CVB   R0,DUB                                                           
         STCM  R0,3,INTSTIM        PROGRAM START TIME                           
         LTR   R0,R0                                                            
         BNZ   *+6                                                              
         DC    H'0'                CATCH INVALID TIME (060--2959)               
*                                                                               
         PACK  DUB,MIRETIME                                                     
         CVB   R0,DUB                                                           
         STCM  R0,3,INTETIM        PROGRAM END TIME                             
         LTR   R0,R0                                                            
         BNZ   *+6                                                              
         DC    H'0'                CATCH INVALID TIME (060--2959)               
*                                                                               
         PACK  DUB,MIRPRDUR                                                     
         CVB   R0,DUB                                                           
         STCM  R0,3,INTDURM        PROGRAM DURATION IN MINUTES                  
*                                                                               
         GOTO1 VHRTOQH,DMCB,INTSTIM,INTSQH   START QUARTER HOUR                 
*                                                                               
*        DEVELOP END QUARTER HOUR                                               
*        DURATION IN QUARTER HOURS = 1 ( 1-22 MIN.)                             
*                                  = 2 (23-37 MIN.)                             
*                                  = 3 (38-52 MIN.)...ETC.                      
         ZIC   R0,INTSQH           START QUARTER HOUR                           
         SR    RF,RF                                                            
         ICM   RF,3,INTDURM                                                     
         LA    RF,8(RF)            ADD 8 BEFORE CNV TO QH                       
         SR    RE,RE                                                            
         D     RE,=F'15'                                                        
         CH    RF,=H'1'            TEST FOR DURATION OF AT LEAST ONE            
         BH    *+8                                                              
         LA    RF,1                DURATION IN QH                               
         AR    R0,RF               SQH PLUS DUR                                 
         BCTR  R0,0                LESS ONE GIVE END QH                         
         STC   R0,INTEQH                                                        
*                                                                               
         MVC   INTPNAME,MIRPRNAM   PROGRAM NAME                                 
         MVC   INTTRNAM,MIRTKNAM   TACK NAME                                    
         MVC   INTEPNAM,MIREPNAM   EPISODE NAME                                 
*                                                                               
         MVC   INTPTYP,MIRPTYP     PROGRAM TYPE                                 
         MVC   INTSBTYP,MIRSPTYP   PROGRAM SUBTYPE                              
*                                                                               
         MVI   INTLIVE,C' '        LIVE PROGRAM INDICATOR                       
         CLI   MIRLIVE,MIRLIVEY                                                 
         BNE   *+8                                                              
         MVI   INTLIVE,INTLIVEY    (MATCHES THE MIT TAPE)                       
*                                                                               
         MVI   INTPREM,C' '        PREMIERE INDICATOR                           
         CLI   MIRPREM,MIRPREMY                                                 
         BNE   *+8                                                              
         MVI   INTPREM,INTPREMY    (MATCHES THE MIT TAPE)                       
*                                                                               
         MVI   INTGAPD,C' '        GAPPED INDICATOR                             
         CLI   MIRGAPD,MIRGAPDY                                                 
         BNE   *+8                                                              
         MVI   INTGAPD,INTGAPDY    (MATCHES THE MIT TAPE)                       
*                                                                               
         PACK  DUB,MIRSCNT                                                      
         CVB   R0,DUB                                                           
         STCM  R0,3,INTSTAC        STATION COUNT                                
*                                                                               
         MVC   INTFEED,MIRFEED     FEED PATTERN                                 
         MVC   INTAUDES,MIRAETYP   AUDIENCE ESTIMATE TYPE                       
*                                                                               
         PACK  DUB,MIRCOV                                                       
         CVB   R0,DUB                                                           
         STCM  R0,3,INTCOV         PROGRAM COVERAGE PERCENT                     
*                                                                               
         CLI   MIRSPEC,MIRSPECY                                                 
         BNE   MIR90                                                            
         OI    INTDTYP,INTDTSPE    SPECIAL INDICATOR                            
         B     MIR100                                                           
MIR90    OI    INTDTYP,INDTREG     REGULAR INDICATOR (NON-SPECIAL)              
*                                                                               
MIR100   DS    0X                                                               
*&&DO                                                                           
* MIRDTYP IS AVAILABLE ONLY FOR SYNDICATION                                     
         CLI   MIRDTYP,MIRDTYPO    DETERMINE DATA TYPE FLAGS                    
         BNE   MIR102                                                           
         OI    INTDTYP,INTDTORG    ORIGINAL INDICATOR                           
         B     MIR110                                                           
MIR102   CLI   MIRDTYP,MIRDTYPR                                                 
         BNE   MIR104                                                           
         OI    INTDTYP,INTDTREP    REPEAT INDICATOR                             
         B     MIR110                                                           
MIR104   CLI   MIRDTYP,MIRDTYPM                                                 
         BNE   MIR106                                                           
         OI    INTDTYP,INTDTMUL    MULTIPLE INDICATOR                           
         B     MIR110                                                           
MIR106   CLI   MIRDTYP,MIRDTYPC                                                 
         BNE   MIR108                                                           
         OI    INTDTYP,INTDTORG+INTDTREP   COMBINED=ORIGINAL+REPEAT             
         B     MIR110                                                           
*&&                                                                             
MIR108   CLI   MIRDTYP,C' '                                                     
         BE    *+6                                                              
         DC    H'0'                INVALID INPUT DATA TYPE                      
*                                                                               
MIR110   CLI   MIRBKOT,MIRBKOTY    BREAKOUT INDICATOR                           
         BNE   *+8                                                              
         MVI   INTBREAK,INTBREKY                                                
*                                                                               
         LA    RE,DAYPTAB          GET DAYPART CODE                             
         USING DAYPTABD,RE                                                      
MIR115   CLI   0(RE),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                INVALID INPUT DAYPART                        
         CLC   MIRDPT,DYPINREC                                                  
         BE    MIR117                                                           
         LA    RE,DAYPTABL(RE)                                                  
         B     MIR115                                                           
MIR117   MVC   INTDPT,DYPDMREC     2-CHARACTER DAYPART CODE                     
         DROP  RE                                                               
*                                                                               
         CLI   MIRREPT,MIRREPTY    FLAGS                                        
         BNE   MIR120                                                           
         OI    INTFLAGS,INTFREP    REPEAT INDICATOR                             
         OI    INTDTYP,INTDTREP    REPEAT INDICATOR FOR BROADCAST               
         B     MIR125               AND CABLE                                   
MIR120   OI    INTDTYP,INTDTORG    ORIGINAL INDICATOR                           
*                                                                               
MIR125   CLI   MIRMSEG,MIRMSEGY                                                 
         BNE   *+8                                                              
         OI    INTFLAGS,INTMSEG    MULTI-SEGMENT INDICATOR                      
         CLI   MIRATUM,MIRATUMY                                                 
         BNE   *+8                                                              
         OI    INTFLAGS,INTATUM    AT/UMBRELLA INDICATOR                        
*                                                                               
         PACK  DUB,MIRMTNUM                                                     
         CVB   R0,DUB                                                           
         STC   R0,INTCTNUM         CONTRIBUTING TELECASTS FOR MULTI-SEG         
*                                                                               
         L     RE,ARREC            INPUT RECORD AREA                            
         SR    RF,RF                                                            
         ICM   RF,3,0(RE)          INPUT RECORD LENGTH                          
         CHI   RF,MIRECORL         CHECK RECORD LENGTH TO MAKE SURE             
         BL    MIR127               THE ORDERED SUSTAINER FIELD IS              
         CLI   MIRORDST,MIRORSY     AVAILABLE                                   
         BNE   *+8                                                              
         OI    INTFLAGS,INTORDS    ORDERED SUSTAINER INDICATOR                  
*                                                                               
MIR127   PACK  DUB,MIRMOP                                                       
         CVB   R0,DUB                                                           
         STCM  R0,3,INTMOP         MINUTE OF PROGRAM (MOP)                      
*                                                                               
         CLI   MIRREPR,MIRREPRY    REPROCESSING RECORDS                         
         BNE   MIR130                                                           
         GOTO1 VDATCON,DMCB,(0,MIRRDATE),(3,INTRPRDT) REPROCESSING DATE         
         CLI   MIRRPTYP,MIRRPDES   REPROCESSING TYPE                            
         BNE   *+12                                                             
         MVI   INTRPTYP,INTRPTDS   CHANGE TO DESCRIPTIVE DATA                   
         B     MIR130                                                           
         CLI   MIRRPTYP,MIRRPVW                                                 
         BNE   *+12                                                             
         MVI   INTRPTYP,INTRPTVW   CHANGE TO VIEWING DATA                       
         B     MIR130                                                           
         CLI   MIRRPTYP,MIRRPNA                                                 
         BE    *+6                                                              
         DC    H'0'                INVALID INPUT REPROCESSING TYPE              
         MVI   INTRPTYP,INTRPTNA   CHANGE NOT IDENTIFIED                        
*                                                                               
MIR130   MVC   INTCOMM,MIRCOMM     COMMERCIAL FLAG                              
*                                                                               
         CLI   MIRCOMM,MIRCOMMC    FIELDS BELOW APPLY ONLY TO                   
         BNE   MIR140              MINUTES WITH COMMERCIALS                     
*                                                                               
         PACK  DUB,MIRCMSEC                                                     
         CVB   R0,DUB                                                           
         STC   R0,INTCMSEC         NUMBER OF COMMERCIAL SECONDS                 
*                                                                               
         PACK  DUB,MIRPRSEC                                                     
         CVB   R0,DUB                                                           
         STC   R0,INTPRSEC         NUMBER OF PROMO SECONDS                      
*                                                                               
         PACK  DUB,MIRPSSEC                                                     
         CVB   R0,DUB                                                           
         STC  R0,INTPSSEC          NUMBER OF PSA SECONDS                        
*                                                                               
         PACK  DUB,MIRPODST                                                     
         CVB   R0,DUB                                                           
         STCM  R0,3,INTPODST       POD START TIME HHMM                          
         LTR   R0,R0                                                            
         BNZ   *+6                                                              
         DC    H'0'                CATCH INVALID TIME (060--2959)               
*                                                                               
         PACK  DUB,MIRPODMN                                                     
         CVB   R0,DUB                                                           
         STCM  R0,3,INTPODMN       POD DURATION IN MINUTES                      
*                                                                               
         PACK  DUB,MIRPODSC                                                     
         CVB   R0,DUB                                                           
         STCM  R0,15,INTPODSC      POD TOTAL COMMERCIAL SECONDS                 
*                                                                               
         PACK  DUB,MIRPODTS                                                     
         CVB   R0,DUB                                                           
         STCM  R0,15,INTPODTS      POD TOTAL SECONDS                            
*                                                                               
         PACK  DUB,MIRPODNO                                                     
         CVB   R0,DUB                                                           
         STCM  R0,3,INTPODNO       POD NUMBER                                   
*                                                                               
         PACK  DUB,MIRFLFLG                                                     
         CVB   R0,DUB                                                           
         STC   R0,INTFL            FIRST/LAST MINUTE FLAG                       
*                                                                               
MIR140   DS    0X                                                               
*                                                                               
MIRECX   CLC   INTSTA,PRVSTA       PRINT OUT THE STATION                        
         BE    MIRECXX                                                          
         MVC   PRVSTA,INTSTA                                                    
         MVC   P,SPACES                                                         
         MVC   P+0(L'INTMEDIA),INTMEDIA  MEDIA                                  
         MVC   P+4(L'INTKSRC),INTKSRC    SOURCE                                 
         MVC   P+8(L'INTBTYP),INTBTYP    BOOKTYPE                               
         MVC   FULL+1(2),=X'0101'        ANY VALID MONTH/DAY WILL DO            
         MVC   FULL(1),INTBOOK           YEAR                                   
         GOTO1 VDATCON,DMCB,(3,FULL),(20,DUB)  YYYYMMDD                         
         MVC   P+13(4),DUB               YYYY                                   
         MVI   P+17,C'/'                                                        
         EDIT  (B1,INTBOOK+1),(2,P+18),FILL=0   WEEK #                          
         MVC   P+21(L'INTSTA),INTSTA                                            
         IF  (CLI,INTSTA,EQ,0)                                                  
           EDIT  (4,INTSTA),(5,P+21)     EDIT 5-DIGIT BINARY STA. CODE          
           MVC   P+26(1),INTSTA+4                                               
         ENDIF ,                                                                
*                                                                               
         GOTO1 VDEPRNT2                                                         
*                                                                               
MIRECXX  B     XIT                                                              
         DROP  R9                                                               
         EJECT                                                                  
**********************************************************************          
* PROCESS MINUTE DEMO VALUES RECORDS 'MD'                                       
**********************************************************************          
MDREC    NTR1                                                                   
         LA    R9,4(R9)                                                         
         USING MDRECD,R9                                                        
*                                                                               
         CLI   PRVRTYP,MIRTYPQ                                                  
         BE    *+6                                                              
         DC    H'0'                INVALID PREVIOUS INPUT RECORD TYPE           
*                                                                               
         CLI   INTMEDIA,IMEDBRO    BROADCAST                                    
         BNE   MDR50                                                            
*                                                                               
         CLI   MDRMOD,MDRMODY                                                   
         BNE   MDR10                                                            
         LA    R4,BYSLOT           TABLE OF DISPLACEMENTS FOR BROADCAST         
         B     MDR100              IMPRESSIONS                                  
MDR10    CLI   MDRMOD,MDRMODZ                                                   
         BNE   MDR20                                                            
         LA    R4,BZSLOT           TABLE OF DISPLACEMENTS FOR BROADCAST         
         B     MDR100              PROGRAM PUTS                                 
MDR20    CLI   MDRMOD,MDRMODB                                                   
         BE    *+6                                                              
         DC    H'0'                INVALID INPUT MODIFIER                       
         LA    R4,BBSLOT           TABLE OF DISPLACEMENTS FOR BROADCAST         
         B     MDR100              PROGRAM PUTS                                 
*                                                                               
MDR50    CLI   INTMEDIA,IMEDCAB    CABLE                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   MDRMOD,MDRMODY                                                   
         BNE   MDR60                                                            
         LA    R4,CYSLOT           TABLE OF DISPLACEMENTS FOR CABLE             
         B     MDR100              IMPRESSIONS                                  
MDR60    CLI   MDRMOD,MDRMODZ                                                   
         BE    *+6                                                              
         DC    H'0'                INVALID INPUT MODIFIER                       
         LA    R4,CZSLOT           TABLE OF DISPLACEMENTS FOR CABLE             
         B     MDR100              PROGRAM PUTS                                 
*                                                                               
MDR100   BAS   RE,SLOTDEM          SLOT DEMOS ON THE INTERIM RECORD             
*                                                                               
MDRECX   B     XIT                                                              
         DROP  R9                                                               
         EJECT                                                                  
**********************************************************************          
* PROCESS PASSIVE RECORDS FOR PROGRAM TIMES 'PT'                                
**********************************************************************          
PTREC    NTR1                                                                   
         LA    R9,4(R9)                                                         
         USING PTRECD,R9                                                        
         LA    RE,INTERD           CLEAR INTERIM RECORD AREA                    
         LA    RF,MAXIRECL                                                      
         XCEF                                                                   
*                                                                               
         CLI   PTRSRC,PTRSRCP      CHECK SOURCE IS PROGRAM AVERAGE              
         BE    *+6                                                              
         DC    H'0'                ONLY SUPPORT PROG AVG FOR NOW                
*                                                                               
* BUILD INTERIM RECORD KEY                                                      
         USING MPTKEY,INTKEY                                                    
         MVI   MPTCODE,MPTCODEQ    'T' RECORDS                                  
*                                                                               
         CLI   PTRMED,PTRMEDB      FOR MEDIA BROADCAST                          
         BNE   PTR10                                                            
         MVI   MPTMEDIA,IMEDBRO    FILL IN KEY MEDIA                            
         MVI   MPTSTAT+4,C'T'      FILL IN STATION+4                            
         MVI   MPTBTYP,C'A'        BOOKTYPE FOR VCR-INCLUDE                     
         B     PTR20                                                            
PTR10    CLI   PTRMED,PTRMEDC      FOR MEDIA CABLE                              
         BNE   PTR15                                                            
         MVI   MPTMEDIA,IMEDCAB    FILL IN KEY MEDIA                            
         MVI   MPTSTAT+4,C'C'      FILL IN STATION+4                            
         B     PTR20                                                            
PTR15    DC    H'0'                INVALID INPUT MEDIA                          
*                                                                               
PTR20    MVI   MPTSRC,SRCLIVE      ONLY LIVE VIEWING TYPE                       
*                                                                               
         GOTO1 VNETWEEK,DMCB,PTRWEEK,VGETDAY,VADDAY   GET BOOK                  
         MVC   MPTBOOK+0(1),4(R1)  YEAR                                         
         MVC   MPTBOOK+1(1),8(R1)  WEEK #                                       
*                                                                               
PTR30    CLI   PTRMED,PTRMEDB      GET STATION CALL LETTERS                     
         BNE   PTR50                                                            
         LA    RE,BRDDNETT         CHECK SPECIAL BROADCAST STATIONS             
         USING BRDDNETD,RE                                                      
PTR35    CLI   0(RE),X'FF'                                                      
         BE    PTR50                                                            
         CLC   BRDNIELS,PTRSTA                                                  
         BE    PTR40                                                            
         LA    RE,BRDDNETL(RE)                                                  
         B     PTR35                                                            
PTR40    MVC   MPTSTAT(4),BRDDDS   MOVE IN MODIFIED CALL LETTERS                
         B     PTR60                                                            
         DROP  RE                                                               
PTR50    MVC   MPTSTAT(4),PTRSTA   MOVE CALL LETTERS/NUMERIC AS GIVEN           
         TM    PTRSTA+4,X'F0'                                                   
         BNO   PTR60                                                            
         XC    MPTSTAT(4),MPTSTAT  SAVE 5-DIGIT STATION CODE AS BINARY          
         PACK  DUB,PTRSTA                                                       
         CVB   R1,DUB                                                           
         STCM  R1,15,MPTSTAT                                                    
*                                                                               
PTR60    CLI   PTRBKOT,PTRBKOTY    STATION TYPE (BREAKOUT INDICATOR)            
         BNE   *+10                                                             
         MVC   MPTSTYP,PTRBKOT                                                  
*                                                                               
         LA    RE,DAYTAB           GET INTERNAL DAY REPRESENTATION              
         USING DAYTABD,RE                                                       
PTR75    CLI   0(RE),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                INVALID INPUT DAY                            
         CLC   PTRDAY,DYINREC                                                   
         BE    PTR77                                                            
         LA    RE,DAYTABL(RE)                                                   
         B     PTR75                                                            
PTR77    MVC   MPTDAY,DYDEMREC                                                  
         DROP  RE                                                               
*                                                                               
         PACK  DUB,PTRSTIME        START TIME                                   
         CVB   R0,DUB                                                           
         STCM  R0,3,INTSTIM                                                     
         LTR   R0,R0                                                            
         BNZ   *+6                                                              
         DC    H'0'                CATCH INVALID TIME (060--2959)               
         SLL   R0,4                FIRST 12 BITS IN KEY FIELD                   
         STCM  R0,3,MPTTIMES                                                    
*                                                                               
         PACK  DUB,PTRETIME        END TIME                                     
         CVB   R0,DUB                                                           
         STCM  R0,3,INTETIM                                                     
         LTR   R0,R0                                                            
         BNZ   *+6                                                              
         DC    H'0'                CATCH INVALID TIME (060--2959)               
         LR    RE,R0               LAST 12 BITS IN KEY FIELD:                   
         SRL   RE,8                FIRST 4 BITS OF END TIME                     
         ZIC   R1,MPTTIMES+1       LAST 4 BITS OF START TIME                    
         OR    R1,RE               MERGE THEM                                   
         STC   R1,MPTTIMES+1                                                    
         STC   R0,MPTTIMES+2       LAST 8 BITS OF END TIME                      
*                                                                               
         OI    MPTKSTA,X'40'       EXTENDED PASSIVE INDICATOR                   
*                                                                               
         LA    RE,MINTYPT          GET INTERNAL MINUTE TYPE VALUE               
         USING MINTYPTD,RE                                                      
PTR80    CLI   0(RE),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                INVALID INPUT MINUTE TYPE                    
         CLC   PTRMTYP,MTYINREC                                                 
         BE    PTR85                                                            
         LA    RE,MINTYPTL(RE)                                                  
         B     PTR80                                                            
PTR85    MVC   MPTMTYP,MTYDMREC                                                 
         DROP  RE                                                               
*                                                                               
         MVC   INTKSRC,MPTSRC      SOURCE                                       
*                                                                               
* BUILD INTERIM RECORD FILTERABLE VALUES                                        
         MVI   INTRTYP,MPTCODEQ    RECORD TYPE                                  
         MVC   INTSTA,MPTSTAT      STATION                                      
         MVC   INTBOOK,MPTBOOK     BOOK                                         
         MVC   INTSTYP,MPTSTYP     STATION TYPE (BREAKOUT INDICATOR)            
         MVC   INTBTYP,MPTBTYP     BOOK TYPE                                    
*                                                                               
* BUILD INTERIM RECORD DATA                                                     
         MVC   INTMEDIA,MPTMEDIA   MEDIA                                        
*                                                                               
         MVC   INTPNM10(3),=C'000'                                              
         MVC   INTPNM10+3(7),PTRPRGCD 10-CHAR PROGRAM CODE                      
*                                                                               
         MVC   PACK16(L'MIRPRGCD),PTRPRGCD      PROGRAM CODE                    
         MVI   PACK16+L'MIRPRGCD,C'0'                                           
         PACK  DUB,PACK16(L'PTRPRGCD+1)                                         
         MVC   INTPNUM,DUB+2       5 CHAR PWOS                                  
*                                                                               
         MVC   INTDAY,MPTDAY                                                    
*                                                                               
         MVC   INTCOMM,MPTMTYP                                                  
*                                                                               
         B     XIT                                                              
         DROP  R9                                                               
         EJECT                                                                  
**********************************************************************          
* PROCESS PASSIVE RECORDS FOR COMMERCIAL PODS 'PD'                              
**********************************************************************          
PDREC    NTR1                                                                   
         LA    R9,4(R9)                                                         
         USING PDRECD,R9                                                        
         LA    RE,INTERD           CLEAR INTERIM RECORD AREA                    
         LA    RF,MAXIRECL                                                      
         XCEF                                                                   
*                                                                               
         CLI   PDRSRC,PDRSRCP      CHECK SOURCE IS PROGRAM AVERAGE              
         BE    *+6                                                              
         DC    H'0'                ONLY SUPPORT PROG AVG FOR NOW                
*                                                                               
* BUILD INTERIM RECORD KEY                                                      
         USING MPDKEY,INTKEY                                                    
         MVI   MPDCODE,MPDCODEQ    'D' RECORDS                                  
*                                                                               
         CLI   PDRMED,PDRMEDB      FOR MEDIA BROADCAST                          
         BNE   PDR10                                                            
         MVI   MPDMEDIA,IMEDBRO    FILL IN KEY MEDIA                            
         MVI   MPDSTAT+4,C'T'      FILL IN STATION+4                            
         MVI   MPDBTYP,C'A'        BOOKTYPE FOR VCR-INCLUDE                     
         B     PDR20                                                            
PDR10    CLI   PDRMED,PDRMEDC      FOR MEDIA CABLE                              
         BNE   PDR15                                                            
         MVI   MPDMEDIA,IMEDCAB    FILL IN KEY MEDIA                            
         MVI   MPDSTAT+4,C'C'      FILL IN STATION+4                            
         B     PDR20                                                            
PDR15    DC    H'0'                INVALID INPUT MEDIA                          
*                                                                               
PDR20    MVI   MPDSRC,SRCLIVE      ONLY LIVE VIEWING TYPE O RECORD              
*                                                                               
         GOTO1 VNETWEEK,DMCB,PDRWEEK,VGETDAY,VADDAY   GET BOOK                  
         MVC   MPDBOOK+0(1),4(R1)  YEAR                                         
         MVC   MPDBOOK+1(1),8(R1)  WEEK #                                       
*                                                                               
PDR30    CLI   PDRMED,PDRMEDB      GET STATION CALL LETTERS                     
         BNE   PDR50                                                            
         LA    RE,BRDDNETT         CHECK SPECIAL BROADCAST STATIONS             
         USING BRDDNETD,RE                                                      
PDR35    CLI   0(RE),X'FF'                                                      
         BE    PDR50                                                            
         CLC   BRDNIELS,PDRSTA                                                  
         BE    PDR40                                                            
         LA    RE,BRDDNETL(RE)                                                  
         B     PDR35                                                            
PDR40    MVC   MPDSTAT(4),BRDDDS   MOVE IN MODIFIED CALL LETTERS                
         B     PDR70                                                            
         DROP  RE                                                               
PDR50    MVC   MPDSTAT(4),PDRSTA   MOVE CALL LETTERS/NUMERIC AS GIVEN           
         TM    PDRSTA+4,X'F0'                                                   
         BNO   PDR70                                                            
         XC    MPDSTAT(4),MPDSTAT  SAVE 5-DIGIT STATION CODE AS BINARY          
         PACK  DUB,PDRSTA                                                       
         CVB   R1,DUB                                                           
         STCM  R1,15,MPDSTAT                                                    
*                                                                               
PDR70    LA    RE,DAYTAB           GET INTERNAL DAY REPRESENTATION              
         USING DAYTABD,RE                                                       
PDR75    CLI   0(RE),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                INVALID INPUT DAY                            
         CLC   PDRDAY,DYINREC                                                   
         BE    PDR77                                                            
         LA    RE,DAYTABL(RE)                                                   
         B     PDR75                                                            
PDR77    MVC   MPDDAY,DYDEMREC                                                  
         DROP  RE                                                               
*                                                                               
         PACK  DUB,PDRETIME        END TIME                                     
         CVB   R0,DUB                                                           
         STCM  R0,3,INTETIM                                                     
         LTR   R0,R0                                                            
         BNZ   *+6                                                              
         DC    H'0'                CATCH INVALID TIME (060--2959)               
*                                                                               
         SLL   R0,4                FIRST 12 BITS OF KEY FIELD                   
         STCM  R0,3,MPDTIMES                                                    
*                                                                               
         PACK  DUB,PDRSTIME        START TIME                                   
         CVB   R0,DUB                                                           
         STCM  R0,3,INTSTIM                                                     
         LTR   R0,R0                                                            
         BNZ   *+6                                                              
         DC    H'0'                CATCH INVALID TIME (060--2959)               
         LR    RE,R0               LAST 12 BITS OF KEY FIELD:                   
         SRL   RE,8                FIRST 4 BITS OF START TIME                   
         ZIC   R1,MPDTIMES+1       LAST 4 BITS OF END TIME                      
         OR    R1,RE               MERGE THEM                                   
         STC   R1,MPDTIMES+1                                                    
         STC   R0,MPDTIMES+2       LAST 8 BITS OF START TIME                    
*                                                                               
         MVC   INTKSRC,MPDSRC      SOURCE                                       
*                                                                               
* BUILD INTERIM RECORD FILTERABLE VALUES                                        
         MVI   INTRTYP,MPDCODEQ    RECORD TYPE                                  
         MVC   INTSTA,MPDSTAT      STATION                                      
         MVC   INTBOOK,MPDBOOK     BOOK                                         
         MVC   INTBTYP,MPDBTYP     BOOK TYPE                                    
*                                                                               
* BUILD INTERIM RECORD DATA                                                     
         MVC   INTMEDIA,MPDMEDIA   MEDIA                                        
*                                                                               
         MVC   INTPNM10(3),=C'000'                                              
         MVC   INTPNM10+3(7),PDRPRGCD 10-CHAR PROGRAM CODE                      
*                                                                               
         MVC   PACK16(L'MIRPRGCD),PDRPRGCD      PROGRAM CODE                    
         MVI   PACK16+L'MIRPRGCD,C'0'                                           
         PACK  DUB,PACK16(L'PDRPRGCD+1)                                         
         MVC   INTPNUM,DUB+2       5 CHAR PWOS                                  
*                                                                               
         MVC   INTDAY,MPDDAY                                                    
*                                                                               
         B     XIT                                                              
         DROP  R9                                                               
         EJECT                                                                  
**********************************************************************          
* SLOT DEMO VALUES INTO THEIR RESPECTIVE SLOTS ON THE INTERIM RECORD.           
* AT ENTRY, R4 POINTS TO THE TABLE OF INPUT/OUTPUT DISPLACEMENTS.               
*           R9 POINTS TO THE 'MD' DEMO VALUES INPUT RECORD.                     
**********************************************************************          
SLOTDEM  NTR1                                                                   
         USING SLOTTABD,R4                                                      
         USING MDRECD,R9                                                        
*                                                                               
SLD10    CLC   =X'FFFF',0(R4)                                                   
         BE    SLOTDEMX             FINISHED SLOTTING DEMOS                     
*                                                                               
         ZIC   RE,SLINDSP           DISPLACEMENT OF DEMO ON INPUT RECD          
         SHI   RE,1                 -1 BECAUSE IT STARTS AT 1                   
         MHI   RE,L'MDRDEMS                                                     
         LA    RF,MDRDEMS                                                       
         AR    RF,RE                DEMO SLOT ON THE INPUT RECORD               
*                                                                               
         SR    R1,R1                                                            
         ICM   R1,3,SLOUTDSP        DISPLACEMENT OF DEMO ON INTERIM REC         
         MHI   R1,L'INTACCS                                                     
         LA    R2,INTACCS                                                       
         AR    R2,R1                DEMO SLOT ON THE INTERIM RECORD             
*                                                                               
         PACK  DUB,0(L'MDRDEMS,RF)  CONVERT DEMO VALUE TO BINARY                
         CVB   R0,DUB                                                           
         STCM  R0,15,0(R2)          SAVE IT ON THE INTERIM RECORD               
*                                                                               
         LA    R4,SLOTTABL(R4)                                                  
         B     SLD10                                                            
*                                                                               
SLOTDEMX B     XIT                                                              
         DROP  R4,R9                                                            
         EJECT                                                                  
**********************************************************************          
* COMPUTE THE HOME SHARE AS                                                     
* SHOMES = YHOMES * 100 * 10 / ZHOMES.                                          
* THE MULTIPLICATION BY 10 ABOVE IS NEEDED BECAUSE THE SHARE IS STORED          
* WITH ONE DECIMAL.                                                             
**********************************************************************          
HOMSHARE NTR1                                                                   
*                                                                               
         CLI   INTMEDIA,IMEDBRO    ONLY FOR BROADCAST                           
         BNE   HSHX                                                             
*                                                                               
         LHI   R0,IBZHOMES                                                      
         MHI   R0,L'INTACCS                                                     
         LA    R2,INTACCS                                                       
         AR    R2,R0               LOCATION OF ZHOMES                           
*                                                                               
         ICM   R1,15,0(R2)         NO ZHOMES, CAN'T COMPUTE SHARE               
         BZ    HSHX                                                             
*                                                                               
         LHI   RF,IBYHOMES                                                      
         MHI   RF,L'INTACCS                                                     
         LA    RE,INTACCS                                                       
         AR    RE,RF               LOCATION OF YHOMES                           
         ICM   RF,15,0(RE)         YHOMES VALUE                                 
*                                                                               
         SR    RE,RE                                                            
         M     RE,=A(1000)         YHOMES*1000                                  
         DR    RE,R1               YHOMES*1000/ZHOMES                           
*                                                                               
         MHI   RE,2                PERFORM ROUNDING                             
         CR    RE,R1                                                            
         BL    *+8                                                              
         AHI   RF,1                                                             
*                                                                               
         LHI   R0,IBSHOMES                                                      
         MHI   R0,L'INTACCS                                                     
         LA    R2,INTACCS                                                       
         AR    R2,R0               LOCATION OF SHOMES                           
         STCM  RF,15,0(R2)         STORE COMPUTED SHOMES                        
*                                                                               
HSHX     B     XIT                                                              
         EJECT                                                                  
**********************************************************************          
* COMPUTE THE VPH VALUES FOR BROADCAST                                          
* GET THE INTABS VALUES FROM FILE, THEN CALL DECALVPH                           
**********************************************************************          
COMPVPH  NTR1                                                                   
*                                                                               
         CLI   INTMEDIA,IMEDBRO                                                 
         BNE   CMPVX                                                            
*                                                                               
         CLC   PRVMXMBK,INTBOOK     IF SAME BOOK, ALREADY HAVE INTABS           
         BE    CMPV30               USE INTAB TABLE                             
         MVC   PRVMXMBK,INTBOOK                                                 
*                                                                               
         L     R1,=A(INTAB)        READ INTABS RECDS AND CREATE TABLE           
         USING INTABD,R1                                                        
         MVC   DATADISP,=Y(PRFRSTEL-PRKEY)                                      
         MVI   ELCODE,X'21'        NAME ELEMENT (HAS DATE IN IT)                
         LA    R4,X'40'            READ 7-DAYS STARTING WITH MONDAY             
*                                                                               
CMPV10   LA    RF,INTBKEY          BUILD KEY OF THE INTAB RECORDS               
         XC    INTBKEY,INTBKEY                                                  
         USING PRKEY,RF                                                         
         MVI   PRCODE,C'P'                                                      
         MVI   PRMED,C'N'                                                       
         MVI   PRSRC,C'N'          ONLY LIVE SAMPLE COUNTS ON FILE              
         MVC   PRSTAT,=C'ITABT'                                                 
         MVC   PRBOOK,INTBOOK      FOR CURRENT WEEK                             
         MVC   PRBTYP,INTBTYP                                                   
         STC   R4,PRDW                                                          
*                                                                               
         LA    RE,INTABCNT                                                      
         ST    RE,ASLOT            SLOT INTO INTABCNT                           
         DROP  RF                                                               
         BAS   RE,RESLOT           READ RECD IN AND SLOT DEMOS                  
         BNZ   CMPV20              THIS DAY NOT FOUND--TRY NEXT ONE             
         L     R6,ASREC                                                         
         LA    R6,4(R6)                                                         
         USING PRKEY,R6                                                         
         MVC   INTABDAY,PRDW       SAVE DAY OF WEEK                             
         BAS   RE,GETEL            GET NAME ELEMENT FOR DATE                    
         MVC   INTABDAT,2(R6)      DATE                                         
         LA    R1,L'INTABTAB(R1)   NEXT INTAB BUCKET AREA                       
CMPV20   SRL   R4,1                NEXT DAY BIT                                 
         LTR   R4,R4                                                            
         BNZ   CMPV10                                                           
         DROP  R6                                                               
*                                                                               
CMPV30   LA    R2,CALVPH           CALVPH BLOCK                                 
         USING CALVPHD,R2                                                       
         LA    RE,INTACCS                                                       
         ST    RE,VPHDEMS          A(RECORD DEMOS)                              
         L     RE,=A(INTAB)                                                     
         ST    RE,VPHINT           A(INTAB VALUES)                              
         MVC   VPHDPT,INTDPT       DAYPART CODE                                 
         MVC   VPHPCOD,INTPTYP     2-CHAR PROGRAM TYPE                          
         MVI   VPHSPCL,C' '        SPECIAL INDICATOR                            
         TM    INTDTYP,INTDTSPE                                                 
         BNO   *+8                                                              
         MVI   VPHSPCL,C'S'                                                     
*                                                                               
         LHI   R0,X'80'            GET DAY CODE                                 
         ZIC   R1,INTDAY                                                        
         SRL   R0,1                                                             
         BCT   R1,*-4                                                           
         STC   R0,VPHACTD          WEEK DAY BITS. MON=X'40'...SUN=X'01'         
         CLI   VPHACTD,0                                                        
         BNE   *+8                                                              
         MVI   VPHACTD,X'40'                                                    
*                                                                               
         MVI   VPHTELE,1                                                        
         MVC   VPHDUR,INTDURM                                                   
         DROP  R2                                                               
*                                                                               
         GOTO1 VCALVPH,CALVPH      CALL CALVPH TO COMPUTE VPHS                  
*                                                                               
CMPVX    B     XIT                                                              
         EJECT                                                                  
**********************************************************************          
*RESLOT- READ RECD FROM FILE USING KEY IN INTBKEY                               
*        SLOT DEMOS INTO THE APPROPRIATE BUCKETS                                
**********************************************************************          
RESLOT   NTR1                                                                   
         MVC   SVKEY,INTBKEY                                                    
         L     R6,ASREC                                                         
         USING PRKEY,R6                                                         
         LA    R6,4(R6)                                                         
         GOTO1 VDATAMGR,DMCB,=C'DMRDHI ',=C'NTIDIR',INTBKEY,(R6)                
         CLI   DMCB+8,0            ANY READ ERRORS                              
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   INTBKEY(18),0(R6)   KEY READ IN                                  
         BE    *+6                                                              
         DC    H'0'                NOT FOUND                                    
         MVC   SVDA,PRNDXDA-PRKEY(R6)                                           
         MVC   SVSTATUS,PRKSTAT-PRKEY(R6)                                       
         MVC   INTBKEY,SVKEY       RESTORE SAVED KEY                            
         MVC   0(L'SVKEY,R6),SVKEY                                              
         MVC   PRRSTAT-PRKEY(1,R6),SVSTATUS                                     
         GOTO1 VDATAMGR,DMCB,=C'DMRDHI ',=C'NTIFIL',SVDA,(R6)                   
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   SVKEY(20),0(R6)     SAME KEY?                                    
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R6,PRFRSTEL-PRKEY(R6) PT TO 1ST ELEMENT                          
         SR    R0,R0                                                            
RESL10   CLI   0(R6),X'00'         ENDREC?                                      
         BNE   *+6                                                              
         DC    H'0'                END OF REC --NO '5E' ELEMENT???              
         CLI   0(R6),X'41'                                                      
         BE    RESL15              DEMO ELEMENT                                 
         IC    R0,1(R6)            BUMP TO NEXT ELEMENT                         
         AR    R6,R0               NEXT ELEMENT                                 
         B     RESL10                                                           
*                                                                               
RESL15   ZIC   R0,1(R6)                                                         
         AR    R0,R6                                                            
         ST    R0,DMCB             ADDR OF NEXT ELEM                            
         MVC   DMCB+4(1),2(R6)     SAVE PRECISION                               
         NI    DMCB+4,X'0F'                                                     
         ZIC   R0,DMCB+4           LENGTH OF EACH FLD IN ELEMT                  
         LA    R1,4                                                             
         SR    R1,R0               #BYTES TO DISP INTO UNIVSAVE                 
         L     R5,ASLOT            SLOT INTO BEGIN SLOT SPECIFIED               
         AR    R5,R1                                                            
         LA    R6,3(R6)            PT TO 1ST DEMO IN ELEMENT                    
*                                                                               
RESL20   ZIC   R1,DMCB+4           PRECISION                                    
         C     R6,DMCB             END OF THIS ELELMENT                         
         BL    RESL25                                                           
         SR    R1,R1               SET CC=ZERO ON EXIT                          
         B     RELSLOTX                                                         
*                                                                               
RESL25   BCTR  R1,0                MOVE DEMO INTO SLOT                          
         EXMVC R1,0(R5),0(R6)                                                   
         LA    R5,4(R5)                                                         
         AR    R6,R1               BUMP ELEMENT PTR                             
         LA    R6,1(R6)            +1 FROM BCTR                                 
         B     RESL20                                                           
*                                                                               
RESLEOF  LA    R1,1                SET CC TO NON ZERO                           
*                                                                               
RELSLOTX LTR   R1,R1                                                            
         B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
**********************************************************************          
* LTORG, TABLES, AND CONSTANTS                                                  
**********************************************************************          
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
* THIS OVERRIDE "SORT FIELDS" CARD FOR DFSORT CONTAINS A VERY LARGE             
* RECORD COUNT ESTIMATE, IN AN ATTEMPT TO HELP DFSORT MAKE GOOD CHOICES         
* REGARDING ITS ALGORITHM. IN PARTICULAR, WE'RE TRYING TO PREVENT               
* DFSORT FROM GOING INTO ITS "INTERMEDIATE MERGE" PATH, WHICH IS KNOWN          
* TO BE INEFFICIENT.                                                            
*                                                                               
SORTCARD DC    CL80' '                                                          
         ORG   SORTCARD                                                         
         DC    C'SORT FIELDS=(5,'                                               
SORTCRDL DS    CL3                                                              
         DC    C',A),FORMAT=BI,'                                                
         DC    C'FILSZ=E20000000'  ESTIMATE 20 MILLION SORT RECORDS             
         ORG                                                                    
*                                                                               
IN1      DCB   DDNAME=IN1,                                             X        
               DSORG=PS,                                               X        
               RECFM=VB,                                               X        
               LRECL=MAXRRECL,                                         X        
               BLKSIZE=0,                                              X        
               MACRF=GM,                                               X        
               EODAD=DONE                                                       
*                                                                               
MAXRRECL EQU   1996                MAX LENGTH OF INPUT RECORD(2000-4)           
YES      EQU   0                                                                
NO       EQU   1                                                                
*                                                                               
* TABLE OF VIEWING TYPES                                                        
VTYPTAB  DC    AL1(MIRVLIVE),AL1(SRCLIVE)                                       
         DC    AL1(MIRVLSD),AL1(SRCLIVSD)                                       
         DC    AL1(MIRVL1),AL1(SRCLIVE1)                                        
         DC    AL1(MIRVL2),AL1(SRCLIVE2)                                        
         DC    AL1(MIRVL3),AL1(SRCLIVE3)                                        
         DC    AL1(MIRVL7),AL1(SRCLIVE7)                                        
         DC    X'FF'                                                            
*                                                                               
* BROADCAST NETS THAT GET STORED WITH DIFFERENT CALL LETTERS THAN HOW           
* THEY COME ON THE RLD FILES.                                                   
BRDDNETT DC    CL4'UPN ',CL4'PAR '                                              
         DC    CL4'ION ',CL4'PAX '                                              
         DC    CL4'UMA ',CL4'TF  '                                              
         DC    CL4'MMX ',CL4'MFX '                                              
         DC    X'FF'                                                            
*                                                                               
* INTERNAL REPRESENTATION OF DAYS                                               
DAYTAB   DS    0XL2                                                             
         DC    C'1',X'01'          MONDAY                                       
         DC    C'2',X'02'          TUESDAY                                      
         DC    C'3',X'03'          WEDNESDAY                                    
         DC    C'4',X'04'          THURSDAY                                     
         DC    C'5',X'05'          FRIDAY                                       
         DC    C'6',X'06'          SATURDAY                                     
         DC    C'7',X'07'          SUNDAY                                       
         DC    X'FF'                                                            
*                                                                               
* DAYPART TABLE                                                                 
DAYPTAB  DC    C'PRI     ',C'PR'   PRIME TIME                                   
         DC    C'EF      ',C'EF'   EARLY FRINGE                                 
         DC    C'LF      ',C'LF'   LATE FRINGE                                  
         DC    C'WKM     ',C'WM'   WEEKDAY MORNING                              
         DC    C'WKD     ',C'WD'   WEEKDAY DAYTIME                              
         DC    C'WED     ',C'ED'   WEEKEND DAYTIME                              
         DC    X'FF'                                                            
*                                                                               
* TABLE OF MEDIAS FOR DELETION RECORDS                                          
DMEDTAB  DC    AL1(DELRMEDB),AL1(IMEDBRO)     BROADCAST                         
         DC    AL1(DELRMEDC),AL1(IMEDCAB)     CABLE                             
         DC    AL1(DELRMEDA),AL1(0)           ALL                               
         DC    X'FF'                                                            
*                                                                               
* TABLE OF MINUTE TYPES                                                         
MINTYPT  DC    AL1(PTRMTYPC),AL1(MPTMTYPC)    COMMERCIAL                        
         DC    AL1(PTRMTYPN),AL1(MPTMTYPP)    NON-COMMERCIAL                    
         DC    AL1(PTRMTYPU),AL1(MPTMTYPU)    UNKNOWN                           
         DC    X'FF'                                                            
*                                                                               
* INTAB COUNTS TABLE                                                            
         DS    0F                                                               
INTAB    DS    0XL156                                                           
         DS    XL3,XL1,38F         MON                                          
         DS    XL3,XL1,38F         TUE                                          
         DS    XL3,XL1,38F         WED                                          
         DS    XL3,XL1,38F         THU                                          
         DS    XL3,XL1,38F         FRI                                          
         DS    XL3,XL1,38F         SAT                                          
         DS    XL3,XL1,38F         SUN                                          
INTABS   EQU   (*-INTAB)/L'INTAB                                                
         DC    XL3'00'                                                          
*--------------------------------------------------------------------*          
* DISPLACEMENTS TO DEMOS ON THE INTERIM RECORD DEMO AREA (INTACCS)              
* FOR ***BROADCAST***. THIS CORRESPONDS TO THE DEMDISP TABLE FOR 'PNN'.         
*--------------------------------------------------------------------*          
*                                                                               
* RAW IMPRESSIONS 'Y'                                                           
IBYW2_5  EQU   0                                                                
IBYW6_8  EQU   1                                                                
IBYW9_11 EQU   2                                                                
IBYW1214 EQU   3                                                                
IBYW1517 EQU   4                                                                
IBYW1820 EQU   5                                                                
IBYW2124 EQU   6                                                                
IBYW2529 EQU   7                                                                
IBYW3034 EQU   8                                                                
IBYW3539 EQU   9                                                                
IBYW4044 EQU   10                                                               
IBYW4549 EQU   11                                                               
IBYW5054 EQU   12                                                               
IBYW5564 EQU   13                                                               
IBYW65_P EQU   14                                                               
IBYM2_5  EQU   15                                                               
IBYM6_8  EQU   16                                                               
IBYM9_11 EQU   17                                                               
IBYM1214 EQU   18                                                               
IBYM1517 EQU   19                                                               
IBYM1820 EQU   20                                                               
IBYM2124 EQU   21                                                               
IBYM2529 EQU   22                                                               
IBYM3034 EQU   23                                                               
IBYM3539 EQU   24                                                               
IBYM4044 EQU   25                                                               
IBYM4549 EQU   26                                                               
IBYM5054 EQU   27                                                               
IBYM5564 EQU   28                                                               
IBYM65_P EQU   29                                                               
*                                                                               
* HOMES RATING 'R'                                                              
IBRHOMES EQU   85                                                               
*                                                                               
* HOMES RAW IMPRESSION 'Y'                                                      
IBYHOMES EQU   86                                                               
*                                                                               
* HOMES SHARE 'S'                                                               
IBSHOMES EQU   87                                                               
*                                                                               
* RAW PROGRAM PUTS 'Z'                                                          
IBZW2_5  EQU   214                                                              
IBZW6_8  EQU   215                                                              
IBZW9_11 EQU   216                                                              
IBZW1214 EQU   217                                                              
IBZW1517 EQU   218                                                              
IBZW1820 EQU   219                                                              
IBZW2124 EQU   220                                                              
IBZW2529 EQU   221                                                              
IBZW3034 EQU   222                                                              
IBZW3539 EQU   223                                                              
IBZW4044 EQU   224                                                              
IBZW4549 EQU   225                                                              
IBZW5054 EQU   226                                                              
IBZW5564 EQU   227                                                              
IBZW65_P EQU   228                                                              
IBZM2_5  EQU   229                                                              
IBZM6_8  EQU   230                                                              
IBZM9_11 EQU   231                                                              
IBZM1214 EQU   232                                                              
IBZM1517 EQU   233                                                              
IBZM1820 EQU   234                                                              
IBZM2124 EQU   235                                                              
IBZM2529 EQU   236                                                              
IBZM3034 EQU   237                                                              
IBZM3539 EQU   238                                                              
IBZM4044 EQU   239                                                              
IBZM4549 EQU   240                                                              
IBZM5054 EQU   241                                                              
IBZM5564 EQU   242                                                              
IBZM65_P EQU   243                                                              
*                                                                               
* GAA RAW IMPRESSIONS 'B'                                                       
IBBW2_5  EQU   290                                                              
IBBW6_8  EQU   291                                                              
IBBW9_11 EQU   292                                                              
IBBW1214 EQU   293                                                              
IBBW1517 EQU   294                                                              
IBBW1820 EQU   295                                                              
IBBW2124 EQU   296                                                              
IBBW2529 EQU   297                                                              
IBBW3034 EQU   298                                                              
IBBW3539 EQU   299                                                              
IBBW4044 EQU   300                                                              
IBBW4549 EQU   301                                                              
IBBW5054 EQU   302                                                              
IBBW5564 EQU   303                                                              
IBBW65_P EQU   304                                                              
IBBM2_5  EQU   305                                                              
IBBM6_8  EQU   306                                                              
IBBM9_11 EQU   307                                                              
IBBM1214 EQU   308                                                              
IBBM1517 EQU   309                                                              
IBBM1820 EQU   310                                                              
IBBM2124 EQU   311                                                              
IBBM2529 EQU   312                                                              
IBBM3034 EQU   313                                                              
IBBM3539 EQU   314                                                              
IBBM4044 EQU   315                                                              
IBBM4549 EQU   316                                                              
IBBM5054 EQU   317                                                              
IBBM5564 EQU   318                                                              
IBBM65_P EQU   319                                                              
*                                                                               
* HOMES GAA RATING 'L'                                                          
IBLHOMES EQU   374                                                              
*                                                                               
* HOMES GAA RAW IMPRESSION 'B'                                                  
IBBHOMES EQU   375                                                              
*                                                                               
* RAW PROGRAM HUTS 'Z'                                                          
IBZHOMES EQU   408                                                              
*                                                                               
*            ______                                                             
*              409  ---------> MAXDIDEM                                         
*            CHANGE MAXDIDEM IF THIS VALUE CHANGES                              
*                                                                               
*--------------------------------------------------------------------*          
* TABLE TO TRANSLATE INPUT DEMOS TO INTERIM SLOTS (INTACCS)                     
* FOR ***BROADCAST***                                                           
*--------------------------------------------------------------------*          
* BROADCAST IMPRESSIONS 'Y'                                                     
BYSLOT   DC    AL1(MDRHOMES),AL2(IBYHOMES)                                      
         DC    AL1(MDRM2_5),AL2(IBYM2_5)                                        
         DC    AL1(MDRM6_8),AL2(IBYM6_8)                                        
         DC    AL1(MDRM9_11),AL2(IBYM9_11)                                      
         DC    AL1(MDRM1214),AL2(IBYM1214)                                      
         DC    AL1(MDRM1517),AL2(IBYM1517)                                      
         DC    AL1(MDRM1820),AL2(IBYM1820)                                      
         DC    AL1(MDRM2124),AL2(IBYM2124)                                      
         DC    AL1(MDRM2529),AL2(IBYM2529)                                      
         DC    AL1(MDRM3034),AL2(IBYM3034)                                      
         DC    AL1(MDRM3539),AL2(IBYM3539)                                      
         DC    AL1(MDRM4044),AL2(IBYM4044)                                      
         DC    AL1(MDRM4549),AL2(IBYM4549)                                      
         DC    AL1(MDRM5054),AL2(IBYM5054)                                      
         DC    AL1(MDRM5564),AL2(IBYM5564)                                      
         DC    AL1(MDRM65_P),AL2(IBYM65_P)                                      
         DC    AL1(MDRF2_5),AL2(IBYW2_5)                                        
         DC    AL1(MDRF6_8),AL2(IBYW6_8)                                        
         DC    AL1(MDRF9_11),AL2(IBYW9_11)                                      
         DC    AL1(MDRF1214),AL2(IBYW1214)                                      
         DC    AL1(MDRF1517),AL2(IBYW1517)                                      
         DC    AL1(MDRF1820),AL2(IBYW1820)                                      
         DC    AL1(MDRF2124),AL2(IBYW2124)                                      
         DC    AL1(MDRF2529),AL2(IBYW2529)                                      
         DC    AL1(MDRF3034),AL2(IBYW3034)                                      
         DC    AL1(MDRF3539),AL2(IBYW3539)                                      
         DC    AL1(MDRF4044),AL2(IBYW4044)                                      
         DC    AL1(MDRF4549),AL2(IBYW4549)                                      
         DC    AL1(MDRF5054),AL2(IBYW5054)                                      
         DC    AL1(MDRF5564),AL2(IBYW5564)                                      
         DC    AL1(MDRF65_P),AL2(IBYW65_P)                                      
         DC    AL1(MDRRHOMS),AL2(IBRHOMES)   HOMES RATING                       
         DC    X'FFFF'                                                          
* BROADCAST PROGRAM PUTS 'Z'                                                    
BZSLOT   DC    AL1(MDRM2_5),AL2(IBZM2_5)                                        
         DC    AL1(MDRM6_8),AL2(IBZM6_8)                                        
         DC    AL1(MDRM9_11),AL2(IBZM9_11)                                      
         DC    AL1(MDRM1214),AL2(IBZM1214)                                      
         DC    AL1(MDRM1517),AL2(IBZM1517)                                      
         DC    AL1(MDRM1820),AL2(IBZM1820)                                      
         DC    AL1(MDRM2124),AL2(IBZM2124)                                      
         DC    AL1(MDRM2529),AL2(IBZM2529)                                      
         DC    AL1(MDRM3034),AL2(IBZM3034)                                      
         DC    AL1(MDRM3539),AL2(IBZM3539)                                      
         DC    AL1(MDRM4044),AL2(IBZM4044)                                      
         DC    AL1(MDRM4549),AL2(IBZM4549)                                      
         DC    AL1(MDRM5054),AL2(IBZM5054)                                      
         DC    AL1(MDRM5564),AL2(IBZM5564)                                      
         DC    AL1(MDRM65_P),AL2(IBZM65_P)                                      
         DC    AL1(MDRF2_5),AL2(IBZW2_5)                                        
         DC    AL1(MDRF6_8),AL2(IBZW6_8)                                        
         DC    AL1(MDRF9_11),AL2(IBZW9_11)                                      
         DC    AL1(MDRF1214),AL2(IBZW1214)                                      
         DC    AL1(MDRF1517),AL2(IBZW1517)                                      
         DC    AL1(MDRF1820),AL2(IBZW1820)                                      
         DC    AL1(MDRF2124),AL2(IBZW2124)                                      
         DC    AL1(MDRF2529),AL2(IBZW2529)                                      
         DC    AL1(MDRF3034),AL2(IBZW3034)                                      
         DC    AL1(MDRF3539),AL2(IBZW3539)                                      
         DC    AL1(MDRF4044),AL2(IBZW4044)                                      
         DC    AL1(MDRF4549),AL2(IBZW4549)                                      
         DC    AL1(MDRF5054),AL2(IBZW5054)                                      
         DC    AL1(MDRF5564),AL2(IBZW5564)                                      
         DC    AL1(MDRF65_P),AL2(IBZW65_P)                                      
         DC    AL1(MDRHOMES),AL2(IBZHOMES)                                      
         DC    X'FFFF'                                                          
* BROADCAST GAA IMPRESSIONS 'B'                                                 
BBSLOT   DC    AL1(MDRHOMES),AL2(IBBHOMES)                                      
         DC    AL1(MDRM2_5),AL2(IBBM2_5)                                        
         DC    AL1(MDRM6_8),AL2(IBBM6_8)                                        
         DC    AL1(MDRM9_11),AL2(IBBM9_11)                                      
         DC    AL1(MDRM1214),AL2(IBBM1214)                                      
         DC    AL1(MDRM1517),AL2(IBBM1517)                                      
         DC    AL1(MDRM1820),AL2(IBBM1820)                                      
         DC    AL1(MDRM2124),AL2(IBBM2124)                                      
         DC    AL1(MDRM2529),AL2(IBBM2529)                                      
         DC    AL1(MDRM3034),AL2(IBBM3034)                                      
         DC    AL1(MDRM3539),AL2(IBBM3539)                                      
         DC    AL1(MDRM4044),AL2(IBBM4044)                                      
         DC    AL1(MDRM4549),AL2(IBBM4549)                                      
         DC    AL1(MDRM5054),AL2(IBBM5054)                                      
         DC    AL1(MDRM5564),AL2(IBBM5564)                                      
         DC    AL1(MDRM65_P),AL2(IBBM65_P)                                      
         DC    AL1(MDRF2_5),AL2(IBBW2_5)                                        
         DC    AL1(MDRF6_8),AL2(IBBW6_8)                                        
         DC    AL1(MDRF9_11),AL2(IBBW9_11)                                      
         DC    AL1(MDRF1214),AL2(IBBW1214)                                      
         DC    AL1(MDRF1517),AL2(IBBW1517)                                      
         DC    AL1(MDRF1820),AL2(IBBW1820)                                      
         DC    AL1(MDRF2124),AL2(IBBW2124)                                      
         DC    AL1(MDRF2529),AL2(IBBW2529)                                      
         DC    AL1(MDRF3034),AL2(IBBW3034)                                      
         DC    AL1(MDRF3539),AL2(IBBW3539)                                      
         DC    AL1(MDRF4044),AL2(IBBW4044)                                      
         DC    AL1(MDRF4549),AL2(IBBW4549)                                      
         DC    AL1(MDRF5054),AL2(IBBW5054)                                      
         DC    AL1(MDRF5564),AL2(IBBW5564)                                      
         DC    AL1(MDRF65_P),AL2(IBBW65_P)                                      
         DC    AL1(MDRRHOMS),AL2(IBLHOMES) HOMES GAA RATING                     
         DC    X'FFFF'                                                          
         EJECT                                                                  
*--------------------------------------------------------------------*          
* DISPLACEMENTS TO DEMOS ON THE INTERIM RECORD DEMO AREA (INTACCS)              
* FOR ***CABLE***. THIS CORRESPONDS TO THE DEMDISP TABLE FOR 'CNN'.             
*--------------------------------------------------------------------*          
*                                                                               
* RAW IMPRESSIONS 'Y'                                                           
ICYW1214 EQU   0                                                                
ICYW1517 EQU   1                                                                
ICYW1820 EQU   2                                                                
ICYW2124 EQU   3                                                                
ICYW2529 EQU   4                                                                
ICYW3034 EQU   5                                                                
ICYW3539 EQU   6                                                                
ICYW4044 EQU   7                                                                
ICYW4549 EQU   8                                                                
ICYW5054 EQU   9                                                                
ICYW5564 EQU   10                                                               
ICYW65_P EQU   11                                                               
ICYM1214 EQU   12                                                               
ICYM1517 EQU   13                                                               
ICYM1820 EQU   14                                                               
ICYM2124 EQU   15                                                               
ICYM2529 EQU   16                                                               
ICYM3034 EQU   17                                                               
ICYM3539 EQU   18                                                               
ICYM4044 EQU   19                                                               
ICYM4549 EQU   20                                                               
ICYM5054 EQU   21                                                               
ICYM5564 EQU   22                                                               
ICYM65_P EQU   23                                                               
ICYW9_11 EQU   24                                                               
ICYM9_11 EQU   25                                                               
ICYW6_8  EQU   26                                                               
ICYM6_8  EQU   27                                                               
ICYW2_5  EQU   28                                                               
ICYM2_5  EQU   29                                                               
ICYH_USA EQU   30                                                               
ICYHOMES EQU   31                                                               
*                                                                               
* PROGRAM PUTS 'Z'                                                              
ICZW1214 EQU   41                                                               
ICZW1517 EQU   42                                                               
ICZW1820 EQU   43                                                               
ICZW2124 EQU   44                                                               
ICZW2529 EQU   45                                                               
ICZW3034 EQU   46                                                               
ICZW3539 EQU   47                                                               
ICZW4044 EQU   48                                                               
ICZW4549 EQU   49                                                               
ICZW5054 EQU   50                                                               
ICZW5564 EQU   51                                                               
ICZW65_P EQU   52                                                               
ICZM1214 EQU   53                                                               
ICZM1517 EQU   54                                                               
ICZM1820 EQU   55                                                               
ICZM2124 EQU   56                                                               
ICZM2529 EQU   57                                                               
ICZM3034 EQU   58                                                               
ICZM3539 EQU   59                                                               
ICZM4044 EQU   60                                                               
ICZM4549 EQU   61                                                               
ICZM5054 EQU   62                                                               
ICZM5564 EQU   63                                                               
ICZM65_P EQU   64                                                               
ICZW9_11 EQU   65                                                               
ICZM9_11 EQU   66                                                               
ICZW6_8  EQU   67                                                               
ICZM6_8  EQU   68                                                               
ICZW2_5  EQU   69                                                               
ICZM2_5  EQU   70                                                               
ICZH_USA EQU   71                                                               
ICZHOMES EQU   72                                                               
*--------------------------------------------------------------------*          
* TABLE TO TRANSLATE INPUT DEMOS TO INTERIM SLOTS (INTACCS)                     
* FOR ***CABLE***                                                               
*--------------------------------------------------------------------*          
* CABLE IMPRESSIONS 'Y'                                                         
CYSLOT   DC    AL1(MDRF1214),AL2(ICYW1214)                                      
         DC    AL1(MDRF1517),AL2(ICYW1517)                                      
         DC    AL1(MDRF1820),AL2(ICYW1820)                                      
         DC    AL1(MDRF2124),AL2(ICYW2124)                                      
         DC    AL1(MDRF2529),AL2(ICYW2529)                                      
         DC    AL1(MDRF3034),AL2(ICYW3034)                                      
         DC    AL1(MDRF3539),AL2(ICYW3539)                                      
         DC    AL1(MDRF4044),AL2(ICYW4044)                                      
         DC    AL1(MDRF4549),AL2(ICYW4549)                                      
         DC    AL1(MDRF5054),AL2(ICYW5054)                                      
         DC    AL1(MDRF5564),AL2(ICYW5564)                                      
         DC    AL1(MDRF65_P),AL2(ICYW65_P)                                      
         DC    AL1(MDRM1214),AL2(ICYM1214)                                      
         DC    AL1(MDRM1517),AL2(ICYM1517)                                      
         DC    AL1(MDRM1820),AL2(ICYM1820)                                      
         DC    AL1(MDRM2124),AL2(ICYM2124)                                      
         DC    AL1(MDRM2529),AL2(ICYM2529)                                      
         DC    AL1(MDRM3034),AL2(ICYM3034)                                      
         DC    AL1(MDRM3539),AL2(ICYM3539)                                      
         DC    AL1(MDRM4044),AL2(ICYM4044)                                      
         DC    AL1(MDRM4549),AL2(ICYM4549)                                      
         DC    AL1(MDRM5054),AL2(ICYM5054)                                      
         DC    AL1(MDRM5564),AL2(ICYM5564)                                      
         DC    AL1(MDRM65_P),AL2(ICYM65_P)                                      
         DC    AL1(MDRF9_11),AL2(ICYW9_11)                                      
         DC    AL1(MDRM9_11),AL2(ICYM9_11)                                      
         DC    AL1(MDRF6_8),AL2(ICYW6_8)                                        
         DC    AL1(MDRM6_8),AL2(ICYM6_8)                                        
         DC    AL1(MDRF2_5),AL2(ICYW2_5)                                        
         DC    AL1(MDRM2_5),AL2(ICYM2_5)                                        
         DC    AL1(MDRHOMES),AL2(ICYH_USA)                                      
         DC    AL1(MDRHOMES),AL2(ICYHOMES)                                      
         DC    X'FFFF'                                                          
*                                                                               
* CABLE PROGRAM PUTS 'Z'                                                        
CZSLOT   DC    AL1(MDRF1214),AL2(ICZW1214)                                      
         DC    AL1(MDRF1517),AL2(ICZW1517)                                      
         DC    AL1(MDRF1820),AL2(ICZW1820)                                      
         DC    AL1(MDRF2124),AL2(ICZW2124)                                      
         DC    AL1(MDRF2529),AL2(ICZW2529)                                      
         DC    AL1(MDRF3034),AL2(ICZW3034)                                      
         DC    AL1(MDRF3539),AL2(ICZW3539)                                      
         DC    AL1(MDRF4044),AL2(ICZW4044)                                      
         DC    AL1(MDRF4549),AL2(ICZW4549)                                      
         DC    AL1(MDRF5054),AL2(ICZW5054)                                      
         DC    AL1(MDRF5564),AL2(ICZW5564)                                      
         DC    AL1(MDRF65_P),AL2(ICZW65_P)                                      
         DC    AL1(MDRM1214),AL2(ICZM1214)                                      
         DC    AL1(MDRM1517),AL2(ICZM1517)                                      
         DC    AL1(MDRM1820),AL2(ICZM1820)                                      
         DC    AL1(MDRM2124),AL2(ICZM2124)                                      
         DC    AL1(MDRM2529),AL2(ICZM2529)                                      
         DC    AL1(MDRM3034),AL2(ICZM3034)                                      
         DC    AL1(MDRM3539),AL2(ICZM3539)                                      
         DC    AL1(MDRM4044),AL2(ICZM4044)                                      
         DC    AL1(MDRM4549),AL2(ICZM4549)                                      
         DC    AL1(MDRM5054),AL2(ICZM5054)                                      
         DC    AL1(MDRM5564),AL2(ICZM5564)                                      
         DC    AL1(MDRM65_P),AL2(ICZM65_P)                                      
         DC    AL1(MDRF9_11),AL2(ICZW9_11)                                      
         DC    AL1(MDRM9_11),AL2(ICZM9_11)                                      
         DC    AL1(MDRF6_8),AL2(ICZW6_8)                                        
         DC    AL1(MDRM6_8),AL2(ICZM6_8)                                        
         DC    AL1(MDRF2_5),AL2(ICZW2_5)                                        
         DC    AL1(MDRM2_5),AL2(ICZM2_5)                                        
         DC    AL1(MDRHOMES),AL2(ICZH_USA)                                      
         DC    AL1(MDRHOMES),AL2(ICZHOMES)                                      
         DC    X'FFFF'                                                          
         EJECT                                                                  
***********************************************************************         
* STORAGE                                                                       
***********************************************************************         
PACK16   DS    PL16                                                             
*                                                                               
PRVRTYP  DC    C' '                PREVIOUS RECORD TYPE                         
PRVRSTYP DC    C' '                PREVIOUS RECORD SUBTYPE                      
PRVSTA   DC    CL5' '              PREVIOUS STATION                             
*                                                                               
PRVMKEY  DC    CL(L'INTKEY)' '     PREVIOUS 'M' KEY                             
*                                                                               
PRVMXMBK DC    X'0000'             PREVIOUS MXM BOOK                            
*                                                                               
INTBKEY  DC    XL24'00'            INTAB KEY                                    
SVKEY    DC    XL24'00'            SAVE KEY                                     
SVDA     DS    F                                                                
SVSTATUS DS    C                                                                
*                                                                               
MYMODE   DC    AL1(0)              CONVERSION SPECIFIC MODE                     
RELMRET  EQU   1                   RELEASE 'M' RECORD AND RETURN                
RELMEND  EQU   2                   RELEASE 'M' RECORD AND END                   
*                                                                               
DATADISP DS    H                   FOR GETEL                                    
ELCODE   DS    X                                                                
*                                                                               
ASLOT    DS    A                                                                
*                                                                               
INTABBK  DS    XL2                 INTAB BOOK                                   
*                                                                               
         DS    0F                                                               
CALVPH   DS    CL(VPHINT-VPHDEMS+4) CALCULATE VPH'S                             
*                                                                               
         DS    0F                                                               
DEMAREA  DS    (NDEMS*3)XL4        DEMOS: 3 MODIFIERS (AA, GAA, PUTS)           
DEMLNQ   EQU   *-DEMAREA                                                        
         EJECT                                                                  
***********************************************************************         
* DSECTS                                                                        
***********************************************************************         
VTYPTABD DSECT                     VIEWING TYPES                                
VTINREC  DS    C                   IDENTIFIER ON THE INPUT RECORD               
VTDEMREC DS    C                   IDENTIFIER ON THE DEMO RECORD                
VTYPTABL EQU   *-VTYPTABD                                                       
*                                                                               
BRDDNETD DSECT                     SPECIAL BROADCAST CALL LETTERS               
BRDNIELS DS    CL4                 NIELSEN CALL LETTERS                         
BRDDDS   DS    CL4                 DDS CALL LETTERS                             
BRDDNETL EQU   *-BRDDNETD                                                       
*                                                                               
DAYTABD  DSECT                     WEEK DAYS                                    
DYINREC  DS    C                   DAY ON THE INPUT RECORD                      
DYDEMREC DS    X                   DAY ON THE DEMO RECORD                       
DAYTABL  EQU   *-DAYTABD                                                        
*                                                                               
DAYPTABD DSECT                     DAYPART CODES                                
DYPINREC DS    CL8                 DAYPART ON THE INPUT RECORD                  
DYPDMREC DS    CL2                 DAYPART ON THE DEMO RECORD                   
DAYPTABL EQU   *-DAYPTABD                                                       
*                                                                               
DMEDTABD DSECT                     MEDIA FOR DELETION RECORDS                   
DMDINREC DS    CL1                 MEDIA ON THE INPUT RECORD                    
DMDITREC DS    CL1                 MEDIA ON THE INTERIM RECORD                  
DMEDTABL EQU   *-DMEDTABD                                                       
*                                                                               
MINTYPTD DSECT                     MINUTE TYPES                                 
MTYINREC DS    CL1                 MINUTE TYPE ON THE INPUT RECORD              
MTYDMREC DS    CL1                 MINUTE TYPE ON THE DEMO RECORD               
MINTYPTL EQU   *-MINTYPTD                                                       
*                                                                               
SLOTTABD DSECT                                                                  
SLINDSP  DS    AL1                 DISPLACEMENT ON THE INPUT RECORD             
*                                  STARTS AT 1                                  
SLOUTDSP DS    AL2                 DISPLACEMENT ON THE OUTPUT RECORD            
*                                  STARTS AT 0                                  
SLOTTABL EQU   *-SLOTTABD                                                       
*                                                                               
         EJECT                                                                  
* INTERIM RECORD DSECT                                                          
       ++INCLUDE DEINTD                                                         
         SPACE 2                                                                
       ++INCLUDE DEINTMXMD                                                      
         SPACE 2                                                                
         DS    (MAXDIDEM)XL4                                                    
MAXIRECL EQU   *-INTERD            MAX LENGTH OF INTERIM RECORD                 
         EJECT                                                                  
       ++INCLUDE DEMXMIRD                                                       
         EJECT                                                                  
* OTHER DSECTS                                                                  
*  ++INCLUDE DDDPRINT                                                           
*  ++INCLUDE DEDEMFILE                                                          
*  ++INCLUDE DEDEMCNVD                                                          
*  ++INCLUDE DDCOMFACS                                                          
*  ++INCLUDE DEDEMTABD                                                          
*  ++INCLUDE DECALVPHD                                                          
         PRINT OFF                                                              
       ++INCLUDE DDDPRINT                                                       
       ++INCLUDE DEDEMFILE                                                      
       ++INCLUDE DEDEMCNVD                                                      
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DEDEMTABD                                                      
       ++INCLUDE DECALVPHD                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'011DEMXMI    01/20/21'                                      
         END                                                                    
