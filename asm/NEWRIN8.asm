*          DATA SET NEWRIN8    AT LEVEL 012 AS OF 05/01/02                      
*PHASE T32029A                                                                  
         TITLE 'T32029 - N8  REPORT  PHASE'                                     
T32029   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**NEN8**,RR=R2                                                 
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING T320FFD,RA                                                       
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASYSD                                                         
         USING NETSYSD,R9                                                       
         L     R7,ANETWS4         ANETWS4 = WORKING STORAGE                     
         USING MYD,R7                                                           
         ST    R2,RELO                                                          
         L     R1,=A(RECTABLE)                                                  
         A     R1,RELO                                                          
         ST    R1,ATABLE                                                        
         SPACE 1                                                                
         CLI   MODE,PRINTREP                                                    
         BNE   RP2                                                              
         BAS   RE,REPMOD                                                        
         B     XIT                                                              
*                                                                               
RP2      CLI   MODE,VALREC                                                      
         BNE   RP4                                                              
         BAS   RE,EDITMOD                                                       
         B     XIT                                                              
RP4      EQU   *                                                                
XIT      XIT1                                                                   
         EJECT                                                                  
*              EDIT ROUTINES                                                    
         SPACE 3                                                                
EDITMOD  NTR1                                                                   
         MVI   NBQINIT,0           DO ALL VALIDATIONS EACH TIME                 
         MVI   NBDATA,C'U'         UNIT RECORDS ONLY                            
*                                                                               
         MVI   FTERMFLG,0          FOLLOWING FIELDS ARE REQUIRED                
*                                                                               
         LA    R2,SPLCLIH                CLIENT                                 
         NETGO NVCLIALL,DMCB,SPLCLIN                                            
         OI    SPLCLINH+6,X'80'                                                 
*                                                                               
         MVI   FTERMFLG,1          FOLLOWING FIELDS ARE OPTIONAL                
*                                                                               
         LA    R2,SPLPROH                PRODUCT                                
         NETGO NVPRDALL,DMCB,SPLPRON                                            
         OI    SPLPRONH+6,X'80'                                                 
*                                                                               
         LA    R2,SPLESTH                ESTIMATE                               
         NETGO NVESTRNG,DMCB,SPLESTN                                            
         OI    SPLESTNH+6,X'80'                                                 
*                                                                               
         LA    R2,SPLNETH                NETWORK                                
         NETGO NVNETALL,DMCB                                                    
*                                                                               
         LA    R2,SPLDPTH                DAYPART                                
         NETGO NVDPT,DMCB                                                       
         OI    SPLDPTNH+6,X'80'                                                 
*                                                                               
         LA    R2,SPLPAKH                PACKAGE                                
         NETGO NVPAKLOK,DMCB                                                    
         OI    SPLPAKNH+6,X'80'                                                 
*                                                                               
         LA    R2,SPLSTRTH               START DATE                             
         NETGO NVSTRDAT,DMCB                                                    
*                                                                               
         LA    R2,SPLENDDH               END DATE                               
         NETGO NVENDDAT,DMCB                                                    
         EJECT                                                                  
*                                  DIG OUT ANY OPTIONS                          
EDT14    LA    R2,SPLOPTH          OPTIONS                                      
         NETGO NVGETFLD,DMCB                                                    
         BZ    EDT20                                                            
         GOTO1 SCANNER,DMCB,(R2),(10,BLOCK)                                     
         LA    R3,BLOCK                                                         
         ZIC   R0,DMCB+4                                                        
         LTR   R0,R0                                                            
         BZ    EDINV                                                            
OPT5     CLC   12(4,R3),=C'SKIP'  SEPERATE PAGE PER PRODUCT                     
         BNE   EDT18                                                            
** DO OPTION CHECKING HERE                                                      
         B     EDT18                                                            
OPTINV   B     EDINV                                                            
*                                                                               
EDT16    DS    0H                                                               
*                                                                               
         SPACE 1                                                                
EDT18    LA    R3,32(R3)                                                        
         BCT   R0,EDT16                                                         
         SPACE 1                                                                
EDT20    LA    R2,SPLTITLH                                                      
         MVC   NDTITLE,SPACES                                                   
         NETGO NVGETFLD,DMCB                                                    
         BZ    EDT22                                                            
         MVC   NDTITLE,FLD                                                      
         SPACE 1                                                                
EDT22    LA    R2,SPLCLIH                                                       
         XIT1  REGS=(R2)                                                        
*                                                                               
*                                                                               
EDINV    DS    0H                                                               
         MVI   ERROR,INVALID                                                    
*                                                                               
EDERR    GOTO1 ERREX                                                            
         EJECT                                                                  
*                                                                               
REPMOD   NTR1                                                                   
         XC    LINENUM,LINENUM                                                  
         XC    DMCB,DMCB                                                        
*                                                                               
         GOTO1 SORTER,DMCB,SORTCARD,RECCARD                                     
*                                                                               
REP01    NETGO NSNETIO,DMCB,NETBLOCK    TO SET NBCOMPST,NBCMPEND                
         CLI   NBMODE,NBVALDAT          (DON'T NEED DATE LIST)                  
         BNE   REP01                                                            
         MVC   NUMMONS,=F'2'                                                    
         MVI   PERTYPE,C'W'                                                     
         NETGO NVWKLST,DMCB,NUMMONS,MONLIST,PERTYPE                             
         B     REP5                                                             
*                                                                               
SORTCARD DC    CL80'SORT FIELDS=(1,4,A),FORMAT=BI,WORK=1'                       
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=58'                                    
*                                                                               
         EJECT                                                                  
REP5     MVI   NBDATA,C'U'         UNITS ONLY                                   
         MVI   NBSEQ,C'N'          NETWORK ORDER                                
GETUNIT  NETGO NSNETIO,DMCB,NETBLOCK                                            
         TM    NBSUBMSK,NBSBMNET      BREAK ON NETWORK                          
         BZ    *+8                                                              
         BAS   RE,REP30                                                         
*                                                                               
         CLI   NBMODE,NBPROCUN                                                  
         BE    REP10                                                            
*                                                                               
         CLI   NBMODE,NBREQLST                                                  
         BNE   *+12                                                             
         BAS   RE,REP30                                                         
         B     XIT                                                              
*                                                                               
         CLI   NBERROR,0                                                        
         BE    GETUNIT                                                          
         DC    H'0'                                                             
*                                                                               
REP10    DS    0H                                                               
*                                 R5 - M/G ELEMENT                              
*                                 R6 - RECTABLE                                 
*                                                                               
         L     R5,NBAIO                                                         
         MVI   ELCODE,7            IS IT MADE GOOD                              
         USING NUMGD,R5                                                         
         BAS   RE,GETEL                                                         
         BNE   GETUNIT             IF NOT/ SKIP THE UNIT                        
         L     R6,ATABLE                                                        
         USING RECTABLD,R6                                                      
REP11    CLC   0(6,R6),=6X'0'                                                   
         BE    REP12                                                            
         LA    R6,RECLENE(R6)                                                   
         B     REP11                                                            
REP12    DS    0H                                                               
         LA    R0,6                SET MAX NO OF M/G                            
         BAS   RE,RECNUM           RETURNS REC NUMBER IN WORK                   
*                                  ZERO IF M/G FOR ANOTHER UNIT                 
         MVC   RECNO,WORK                                                       
         MVC   RECDAT,NBACTDAT      UNIT DATA                                   
         MVC   RECSUB,NBACTSUB                                                  
         MVC   RECPROG,NBACTPRG                                                 
         MVC   RECKEY,NBKEY                                                     
*                                                                               
REP14    MVC   RECDAT2,NUMGDATE     M/G BY DATA                                 
         MVC   RECSUB2,NUMGSUB                                                  
         MVC   RECPRG2,NUMGPCOD                                                 
         BAS   RE,NEXTEL           ARE THERE MULTIPLE M/G                       
         BNE   GETUNIT                                                          
         LA    R6,RMGLENE(R6)      BUMP TO NXT M/G ELEM                         
         BCT   R0,REP14                                                         
         DC    H'0'                EXCEEDS NO OF M/G'S                          
         EJECT                                                                  
*                                                                               
REP30    NTR1                                                                   
* STEP THROUGH TABLE. IF RECDAT2 IS BEFORE REQUEST DATE                         
* GET THAT REC AND ADD TO TABLE                                                 
*                                                                               
         L     R2,ATABLE                                                        
         OC    0(6,R2),0(R2)                                                    
         BZ    XIT                  TABLE EMPTY/ EXIT                           
         L     R6,ATABLE                                                        
REP31    LA    R0,6                                                             
         USING RECTABLD,R6                                                      
REP31A   CLC   RECDAT2,NBCMPRSTR                                                
         BL    REP31B                                                           
         CLC   RECDAT2,NBCMPEND                                                 
         BNH   REP31C                                                           
REP31B   BAS   RE,READREC                                                       
         BAS   RE,ADDREC                                                        
REP31C   LA    R6,RMGLENE(R6)      ARE THERE MULTIPLE M/G                       
         BCT   R0,REP31A                                                        
         LA    R2,RECLENE(R2)      BUMP TO NEXT REC                             
         OC    0(6,R2),0(R2)       IS IT END OF TABLE                           
         BZ    *+12                                                             
         LR    R6,R2               NO/SO SET R6 START OF NEW REC                
         B     REP31                                                            
*                                                                               
* ALL UNITS HAVE BEEN READ.                                                     
* NOW STEP THROUGH TABLE AND ONLY CONSIDER                                      
* THOSE RECS THAT HAVE LINE NUMBERS. TAKE THEIR M/G DATE(S) AND CHK             
* IF THE M/G DATE(S) MATCH ANY RECDAT. IF YES, GIVE MATCHED REC THE             
* LINE NO + 1 OF FIRST REC.                                                     
*                                                                               
         L     R6,ATABLE                                                        
         USING RECTABLD,R6                                                      
         LR    R2,R6                STORE AD OF 1ST REC                         
         OC    RECNO,RECNO         DOES REC HAVE LINE NUMBER                    
         BZ    REP40                                                            
REP32    L     R5,ATABLE                                                        
REP32A   CLC   RECDAT2,4(R5)           IS THERE A DATE MATCH                    
         BNE   REP39                                                            
         OC    0(4,R5),0(R5)       DOES MATCHED REC HAVE LINE NO                
         BZ    *+6                                                              
         DC    H'0'                IF SO/BOMB                                   
         L     R1,RECNO                                                         
         LA    R1,1(R1)                                                         
         STCM  R1,15,0(R5)                                                      
         B     REP40                                                            
*                                                                               
REP39    LA    R5,RECLENE(R5)      BUMP TO NEXT REC TO MATCH                    
         OC    0(6,R5),0(R5)                                                    
         BNZ   REP32A                                                           
         LA    R6,RMGLENE(R6)      ARE THERE MULTIPLE M/G                       
         OC    0(2,R6),0(R6)                                                    
         BNZ   REP32                                                            
REP40    LA    R2,RECLENE(R2)      BUMP TO NEXT REC WITH LINE NO                
         OC    0(6,R2),0(R2)       END OF TABLE                                 
         BZ    REP50                                                            
         LR    R6,R2                                                            
         B     REP32                                                            
         EJECT                                                                  
REP50    DS    0H                                                               
* ALL RECS HAVE BEEN CHECKED FOR LINE NUMBERS                                   
* WE WILL IGNORE THOSE THAT HAVE NONE BUT PASS THEM TO SORTER                   
* NEVERTHELESS IN CASE WE NEED TO LOOK AT THEM LATER FOR CHECKING               
*                                                                               
         L     R2,ATABLE                                                        
REP52    GOTO1 SORTER,DMCB,=C'PUT',0(R2)                                        
         LA    R2,RECLENE(R2)                                                   
         OC    0(6,R2),0(R2)                                                    
         BNZ   REP52                                                            
*                                                                               
REP60    DS    0H                                                               
         LA    R2,P                                                             
         GOTO1 SORTER,DMCB,=C'GET'                                              
         L     R6,4(R1)                                                         
         LTR   R6,R6                                                            
         BZ    REP70                                                            
         USING RECTABLD,R6                                                      
         OC    PREVNO,PREVNO                                                    
         BZ    REP63                                                            
         CLC   PREVNO,RECNO                                                     
         BH    REP63A                                                           
REP63    L     R1,RECNO         ADD 10 / BUMPS OF M/G CHAINS ARE BY 10          
         LA    R1,10(R1)                                                        
         ST    R1,PREVNO                                                        
         BAS   RE,PRINT                             SKIP LINE                   
         MVI   NEWLINE,C'Y'                                                     
REP63A   GOTO1 DATCON,DMCB,(2,RECDAT),(5,P+9)                                   
         MVI   P+17,C'-'                                                        
         EDIT  (B1,RECSUB),(2,P+18)                                             
         MVC   P+1(6),RECPROG                                                   
         CLI   NEWLINE,C'Y'                                                     
         BNE   REP65                                                            
         MVI   NEWLINE,0                                                        
         MVC   P+20(12),=C'MADE GOOD BY'                                        
REP65    MVC   P+34(6),RECPRG2                                                  
         GOTO1 DATCON,DMCB,(2,RECDAT2),(5,P+41)                                 
         MVI   P+49,C'-'                                                        
         EDIT  (B1,RECSUB2),(2,P+50)                                            
         BAS   RE,PRINT                                                         
         B     REP60               GET ANOTHER REC                              
*                                                                               
REP70    DS    0H                                                               
         XC    NEWLINE,NEWLINE                                                  
         XC    LINENUM,LINENUM                                                  
         L     R1,ATABLE                                                        
REP72    OC    0(6,R1),0(R1)                                                    
         BZ    REP75                                                            
         XC    0(RECLENE,R1),0(R1) CLEAR TABLE                                  
         LA    R1,RECLENE(R1)                                                   
         B     REP72                                                            
REP75    DS    OH                                                               
         B     XIT                                                              
*                                                                               
         EJECT                                                                  
* THIS ROUTINE READS UNIT RECS WHOSE DATE FALLS OUTSIDE REQUEST                 
* PARAMETERS BUT WHO ARE POINTED TO AS M/G BY READ UNITS                        
* R6 POINTS TO RECORD                                                           
* PUT REC IN ANETWS1                                                            
READREC  NTR1                                                                   
         USING RECTABLD,R6                                                      
         LA    R2,KEY                                                           
         USING NUKPKEY,R2                                                       
         XC    KEY,KEY                                                          
         MVC   KEY,RECKEY          GET CURRENT KEY VALUES                       
         MVC   NUKPPROG,RECPRG2        SET UP FOR NEW PROG/DATE                 
         MVC   NUKPDATE,RECDAT2                                                 
         MVC   NUKPSUB,RECSUB2                                                  
         GOTO1 HIGH                                                             
         CLC   KEY,KEYSAVE                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 DATAMGR,DMCB,=C'GETREC',=C'UNTFILE ',KEY,ANETWS1,DMWORK          
         MVI   NBFUNCT,NBFRDHI     READ HI WHEN RESUMING NETIO READS            
RDX      B     XIT                                                              
         DROP  R2,R6                                                            
*                                                                               
* UNIT REC READ INTO ANETWS1                                                    
* THIS IS A M/G UNIT WHOSE DATE IS OUTSIDE REQUEST PARAMETERS BUT WAS           
* POINTED TO BY A READ UNIT.  IF IT IS MADE GOOD THEN ADD IT TO TABLE.          
* IF NOT, IT IS THE END OF A CHAIN AND WE ALREADY HAVE ITS DATA.                
*                                                                               
ADDREC   NTR1                                                                   
         LA    R0,6                                                             
         USING RECTABLD,R6                                                      
         L     R5,ANETWS1                                                       
         USING NUMGD,R5                                                         
         MVI   ELCODE,7                                                         
         BAS   RE,GETEL                                                         
         BNE   XIT                                                              
         L     R6,ATABLE                                                        
ADD5     OC    0(6,R6),0(R6)                                                    
         BZ    *+12                                                             
         LA    R6,RECLENE(R6)                                                   
         B     ADD5                                                             
         MVC   RECDAT,NBACTDAT      UNIT DATA                                   
         MVC   RECSUB,NBACTSUB                                                  
         MVC   RECPROG,NBACTPRG                                                 
         MVC   RECKEY,NBKEY                                                     
*                                                                               
ADD7     MVC   RECDAT2,NUMGDATE     M/G BY DATA                                 
         MVC   RECSUB2,NUMGSUB                                                  
         MVC   RECPRG2,NUMGPCOD                                                 
         BAS   RE,NEXTEL           ARE THERE MULTIPLE M/G                       
         BNE   ADDX                                                             
         LA    R6,RMGLENE(R6)      BUMP TO NXT M/G ELEM                         
         BCT   R0,ADD7                                                          
         DC    H'0'                EXCEEDS NO OF M/G'S                          
ADDX     B     XIT                                                              
         DROP  R5,R6                                                            
         EJECT                                                                  
*                                                                               
* IF UNIT IS NOT A MAKE GOOD FOR ANOTHER UNIT, IT IS THE START OF A             
* MAKE-GOOD CHAIN AND GETS A LINE NUMBER.                                       
*      LINE NUMBERS START AT 10 AND BUMP BY 10                                  
*      INPUT R6 - UNIT DATA IN RECTABLE                                         
*                                                                               
RECNUM   NTR1                                                                   
         XC    WORK(4),WORK                                                     
         MVI   ELCODE,6      IF UNIT IS A M/G FOR ANOTHER UNIT                  
         L     R5,NBAIO      THEN IT DOES NOT GET A DATA-LINE NO                
         BAS   RE,GETEL                                                         
         BNE   RNUMX                                                            
         L     R5,ATABLE          STEP THROUGH TABLE - GET HIGHEST NO           
         USING RECTABLD,R5                                                      
RNUM3    OC    0(6,R5),0(R5)                                                    
         BZ    RNUM7                                                            
RNUM5    CLC   RECNO,=4X'0'                                                     
         BE    *+10                                                             
         MVC   LINENUM,RECNO                                                    
         LA    R5,RECLENE(R5)                                                   
         B     RNUM3                                                            
RNUM7    L     R1,LINENUM                                                       
         A     R1,=F'10'                                                        
         MVC   WORK(4),LINENUM                                                  
RNUMX    B     XIT                                                              
*                                                                               
         GETEL (R5),DATADISP,ELCODE                                             
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
RECTABLE DS    CL30000       MORE THAN ENOUGH ROOM FOR 500 RECS MAX             
         EJECT                                                                  
*              WORKING STORAGE FOR PROGRAM                                      
         SPACE 3                                                                
MYD      DSECT                                                                  
*                                                                               
RELO     DS    A                                                                
LINENUM  DS    F                                                                
ATABLE   DS    F                                                                
NUMMONS  DS    F                                                                
PERTYPE  DS    1                                                                
ELCODE   DS    CL1                                                              
PEROPT   DS    CL1                                                              
NEWLINE  DS    CL1                                                              
MONLIST  DS    CL20                                                             
         EJECT                                                                  
RECTABLD DSECT                                                                  
RECNO    DS    CL4                 REC LINE NUMBER                              
RECDAT   DS    CL2                                                              
RECSUB   DS    CL1                                                              
RECPROG  DS    CL6                                                              
RECKEY   DS    CL20                                                             
*                                                                               
RECDAT2  DS    CL2                 1ST M/G/DATA                                 
RECSUB2  DS    CL1                                                              
RECPRG2  DS    CL6                                                              
RMGLENE  EQU   *-RECDAT2                                                        
         DS    CL36                 FOR 4 MORE M/G DATA                         
RECLENE  EQU   *-RECNO                                                          
         PRINT OFF                                                              
       ++INCLUDE NEGENINCLS                                                     
         EJECT                                                                  
       ++INCLUDE NEWRIFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE NEWRIE5D                                                       
       ++INCLUDE NEGENUNIT                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'012NEWRIN8   05/01/02'                                      
         END                                                                    
