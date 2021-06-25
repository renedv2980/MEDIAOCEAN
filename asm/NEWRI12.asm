*          DATA SET NEWRI12    AT LEVEL 051 AS OF 05/01/02                      
*PHASE T32012A                                                                  
*                                                                               
*HIPO******************************************************************         
*  NUMBER:                                                            *         
*                                                                     *         
*  TITLE: T32012 - NETWORK DRIVER ACCOUNTING EDIT PHASE               *         
*                                                                     *         
*  COMMENTS: VALIDATES NETWORK ACCOUNTING REPORT REQUESTS, INTERFACES *         
*            WITH DRIVER.                                             *         
*                                                                     *         
*  CALLED FROM: NETWORK SPOOL CONTROLLER NEMED00 (T31E00), WHICH      *         
*               CALLS DDGENCON (T00A30) WHICH CALLS THIS.             *         
*                                                                     *         
*  CALLS TO:    DATAMGR                                               *         
*               NVVALID - VALIDATION ROUTINES.                        *         
*                                                                     *         
*  INPUTS: SCREEN BUWRIF2 (T320F2) -- MAINTENANCE                     *         
*                                                                     *         
*  OUTPUTS - ADRIVE - A(DRIVE TABLE)                                  *         
*            AINTERN -A(START OF INTERNAL RECORD AREA)                *         
*                                                                     *         
*  LOCALS: REGISTER USAGE                                             *         
*          R0 - WORK                                                  *         
*          R1 - WORK                                                  *         
*          R2 - POINTER TO SCREEN FLDH                                *         
*          R3 - WORK                                                  *         
*          R4 - WORK                                                  *         
*          R5 - WORK                                                  *         
*          R6 - DRIVER GLOBALS                                        *         
*          R7 - ACCT BLOCK                                            *         
*          R8 - SPOOLD                                                *         
*          R9 - SYSD                                                  *         
*          RA - TWA                                                   *         
*          RB - FIRST BASE                                            *         
*          RC - GEND                                                  *         
*          RD - SYSTEM                                                *         
*          RE - SYSTEM                                                *         
*          RF - SYSTEM                                                *         
*                                                                     *         
*ENDHIPO***************************************************************         
         TITLE 'T32012 - NETWORK DRIVER ACCOUNTING REPORT'                      
T32012   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T32012**                                                       
*                                                                               
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING T320FFD,RA                                                       
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASYSD            NETWORK SYSTEM DSECT                         
         USING NETSYSD,R9                                                       
         L     R7,ANETWS2          USE ANETWS2 FOR ACCT BLOCK                   
         USING NACCBLKD,R7                                                      
*                                                                               
         CLI   MODE,VALREC                                                      
         BE    VR                                                               
         CLI   MODE,PRINTREP                                                    
         BE    VR                                                               
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
* VALIDATE THE RECORD                                                           
*                                                                               
VR       MVI   NBQINIT,0           DO ALL VALIDATIONS EVERY TIME                
*                                    NOT USED                                   
         MVI   NBDATA,C'B'         WILL WANT PACKAGE RECORD FIRST               
         L     R2,ANETWS1          USE 1ST W/S AREA TO PASS CLIENT              
         ST    R2,NBACLI           I/O AREA TO SAVE CLIENT RECORD               
*                                    TO PASS TO PRINT MODULE                    
*                                                                               
         LA    R2,SPLCLIH          CLIENT (REQUIRED. ALL ALLOWED)               
         NETGO NVCLIALL,DMCB              AND FILL IN UNLCLIN.                  
*                                  (RETURNS CLIENT RECORD IN ANETWS1)           
*                                                                               
         LA    R2,SPLPROH          PRODUCT FIELD (REQUIRED)                     
         NETGO NVPRDALL,DMCB                                                    
*                                                                               
         MVI   FTERMFLG,1          FOLLOWING ARE OPTIONAL                       
         LA    R2,SPLESTH          ESTIMATE. OPTIONAL.                          
         NETGO NVESTRNG,DMCB                                                    
*                                                                               
         LA    R2,SPLNETH          NETWORK ('ALL' IS ALLOWED)                   
         NETGO NVNETALL,DMCB                                                    
*                                                                               
         LA    R2,SPLDPTH          DAYPART FIELD (OPTIONAL)                     
         NETGO NVDPT,DMCB                                                       
*                                                                               
         LA    R2,SPLPAKH          PACKAGE FIELD (OPTIONAL)                     
         NETGO NVPAKALL,DMCB                                                    
*                                                                               
         LA    R2,SPLSTRTH         START DATE                                   
         NETGO NVSTRDAT,DMCB                                                    
*                                                                               
         LA    R2,SPLENDH          END DATE                                     
         NETGO NVENDDAT,DMCB                                                    
*                                                                               
         XC    NACSBDT,NACSBDT                                                  
         XC    NACSPDT,NACSPDT     CLEAR DATES                                  
         LA    R2,SPLBSTRH         BILL START                                   
         NETGO NVGETFLD,DMCB                                                    
         BZ    DTBEND                                                           
         GOTO1 DATVAL,DMCB,(0,FLD),DUB                                          
         OC    DMCB(4),DMCB                                                     
         BZ    DTDTERR                                                          
         GOTO1 DATCON,DMCB,(0,DUB),(2,NACSBDT)                                  
*                                                                               
DTBEND   LA    R2,SPLBENDH         BILL END                                     
         NETGO NVGETFLD,DMCB                                                    
         BZ    DTPSTR                                                           
         GOTO1 DATVAL,DMCB,(0,FLD),DUB                                          
         OC    DMCB(4),DMCB                                                     
         BZ    DTDTERR                                                          
         GOTO1 DATCON,DMCB,(0,DUB),(2,NACSBDT+2)                                
*                                                                               
DTPSTR   LA    R2,SPLPSTRH         PAY START                                    
         NETGO NVGETFLD,DMCB                                                    
         BZ    DTPEND                                                           
         GOTO1 DATVAL,DMCB,(0,FLD),DUB                                          
         OC    DMCB(4),DMCB                                                     
         BZ    DTDTERR                                                          
         GOTO1 DATCON,DMCB,(0,DUB),(2,NACSPDT)                                  
*                                                                               
DTPEND   LA    R2,SPLPENDH         PAY END                                      
         NETGO NVGETFLD,DMCB                                                    
         BZ    DRONE1                                                           
         GOTO1 DATVAL,DMCB,(0,FLD),DUB                                          
         OC    DMCB(4),DMCB                                                     
         BZ    DTDTERR                                                          
         GOTO1 DATCON,DMCB,(0,DUB),(2,NACSPDT+2)                                
         B     DRONE1                                                           
*                                                                               
DTDTERR  MVI   ERROR,INVDATE                                                    
         B     DTERR                                                            
         EJECT                                                                  
* DRONE CALLS                                                                   
*                                                                               
DRONE1   XC    DRGEN,DRGEN         INITIALIZE DRONE BLOCK FOR INPUT             
         MVI   DRWHO,DRNETWHO                                                   
         MVI   DRACTION,DRINIT                                                  
         MVC   DRDICT,=CL8'NETA'                                                
         MVC   DRALTDIC,=CL8'NETACC  '                                          
         L     R3,ANETWS2                                                       
         LA    R3,NACDTLN(R3)      ACC BLOCK, THEN PROG IN ANETWS2              
         ST    R3,DRSTBUF                                                       
         LA    R3,3000(R3)                                                      
         ST    R3,DRENDBUF                                                      
         MVC   DRCOMFAC,NBACOM                                                  
         GOTO1 NDDRONE,DMCB,DRGEN                                               
         CLI   DRERROR,0                                                        
         BNE   ERR                                                              
*                                                                               
         LA    R2,SPLROWH          A(FIRST ROW)                                 
         LA    R3,SPLROWLH         A(LAST ROW)                                  
*                                                                               
PR10     CLI   5(R2),0             TEST ANY INPUT                               
         BE    PR15                                                             
*                                                                               
         MVI   DRACTION,DRROW      VALIDATE THIS ROW                            
         ST    R2,DRNETFLD                                                      
         GOTO1 NDDRONE,DMCB,DRGEN                                               
         CLI   DRERROR,0                                                        
         BNE   ERR                                                              
*                                                                               
         CLI   OFFLINE,C'Y'        IF OFFLINE, GENERATE DRIVER CODE             
         BNE   PR15                                                             
         MVI   DRACTION,DRGENROW                                                
         GOTO1 NDDRONE,DMCB,DRGEN                                               
         CLI   DRERROR,0                                                        
         BNE   ERR                                                              
*                                                                               
PR15     ZIC   R0,0(R2)            BUMP TO NEXT ROW                             
         AR    R2,R0                                                            
         CR    R2,R3               TEST LAST ROW                                
         BNH   PR10                                                             
*                                                                               
         LA    R2,SPLCOLH          A(FIRST COLUMN)                              
         LA    R3,SPLCOLLH         A(LAST COLUMN)                               
*                                                                               
PR20     CLI   5(R2),0             TEST ANY INPUT                               
         BE    PR25                                                             
*                                                                               
         MVI   DRACTION,DRCOL      VALIDATE THIS COLUMN                         
         ST    R2,DRNETFLD                                                      
         GOTO1 NDDRONE,DMCB,DRGEN                                               
         CLI   DRERROR,0                                                        
         BNE   ERR                                                              
*                                                                               
         CLI   OFFLINE,C'Y'        IF OFFLINE, GENERATE DRIVER ELEMENTS         
         BNE   PR25                                                             
         MVI   DRACTION,DRGENCOL                                                
         GOTO1 NDDRONE,DMCB,DRGEN                                               
         CLI   DRERROR,0                                                        
         BNE   ERR                                                              
*                                                                               
PR25     ZIC   R0,0(R2)            BUMP TO NEXT COLUMN                          
         AR    R2,R0                                                            
         CR    R2,R3               TEST LAST COLUMN                             
         BNH   PR20                                                             
*                                                                               
         MVC   NDTITLE,=CL40'OVERNIGHT ACCOUNTING REPORT'   DEFAULT             
         LA    R2,SPLTITH          TITLE                                        
         NETGO NVGETFLD,DMCB                                                    
         BZ    *+10                                                             
         MVC   NDTITLE(40),FLD                                                  
*                                                                               
         MVI   DRACTION,DRWRAPUP   WRAP UP DRONE                                
         GOTO1 NDDRONE,DMCB,DRGEN                                               
         CLI   DRERROR,0                                                        
         BNE   ERR                                                              
*                                                                               
         MVI   DOWNSW,C'N'         DEFAULT IS NO DOWNLOAD                       
         LA    R2,SPLOPTSH         OPTIONS FIELD                                
         NETGO NVGETFLD                                                         
         BZ    PR50                                                             
         CLI   5(R2),4                                                          
         BNE   INVOPT                                                           
         CLC   =C'DOWN',FLD                                                     
         BNE   INVOPT                                                           
         MVI   DOWNSW,C'Y'         DOWNLOAD REPORT                              
*                                                                               
PR50     LA    R2,SPLCLIH          NORMAL END OF EDIT                           
         MVI   NBUSER+8,C'Y'       FORCE ACCTG REPORT TO USE ASSIGNED           
*                                                                               
         CLI   MODE,PRINTREP       IF OFFLINE, GENERATE REPORT                  
         BNE   XIT                                                              
         EJECT                                                                  
* OFFLINE REPORT GENERATION                                                     
*                                                                               
         NETGO NVDRINIT,DMCB       INITIALIZE DRIVER                            
*                                                                               
         LA    R2,HOOK                                                          
         ST    R2,HEADHOOK                                                      
         LA    R2,HEADING                                                       
         ST    R2,SPECS                                                         
*                                                                               
         L     R6,NDGLOBAL         DRIVER GLOBALS                               
         USING GLOBALD,R6                                                       
*                                                                               
         LA    R2,HOOK                                                          
         ST    R2,GLAHOOK                                                       
         ST    R7,AACCBLK                                                       
*                                                                               
         CLI   DOWNSW,C'Y'         TEST DOWNLOAD ACTIVE                         
         BNE   *+8                                                              
         OI    GLDOWNLD,X'80'                                                   
*                                                                               
         LA    R1,NACDTLN(R7)      DPG PROG PASSED HERE                         
         ST    R1,GLAPROG                                                       
*                                                                               
         MVI   GLMODE,GLINIT       INITIALIZE DRIVER                            
         GOTO1 NDDRIVER,DMCB,(R6)                                               
*                                                                               
         MVI   NREPTYP,C'A'        SET UP AS ACCTG REPORT                       
         MVI   PERTYPE,C'W'        SET UP FOR WEEKS                             
         MVI   PERTYPE+1,1         USE MONTHS IF TOO MANY WEEKS                 
         MVI   PERTYPE+2,0         NEVER USE QUARTERS                           
         EJECT                                                                  
PROCDAT  NETGO NSNETIO,DMCB,NETBLOCK                                            
         CLI   NBERROR,0                                                        
         BNE   PROCERR                                                          
         CLI   NBMODE,NBVALDAT     PROCESS DATES                                
         BE    IN6                                                              
         B     PROCDAT             OTHER MODES ARE IGNORED                      
*                                                                               
IN6      LA    R1,MAXMONTS         SET UP MAX SIZE OF MONTH (WEEK) LIST         
         ST    R1,NUMMONS          GET LIST INTO MONLIST. NUMMONS IS            
*                                    NEW SIZE OF LIST                           
         MVI   PERTYPE,C'M'                                                     
         NETGO NVWKLST,DMCB,NUMMONS,MONLIST,PERTYPE                             
*                                                                               
         LA    R1,MONLIST                                                       
         ST    R1,APERLST1         SET UP ADDRESS FOR NET DRIVER                
*                                                                               
         MVI   NBDATA,C'U'         SELECT UNIT RECORDS                          
         MVI   NBUSER+13,C'N'      DONT FILTER PRE-EMPTS                        
*                                                                               
GETFIRST NETGO NSNETIO,DMCB,NETBLOCK   GET FIRST UNIT                           
         CLI   NBERROR,0                                                        
         BNE   PROCERR                                                          
         CLI   NBMODE,NBPROCUN     IF  UNIT RECORD                              
         BNE   GF2                                                              
         MVI   NACINIT,0           A NEW UNIT SO INIT ACCTG BLOCK               
         BAS   RE,FILLAC           FILL FIRST AC BLOCK                          
         CLI   NACMODE,NACREJ      IF DATA DOESN'T MEET FILTER                  
         BE    GETFIRST                                                         
         B     DOUNIT                                                           
GF2      CLI   NBMODE,NBREQLST     IF NO UNITS                                  
         BE    TXIT                 THEN DONE                                   
         B     GETFIRST            OTHER MODES ARE IGNORED                      
*                                                                               
GETNEXT  NETGO NSNETIO,DMCB,NETBLOCK                                            
         CLI   NBERROR,0                                                        
         BNE   PROCERR                                                          
         CLI   NBMODE,NBPROCUN     IF  UNIT RECORD                              
         BNE   GN2                                                              
         MVI   NACINIT,0           A NEW UNIT SO INIT ACCTG BLOCK               
         BAS   RE,FILLAC           FILL FIRST AC BLOCK                          
         CLI   NACMODE,NACREJ      IF DATA DOESN'T MEET FILTER                  
         BE    GETNEXT                                                          
         B     DOUNIT                                                           
GN2      CLI   NBMODE,NBREQLST     IF NO UNITS                                  
         BE    TOTALS                                                           
         B     GETNEXT             OTHER MODES ARE IGNORED                      
*                                                                               
DOUNIT   MVI   GLMODE,GLINPUT                                                   
         GOTO1 NDDRIVER,DMCB,(R6)                                               
*                                                                               
         CLI   NACMODE,NACBP       CK IF MORE TO FILTER                         
         BNE   GETNEXT                                                          
ACCTLOOP BAS   RE,FILLAC           FILL NEXT AC BLOCK                           
         CLI   NACMODE,NACBP       CK IF LAST ACCT BLOCK                        
         BNE   GETNEXT                                                          
         MVI   GLMODE,GLINPUT      AUDIT TRAIL. REPROCESS UNIT                  
         GOTO1 NDDRIVER,DMCB,(R6)                                               
         B     ACCTLOOP                                                         
*                                                                               
TOTALS   MVI   GLMODE,GLOUTPUT                                                  
         GOTO1 NDDRIVER,DMCB,(R6)                                               
*                                                                               
TXIT     B     XIT                                                              
         DROP  R6                                                               
*                                                                               
PROCERR  DC    F'0'                                                             
         EJECT                                                                  
********************************************************                        
* FILLAC - ROUTINE TO FILL ACCOUNTING BLOCK                                     
*          SUPPORTS PAID/CLEARED FILTER DATES                                   
*          LOCALS:  R5 - NACCTBLOCK                                             
*                   R6 - USED FOR NEXTEL                                        
********************************************************                        
FILLAC   NTR1                                                                   
         CLI   NACINIT,1                                                        
         BNE   FAC2                                                             
         XC    NBACTUAL,NBACTUAL   IF SECOND PASS, SET COSTS TO ZERO.           
         XC    NBASSIGN,NBASSIGN   THEY HAVE ALREADY BEEN PRINTED,ADDED         
         XC    NBINTEG,NBINTEG                                                  
         XC    NBCALCOS,NBCALCOS                                                
         MVI   NBUNITST,0          SET UNIT STATUS TO 0 FOR THESE LINES         
         B     PU5                                                              
*                                                                               
FAC2     L     R3,NBAIO            INITIALIZE                                   
         USING NURECD,R3                                                        
         LA    R4,NUMAINEL                                                      
         DROP  R3                                                               
         ST    R4,NACABILE         SET CURRENT ELEMENTS TO FIRST EL             
         ST    R4,NACAPAYE                                                      
         MVI   NACMODE,0           INITIALIZE                                   
         MVI   NACINIT,1                                                        
*                                                                               
         TM    NBUNITST,X'42'      IF PRE-EMPT OR MISSED                        
         BZ    PU1                                                              
         XC    NBACTUAL,NBACTUAL   SET COSTS TO 0 BUT                           
         XC    NBASSIGN,NBASSIGN   LEAVE BILLING AND PAYING                     
         XC    NBINTEG,NBINTEG                                                  
         XC    NBCALCOS,NBCALCOS                                                
*                                                                               
*  CONDITIONAL ASSIGNED COST                                                    
*                                                                               
PU1      CLI   NBUSER+8,C'Y'       IF FLAG SET TO USE ASSIGNED                  
         BNE   PU3                                                              
         OC    NBASSIGN,NBASSIGN   AND IF ASSIGNED COST NON-ZERO                
         BNZ   PU2                                                              
         TM    NBUNITST,X'08'      OR ASSIGNED COST TRULY ZERO                  
         BZ    PU3                                                              
PU2      MVC   NBCALCOS,NBASSIGN   THEN USE ASSIGNED COST                       
         B     *+10                                                             
PU3      MVC   NBCALCOS,NBACTUAL   ELSE USE ACTUAL COST                         
*                                                                               
         CLI   NDPRDRMD,NDALL      IF ONE PRODUCT                               
         BE    PU5                                                              
         CLI   NBPRD2,0                                                         
         BE    PU5                                                              
*                                                                               
PRDPCT   SR    R6,R6                                                            
         ICM   R6,3,NBP1SHR        PCTG FOR FIRST PRODUCT                       
         CLC   NBEFFPNM,NBPRD      IF THIS FIRST PRODUCT THEN GO ON             
         BE    APRD2               IF NOT ITS PROD2                             
         CLC   NBEFFPNM,NBPRD2                                                  
         BE    APRD1                                                            
         DC    H'0'                                                             
APRD1    L     R5,=F'10000'        PUT 100.00 IN R5                             
         SR    R5,R6               THIS IS PCTG FOR PROD 2                      
         LR    R6,R5                                                            
APRD2    DS    0H                                                               
*                                                                               
         L     R5,NBACTUAL         ADJUST ACTUAL                                
         MR    R4,R6                                                            
         SLDA  R4,1                DOUBLE FOR ROUNDING                          
         D     R4,=F'10000'                                                     
         LTR   R5,R5                                                            
         BM    *+8                                                              
         AH    R5,=H'1'                                                         
         SRA   R5,1                                                             
         ST    R5,NBACTUAL                                                      
*                                                                               
         L     R5,NBASSIGN         ADJUST ASSIGNED COST                         
         MR    R4,R6                                                            
         SLDA  R4,1                DOUBLE FOR ROUNDING                          
         D     R4,=F'10000'                                                     
         LTR   R5,R5                                                            
         BM    *+8                                                              
         AH    R5,=H'1'                                                         
         SRA   R5,1                                                             
         ST    R5,NBASSIGN                                                      
*                                                                               
         L     R5,NBCALCOS         ADJUST CALCULATED ASSIGNED COST              
         MR    R4,R6                                                            
         SLDA  R4,1                DOUBLE FOR ROUNDING                          
         D     R4,=F'10000'                                                     
         LTR   R5,R5                                                            
         BM    *+8                                                              
         AH    R5,=H'1'                                                         
         SRA   R5,1                                                             
         ST    R5,NBCALCOS                                                      
*                                                                               
         L     R5,NBINTEG          ADJUST INTEGRATION COST                      
         MR    R4,R6                                                            
         SLDA  R4,1                DOUBLE FOR ROUNDING                          
         D     R4,=F'10000'                                                     
         LTR   R5,R5                                                            
         BM    *+8                                                              
         AH    R5,=H'1'                                                         
         SRA   R5,1                                                             
         ST    R5,NBINTEG                                                       
*                                                                               
PU5      XC    NACDTFLD(NACDTLN),NACDTFLD   CLEAR DATA FIELDS                   
*                                                                               
         L     R6,NACABILE         GET NEXT BILL ELEMENT                        
         MVI   SRCHEL,X'10'                                                     
*                                                                               
NEXTBEL  BAS   RE,NEXTEL                                                        
         BNE   DOPAY               LAST ELEMENT                                 
         USING NUBILD,R6                                                        
         ST    R6,NACABILE         NEW CURRENT ELEMENT                          
         TM    NUBILST,X'20'       CHECK IF UNBILLED                            
         BO    NEXTBEL                                                          
         CLI   NDPRDRMD,NDALL      EXTRA FILTERING IF 1 PROD                    
         BE    *+14                                                             
         CLC   NBEFFPNM,NUBILPRD                                                
         BNE   NEXTBEL                                                          
*                                                                               
         CLI   NACSBTY,0           CK IF BILL TYPE FILTER                       
         BE    *+14                                                             
         CLC   NACSBTY,NUBILBTY                                                 
         BNE   NEXTBEL                                                          
*                                                                               
         OC    NACSBDT(2),NACSBDT                                               
         BZ    *+14                DATE FILTER                                  
         CLC   NACSBDT(2),NUBILDAT                                              
         BH    NEXTBEL                                                          
*                                                                               
         OC    NACSBDT+2(2),NACSBDT+2                                           
         BZ    *+14                                                             
         CLC   NACSBDT+2(2),NUBILDAT                                            
         BL    NEXTBEL                                                          
*                                                                               
         CLI   NUBILTYP,C'T'       DO SUMS                                      
         BNE   DOBILL18                                                         
*                                                                               
         MVI   NACBTFLG,1          ELEMENT EXISTS                               
         L     R2,NUBILGRS                                                      
         A     R2,NACBTGRS                                                      
         ST    R2,NACBTGRS                                                      
         L     R2,NUBILNET                                                      
         A     R2,NACBTNET                                                      
         ST    R2,NACBTNET                                                      
*                                                                               
         OC    NACBTDAT,NACBTDAT    FILL IN DATE.                               
         BNZ   *+14                                                             
         MVC   NACBTDAT,NUBILDAT    MOVE DATE IF NONE ALREADY                   
         B     NEXTBEL                                                          
*                                                                               
         CLC   NACBTDAT,NUBILDAT    IF ONE ALREADY, MAKE SURE ITS SAME          
         BE    NEXTBEL                                                          
         MVC   NACBTDAT,=XL2'FFFF'  ELSE SET TO FFS                             
         B     NEXTBEL                                                          
*                                                                               
DOBILL18 MVI   NACBIFLG,1           ELEMENT EXISTS                              
         L     R2,NUBILGRS          INTEGRATION                                 
         A     R2,NACBIGRS                                                      
         ST    R2,NACBIGRS                                                      
         L     R2,NUBILNET                                                      
         A     R2,NACBINET                                                      
         ST    R2,NACBINET                                                      
*                                                                               
         OC    NACBIDAT,NACBIDAT    FILL IN DATE.                               
         BNZ   *+14                                                             
         MVC   NACBIDAT,NUBILDAT    MOVE DATE IF NONE ALREADY                   
         B     NEXTBEL                                                          
*                                                                               
         CLC   NACBIDAT,NUBILDAT    IF ONE ALREADY, MAKE SURE ITS SAME          
         BE    NEXTBEL                                                          
         MVC   NACBIDAT,=XL2'FFFF'  ELSE SET TO FFS                             
         B     NEXTBEL                                                          
*                                                                               
DOPAY    L     R6,NACAPAYE          GET NEXT PAY ELEMENT                        
         MVI   SRCHEL,X'12'                                                     
NEXTPEL  BAS   RE,NEXTEL                                                        
         BNE   FACXIT               LAST ELEMENT                                
         USING NUPAYD,R6                                                        
         ST    R6,NACAPAYE          NEW CURRENT ELEMENT                         
*                                                                               
         OC    NACSPDT(2),NACSPDT                                               
         BZ    *+14                 DATE FILTER                                 
         CLC   NACSPDT(2),NUPAYDAT                                              
         BH    NEXTPEL                                                          
*                                                                               
         OC    NACSPDT+2(2),NACSPDT+2                                           
         BZ    *+14                                                             
         CLC   NACSPDT+2(2),NUPAYDAT                                            
         BL    NEXTPEL                                                          
*                                                                               
         CLI   NUPAYTYP,C'T'        DO SUMS                                     
         BNE   PAY18                                                            
*                                                                               
         MVI   NACPTFLG,1           ELEMENT EXISTS                              
         L     R2,NUPAYGRS                                                      
         A     R2,NACPTGRS                                                      
         ST    R2,NACPTGRS                                                      
         L     R2,NUPAYNET                                                      
         A     R2,NACPTNET                                                      
         ST    R2,NACPTNET                                                      
*                                                                               
         OC    NACPTDAT,NACPTDAT    FILL IN DATE.                               
         BNZ   *+14                                                             
         MVC   NACPTDAT,NUPAYDAT    MOVE DATE IF NONE ALREADY                   
         B     NEXTPEL                                                          
         CLC   NACPTDAT,NUPAYDAT    IF ONE ALREADY, MAKE SURE ITS SAME          
         BE    NEXTPEL                                                          
         MVC   NACPTDAT,=XL2'FFFF'  ELSE SET TO FFS                             
         B     NEXTPEL                                                          
*                                                                               
PAY18    MVI   NACPIFLG,1           ELEMENT EXISTS                              
         L     R2,NUPAYGRS          INTEGRATION                                 
         A     R2,NACPIGRS                                                      
         ST    R2,NACPIGRS                                                      
         L     R2,NUPAYNET                                                      
         A     R2,NACPINET                                                      
         ST    R2,NACPINET                                                      
*                                                                               
         OC    NACPIDAT,NACPIDAT    FILL IN DATE.                               
         BNZ   *+14                                                             
         MVC   NACPIDAT,NUPAYDAT    MOVE DATE IF NONE ALREADY                   
         B     NEXTPEL                                                          
*                                                                               
         CLC   NACPIDAT,NUPAYDAT    IF ONE ALREADY, MAKE SURE ITS SAME          
         BE    NEXTPEL                                                          
         MVC   NACPIDAT,=XL2'FFFF'  ELSE SET TO FFS                             
         B     NEXTPEL                                                          
         DROP  R6                                                               
*                                                                               
FACXIT   CLI   NACPTFLG,0           IF ANY FLAGS SET, DONT REJECT               
         BNZ   FAC12                                                            
         CLI   NACPIFLG,0                                                       
         BNZ   FAC12                                                            
         CLI   NACBTFLG,0                                                       
         BNZ   FAC12                                                            
         CLI   NACBIFLG,0                                                       
         BNZ   FAC12                                                            
*                                   NOW CK FILTERS                              
         OC    NACSPDT,NACSPDT      IF ANY FILTER, THEN REJECT                  
         BNZ   FAC8                                                             
         OC    NACSBDT,NACSBDT                                                  
         BNZ   FAC8                                                             
         OC    NACSBTY,NACSBTY                                                  
         BZ    FAC12                                                            
*                                                                               
FAC8     MVI   NACMODE,NACREJ       REJECT                                      
         B     XIT                                                              
*                                                                               
FAC12    MVI   NACMODE,NACEND       END                                         
         B     XIT                                                              
         EJECT                                                                  
* HEADLINE ROUTINES                                                             
*                                                                               
HOOK     NTR1                                                                   
*                                                                               
         LA    R5,HEAD1                                                         
         USING PHEAD,R5                                                         
         L     R6,NDGLOBAL         DRIVER GLOBALS                               
         USING GLOBALD,R6                                                       
*                                                                               
         CLI   GLHOOK,GLHEAD                                                    
         BNE   HKXIT                                                            
*                                                                               
         MVC   H1+48(40),NDTITLE                                                
         GOTO1 UNDERLIN,DMCB,(40,H1+48),(X'BF',H2+48)                           
         MVC   H4+10(3),NDCLIABR                                                
         MVC   H5+10(3),NDPRDABR                                                
         MVC   H6+10(3),NDESTABR                                                
         MVC   H4+18(20),NDCLINAM                                               
         MVC   H5+18(20),NDPRDNAM                                               
         MVC   H6+18(24),NDESTNAM                                               
         MVC   H5+120(8),NDDPTNAM                                               
         MVC   H6+105(3),NDPAKABR                                               
*                                                                               
         LA    R4,PHBPLINE         A(PRINT LINE)                                
         OC    NACSBDT,NACSBDT     PRINT BILL LINE IF DATES GIVEN               
         BZ    HH20                                                             
         MVC   0(L'PHBPLINE,R4),SPACES    CLEAR LINE                            
         MVC   0(6,R4),=C'BILLED'                                               
         OC    NACSBDT(2),NACSBDT         IF START DATE                         
         BZ    HH14                                                             
         OC    NACSBDT+2(2),NACSBDT+2     ....IF END DATE                       
         BZ    HH12                                                             
         GOTO1 NBDATCON,DMCB,(2,NACSBDT),(5,7(R4))                              
         MVI   16(R4),C'-'                                                      
         GOTO1 NBDATCON,DMCB,(2,NACSBDT+2),(5,18(R4))                           
         B     HH16                       ....ELSE                              
*                                                                               
HH12     MVC   7(5,R4),=C'AFTER'                                                
         GOTO1 NBDATCON,DMCB,(2,NACSBDT),(5,13(R4))                             
         B     HH16                       ....FI                                
HH14     MVC   7(6,R4),=C'BEFORE'         ELSE (END, NO START)                  
         GOTO1 NBDATCON,DMCB,(2,NACSBDT+2),(5,14(R4))                           
HH16     LA    R4,PHBPLIN2                FI                                    
*                                                                               
HH20     OC    NACSPDT,NACSPDT     PRINT PAY LINE IF DATES GIVEN                
         BZ    HH30                                                             
         MVC   0(L'PHBPLINE,R4),SPACES    CLEAR LINE                            
         MVC   0(4,R4),=C'PAID'                                                 
         OC    NACSPDT(2),NACSPDT         IF START DATE                         
         BZ    HH24                                                             
         OC    NACSPDT+2(2),NACSPDT+2     ....IF END DATE                       
         BZ    HH22                                                             
         GOTO1 NBDATCON,DMCB,(2,NACSPDT),(5,7(R4))                              
         MVI   16(R4),C'-'                                                      
         GOTO1 NBDATCON,DMCB,(2,NACSPDT+2),(5,18(R4))                           
         B     HH26                       ....ELSE                              
*                                                                               
HH22     MVC   7(5,R4),=C'AFTER'                                                
         GOTO1 NBDATCON,DMCB,(2,NACSPDT),(5,13(R4))                             
         B     HH26                       ....FI                                
HH24     MVC   7(6,R4),=C'BEFORE'         ELSE (END, NO START)                  
         GOTO1 NBDATCON,DMCB,(2,NACSPDT+2),(5,14(R4))                           
HH26     LA    R4,132(R4)          NEXT LINE                                    
*                                                                               
HH30     LA    R1,DFL              OFFSET WHERE WE ARE NOW POINTING             
         SR    R4,R1               RESET TO BEGINNING OF LINE                   
*                                                                               
         OC    NACPCT,NACPCT       CHECK IF A PERCENTAGE USED                   
         BZ    HKXIT               IF SO, PUT IT IN HEADLINE                    
         CLC   NACPCT,=F'10000'    (DONT PRINT IF = 100.00)                     
         BE    HKXIT                                                            
         MVC   0(7,R4),=C'PERCENT'                                              
         EDIT  NACPCT,(6,11(R4)),2                                              
         LA    R4,132(R4)                                                       
*                                                                               
HKXIT    B     XIT                                                              
         DROP  R5,R6                                                            
         EJECT                                                                  
         GETEL R6,NBDTADSP,SRCHEL                                               
         SPACE 3                                                                
HEADING  SSPEC H1,1,C'MEDIA     NETWORK T.V.'                                   
         SSPEC H1,97,AGYNAME                                                    
         SSPEC H2,1,REQUESTOR                                                   
         SSPEC H2,97,AGYADD                                                     
         SSPEC H4,1,C'CLIENT'                                                   
         SSPEC H4,49,PERIOD                                                     
         SSPEC H4,97,NETREP                                                     
         SSPEC H4,124,PAGE                                                      
         SSPEC H5,1,C'PRODUCT'                                                  
         SSPEC H5,97,C'NETWORK'                                                 
         SSPEC H5,112,C'DAYPART'                                                
         SSPEC H6,1,C'ESTIMATE'                                                 
         SSPEC H6,97,C'PACKAGE'                                                 
         DC    X'00'                                                            
         SPACE 3                                                                
DTERR    GOTO1 ERREX,DMCB          ERROR                                        
*                                                                               
INVOPT   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'INVOPTM),INVOPTM                                       
         GOTO1 ERREX2                                                           
INVOPTM  DC    C'* ERROR * INVALID OPTION *'                                    
*                                                                               
ERR      MVC   CONHEAD,DRERRMSG                                                 
         GOTO1 ERREX2                                                           
         EJECT                                                                  
         LTORG                                                                  
         SPACE 5                                                                
DOWNSW   DS    CL1                 'Y' FOR DOWNLOAD                             
PERTYPE  DS    CL3                                                              
SRCHEL   DS    CL1                                                              
NUMMONS  DS    F                                                                
MAXMONTS EQU   100                 LARGE MONTH BUFFER                           
MONLIST  DS    (4*MAXMONTS)CL1                                                  
         SPACE 3                                                                
PHEAD    DSECT                     DSECT FOR HEADERS (MUST MATCH SSPEC)         
*                                                                               
PHLENGTH EQU   132                 HEADER LINE LENGTH                           
DFL      EQU   48                  OFFSET TO DATE FILTER LINE                   
         ORG   PHEAD+4*PHLENGTH+DFL                                             
PHBPLINE DS    CL30                BILLED OR PAID DATE FILTER LINE              
         ORG   PHEAD+5*PHLENGTH+DFL                                             
PHBPLIN2 DS    CL30                BILLED OR PAID DATE FILTER LINE              
         PRINT OFF                                                              
       ++INCLUDE NEWRIFFD                                                       
         PRINT ON                                                               
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE NEWRIF2D                                                       
         PRINT OFF                                                              
       ++INCLUDE NEGENINCLS                                                     
         PRINT ON                                                               
         EJECT                                                                  
       ++INCLUDE NEACCTBLK                                                      
         EJECT                                                                  
       ++INCLUDE NEGENUNIT                                                      
         EJECT                                                                  
       ++INCLUDE DRGLOBAL                                                       
         SPACE 5                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'051NEWRI12   05/01/02'                                      
         END                                                                    
