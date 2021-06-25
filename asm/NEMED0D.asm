*          DATA SET NEMED0D    AT LEVEL 005 AS OF 02/18/04                      
*PHASE T31E0DA,+0                                                               
*                                                                               
         TITLE 'T31E0D - EDIT FOR AGENCY SUMMARY'                               
*****************************************************************               
* T31E0D (NEMED0D) - THIS EDITS THE AGY SUMMARY SCREEN.AND READS THE            
*                                                                               
* INPUTS - PARAMETER 1 - LOCATION OF THE GEND SPOOL DSECT. CONTAINS             
*                           MANY USEFUL ADDRESSES, DATA                         
*                                                                               
* OUTPUTS - NETBLOCK - THE BLOCK USED TO READ THE NETWORK FILE.                 
*                      MANY FIELDS FILLED IN BY NETIO.                          
*           ANETWS1 - USED AS I/O AREA. CONTAINS THE CLIENT RECORD.             
*                                                                               
* GLOBALS - R2 - POINTS TO CURRENT FIELD ON SCREEN                              
*                                                                               
*  CALLS TO -                                                                   
*   NVVALID - VALIDATION ROUTINES.                                              
******************************************************************              
*                                                                               
         PRINT NOGEN                                                            
T31E0D   CSECT                                                                  
         NMOD1 0,**AGED**                                                       
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING T31EFFD,RA                                                       
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASYSD            NETWORK SYSTEM DSECT                         
         USING NETSYSD,R9                                                       
         L     R6,ANETWS2          ARGS TO PRINT                                
         A     R6,=F'1000'                                                      
         USING WORKD,R6                                                         
         LA    R1,STWRDLST                                                      
         ST    R1,NBASTWRD                                                      
         EJECT                                                                  
*************  SECURITY *************                                           
         CLI   OFFLINE,C'Y'                                                     
         BE    AG1                                                              
         CLC   NBSELAGY,=C'MX'     FOR THE FOLLOWING AGENCIES                   
         BE    AGERR               DISALLOW ON LINE AGYSUMMARY REQ              
         CLC   NBSELAGY,=C'WI'                                                  
         BE    AGERR                                                            
         CLC   NBSELAGY,=C'WR'                                                  
         BE    AGERR                                                            
         CLC   NBSELAGY,=C'WT'                                                  
         BNE   AG1                                                              
AGERR    MVI   ERROR,INVALID                                                    
         B     DTERR                                                            
                                                                                
*************  INITIALIZE NETBLOCK*************                                 
*                             ASSUMES NETBLOCK IS ALREADY INITIALIZED           
*                             DONE BY CALL TO NVAGY OR NVAGYOUT                 
AG1      MVI   PRMARGA,0           SET ARGS                                     
         MVI   PRNOCLI,0                                                        
*                                                                               
         MVI   NBQINIT,0           DO ALL VALIDATIONS EVERY TIME                
         CLI   OFFLINE,C'Y'        ALLOW ALL AGY OFFLINE                        
         BNE   AG4                                                              
         CLC   AGYCLI(3),=C'***'      CK FOR SPECIAL AGY=ALL                    
         BNE   AG2                                                              
         MVI   DDSRUN,1                                                         
         B     DDSOPT                                                           
AG2      CLI   AGYCLI,C'+'                                                      
         BNE   AG4                                                              
DDSOPT   CLI   AGYCLI+1,C'N'       IF NO CLI DETAILS                            
         BNE   DDSO2                                                            
         MVI   PRNOCLI,1           SET NO CLI FLAG                              
DDSO2    CLI   AGYCLI+2,C'M'       IF MARGA OPTION                              
         BNE   DDSO4                                                            
         MVI   PRMARGA,C'M'                                                     
         MVI   MBILCOL,3           NOTE, DEPENDANT ON FORMAT                    
DDSO4    XC    NBSELAGY,NBSELAGY   RESET AGY TO ALL                             
         XC    NBEFFAGY,NBEFFAGY                                                
         MVC   AGYCLI(3),=C'ALL'   FUDGE CLIENT BACK                            
*                                                                               
AG4      NETGO NVAGY,DMCB,HOLDAGNM    GET 1ST AGYNAME IN HOLDAGNM               
*                                    NEEDED IF ALREADY VALIDATED BITS           
*                                    NOT USED                                   
         MVI   NBDATA,C'P'         WILL WANT PACKAGE RECORD FIRST               
         L     R2,ANETWS1          USE 1ST W/S AREA TO PASS CLIENT              
         ST    R2,NBACLI           I/O AREA TO SAVE CLIENT RECORD               
*                                    TO PASS TO PRINT MODULE                    
*                                                                               
         L     R5,NBAIO            GET AGENCY BILLING PCTG                      
         USING AGYHDR,R5                                                        
         MVC   HOLDAGBP(2),AGYPROF+2                                            
         DROP  R5                                                               
*                                                                               
         LA    R2,AGYCLIH          CLIENT (REQUIRED. ALL ALLOWED)               
         NETGO NVCLIALL,DMCB,AGYCLIN      AND FILL IN UNLCLIN.                  
*                                  (RETURNS CLIENT RECORD IN ANETWS1)           
         OI    AGYCLINH+6,X'80'    TRANSMIT CLIN                                
*                                                                               
         MVI   FTERMFLG,1          OPTIONAL                                     
**********************************************                                  
         LA    R2,WORK            TO FILTER STEWARD ESTIMATES                   
         XC    WORK,WORK                                                        
         MVC   WORK+8(3),=C'POL'                                                
         MVI   WORK,11                                                          
         MVI   WORK+5,3                                                         
         NETGO NVPRDALL,DMCB,0                                                  
         LA    R2,WORK                                                          
         XC    WORK,WORK                                                        
         MVC   WORK+8(3),=C'ALL'                                                
         MVI   WORK,11                                                          
         MVI   WORK+5,3                                                         
         NETGO NVESTRNG,DMCB,0                                                  
****************************************************                            
VALNET   LA    R2,AGYNETH          NETWORK (. 'ALL' ALLOWED)                    
         NETGO NVNETALL,DMCB                                                    
         MVI   FTERMFLG,0          RESET TO REQUIRED                            
*                                                                               
         LA    R2,AGYSTRTH         START DATE                                   
         NETGO NVSTRDAT,DMCB                                                    
*                                                                               
DT2      LA    R2,AGYENDH          END DATE                                     
         NETGO NVENDDAT,DMCB                                                    
*                                                                               
         MVI   FTERMFLG,1          FOLLOWING ARE OPTIONAL                       
         MVI   PRBEFFLG,0          DEFAULT TO NO BEFORE, AFTER                  
         MVI   PRAFTFLG,0                                                       
         LA    R2,AGYPRIH                                                       
         NETGO NVGETFLD,DMCB                                                    
         BZ    DTFORM                                                           
         CLI   FLD,C'A'                                                         
         BNE   DT8                                                              
         MVI   PRAFTFLG,1          AFTER                                        
         B     DTFORM                                                           
DT8      CLI   FLD,C'P'                                                         
         BNE   DT10                                                             
         MVI   PRBEFFLG,1          PRIOR                                        
         B     DTFORM                                                           
DT10     CLI   FLD,C'B'                                                         
         BNE   DT12                                                             
         MVI   PRBEFFLG,1          BOTH                                         
         MVI   PRAFTFLG,1                                                       
         B     DTFORM                                                           
DT12     MVI   ERROR,INVALID       BAD INPUT                                    
         B     DTERR                                                            
*                                                                               
DTFORM   LA    R4,FORMTBL          DEFAULT TO FORMAT 1                          
         LA    R2,AGYFORMH         FORMAT                                       
         NETGO NVGETFLD,DMCB                                                    
         BNZ   DT18                                                             
         LA    R4,1                DEFAULT TO FORMAT 1                          
         B     DTOFORM                                                          
*                                                                               
DT18     LTR   R4,R0               R0 IS NUMERIC VALUE,SAVE IT IN R4            
         BNZ   DT20                                                             
         MVI   ERROR,NOTNUM        NON-NUMERIC                                  
         B     DTERR                                                            
DT20     LA    R1,KMAXFORM                                                      
         CR    R4,R1                                                            
         BNH   DTOFORM                                                          
         MVI   ERROR,INVALID       INVALID NUMERIC                              
         B     DTERR                                                            
DTOFORM  BCTR  R4,0                DECREMENT TO CALC OFFSET                     
         LA    R1,2*4*MAXCOSTS+10*MAXCOSTS+12   LEN OF EACH TBL NTRY            
         STH   R1,HALF                                                          
         MH    R4,HALF             OFFSET INTO TABLE                            
         LA    R4,FORMTBL(R4)                                                   
         LA    R3,2*4*MAXCOSTS                                                  
         MOVE  (CSTMASKS,(R3)),0(R4)  MOVE COSTMASKS                            
         LA    R4,0(R3,R4)         NOW MOVE IN HEADERS                          
         LA    R3,10*MAXCOSTS      HEADERS ARE 10 BYTES                         
         MOVE  (CSTHEADS,(R3)),0(R4)                                            
         LA    R4,0(R3,R4)         PCT INFO                                     
         MVC   PCTDIVA(1),0(R4)    DIVISOR,DIVIDEND                             
         MVC   PCTDIVB(1),1(R4)                                                 
M        MVC   PCTHEAD(10),2(R4)    PCT HEAD                                    
*                                                                               
DTCOST   LA    R2,AGYCOSTH         COST EXCLUSION                               
         NETGO NVGETFLD,DMCB                                                    
         BZ    DTNET                                                            
         CLI   FLD,C'I'                                                         
         BNE   DT30                                                             
         LA    R4,CSTMASKS         EXCLUDE ALL INTEGRATION COSTS                
         LA    R5,2*MAXCOSTS                                                    
DTCLOOP  NC    0(4,R4),=AL4(FULLFF-INTMSK-BIGMSK-PIGMSK)                        
         LA    R4,4(R4)                                                         
         BCT   R5,DTCLOOP                                                       
         B     DTNET                                                            
DT30     MVI   ERROR,INVALID                                                    
         B     DTERR                                                            
*                                                                               
DTNET    MVI   NETOPT,NO           SET NET DOLLAR OPTION                        
         LA    R2,AGYNETAH                                                      
         NETGO NVGETFLD,DMCB                                                    
         BZ    DTNETX                                                           
         MVC   NETOPT,FLD                                                       
         CLI   FLD,YES                                                          
         BE    DTNETX                                                           
         CLI   FLD,NO                                                           
         BE    DTNETX                                                           
         MVI   ERROR,INVALID                                                    
         B     DTERR                                                            
DTNETX   B     DTLOCK                                                           
*                                                                               
DTLOCK   XC    NBSELPST,NBSELPST   DEFAULT TO UNLOCKED ONLY                     
         LA    R2,AGYLOKH                                                       
         NETGO NVGETFLD,DMCB                                                    
         BZ    DTPER                                                            
         MVC   NBSELPST,FLD        TELL NETIO WHAT TO DO                        
         CLI   FLD,C'L'                                                         
         BE    DTPER                                                            
         CLI   FLD,C'B'                                                         
         BE    DTPER                                                            
         MVI   ERROR,INVALID                                                    
         B     DTERR                                                            
*                                                                               
DTPER    LA    R2,AGYPERH          PERIOD TYPE                                  
         NETGO NVGETFLD,DMCB                                                    
         BNZ   *+8                                                              
         MVI   FLD,C'C'           FORCE CALENDAR FOR DDS ALLAGY REQUEST         
         CLI   FLD,C'C'            CALENDAR                                     
         BE    DT40                                                             
         CLI   FLD,C'B'            BROADCAST                                    
         BE    DT40                                                             
         CLI   FLD,C'S'                                                         
         BE    DT35                                                             
         MVI   ERROR,INVALID                                                    
         B     DTERR                                                            
DT35     MVI   ERROR,INVALID       SPECIAL PERIOD LOGIC                         
         B     DTEND                   GOES HERE                                
*DT40     MVC   NBUSER+2(1),FLD                                                 
DT40     MVC   PRDOVR,FLD                                                       
*                                                                               
         LA    R2,AGYDDSH         DDS RUN                                       
         NETGO NVGETFLD,DMCB                                                    
**       BZ    DTEND                                                            
         BZ    DTPROD                                                           
         CLI   FLD,C'M'                                                         
         BNE   DTERR                                                            
         CLI   OFFLINE,C'Y'                                                     
         BNE   DTERR                                                            
         MVC   MANONLY,FLD                                                      
*                                                                               
DTPROD   DS    0H                                                               
*                                                                               
DTEND    MVI   PRZLFLG,X'0'                                                     
         MVI   PRZCFLG,X'0'                                                     
*                                                                               
         MVC   NUMCOLS,=AL2(MAXCOSTS)                                           
*                                                                               
         LA    R2,AGYCLIH          NORMAL END OF EDIT                           
         B     XMOD                                                             
*                                                                               
DTERR    GOTO1 ERREX,DMCB          ERROR                                        
*                                                                               
XMOD     XIT1  REGS=(R2)                                                        
         SPACE 2                                                                
*                                                                               
*                                                                               
         EJECT                                                                  
         DS    0D                                                               
FULLFF   EQU   0-1                 FULL WORD OF FF'S                            
*                                                                               
***** FORMTBL - TABLE OF BIT MASKS, HEADERS AND PCT INFO FOR EACH               
*****           FORMAT                                                          
*****  MAXCOSTS IS MAX NUMBER OF COSTS PER LINE.                                
*****  THERE ARE TWO BIT MASKS PER COST (ADD AND SUBTRACT).                     
*****  EACH BIT MASK IS A FULL WORD.                                            
*****  L'(BIT MASK PORTION)= 2*4*MAXCOSTS                                       
*****  THERE IS A 10 BYTE ALPHANUMERIC HEADER FOR EACH COST                     
*****  L'(HEADERS)= 10*MAXCOSTS                                                 
*****  THERE ARE 2-BYTES WHICH DESCRIBE THE ORDINAL COST NUMBER                 
*****  FOR THE PERCENT DIVISOR AND DIVIDEND, AND A 10 BYTE HEADER.              
*****  L'(PCT INFO)=2+10                                                        
KMAXFORM EQU   7                   MAX FORMATS                                  
FORMTBL  DS    0D                  TABLE                                        
FORMAT1  DS    0C                                                               
         DC    AL4(ACTMSK+INTMSK)                  ORDERED                      
         DC    AL4(0)                                                           
         DC    AL4(PTGMSK+PIGMSK)                  CLEARED                      
         DC    AL4(0)                                                           
         DC    AL4(ACTMSK+INTMSK)                  UNCLEARED                    
         DC    AL4(PTGMSK+PIGMSK)                    (ORD-CLR)                  
         DC    AL4(BTGMSK+BIGMSK)                  BILLED                       
         DC    AL4(0)                                                           
         DC    AL4(ACTMSK+INTMSK)                  BILLABLE                     
         DC    AL4(BTGMSK+BIGMSK)                     (ORD-BILL)                
         DC    AL4(0)                              6                            
         DC    AL4(0)                                                           
         DC    AL4(0)                              7                            
         DC    AL4(0)                                                           
         DC    AL4(0)                              8                            
         DC    AL4(0)                                                           
HEADER1  DC    CL10'  ORDERED '                                                 
         DC    CL10'  CLEARED '                                                 
         DC    CL10'UNCLEARED '                                                 
         DC    CL10'  BILLED  '                                                 
         DC    CL10' BILLABLE '                                                 
         DC    CL10' '                                                          
         DC    CL10' '                                                          
         DC    CL10' '                                                          
PER1     DC    XL1'01'             PCT=CLEARED/ORDERED                          
         DC    XL1'00'                                                          
         DC    CL10'CLEARED'                                                    
FORMAT2  DS    0C                                                               
         DC    AL4(ACTMSK+INTMSK)                  ORDERED                      
         DC    AL4(0)                                                           
         DC    AL4(PTGMSK+PIGMSK)                  CLEARED                      
         DC    AL4(0)                                                           
         DC    AL4(ACTMSK+INTMSK)                  UNCLEARED                    
         DC    AL4(PTGMSK+PIGMSK)                    (ORD-CLR)                  
         DC    AL4(BTGMSK+BIGMSK)                  BILLED                       
         DC    AL4(0)                                                           
         DC    AL4(ACTMSK+INTMSK)                  BILLABLE                     
         DC    AL4(BTGMSK+BIGMSK)                     (ORD-BILL)                
         DC    AL4(0)                              6                            
         DC    AL4(0)                                                           
         DC    AL4(0)                              7                            
         DC    AL4(0)                                                           
         DC    AL4(0)                              8                            
         DC    AL4(0)                                                           
HEADER2  DC    CL10'  ORDERED '                                                 
         DC    CL10'  CLEARED '                                                 
         DC    CL10'UNCLEARED'                                                  
         DC    CL10'  BILLED  '                                                 
         DC    CL10' BILLABLE '                                                 
         DC    CL10' '                                                          
         DC    CL10' '                                                          
         DC    CL10' '                                                          
PER2     DC    XL1'03'             PCT=BILLED/ORDERED                           
         DC    XL1'00'                                                          
         DC    CL10'BILLED'                                                     
FORMAT3  DS    0C                                                               
         DC    AL4(ACTMSK)                         TIME                         
         DC    AL4(0)                                                           
         DC    AL4(INTMSK)                         INTEG                        
         DC    AL4(0)                                                           
         DC    AL4(ACTMSK+INTMSK)                  TOT ORD                      
         DC    AL4(0)                                                           
         DC    AL4(PTGMSK+PIGMSK)                  CLEARED                      
         DC    AL4(0)                                                           
         DC    AL4(ACTMSK+INTMSK)                  UNCLEARED                    
         DC    AL4(PTGMSK+PIGMSK)                   (TOT ORD-CLEAR)             
         DC    AL4(BTGMSK+BIGMSK)                  BILLED                       
         DC    AL4(0)                                                           
         DC    AL4(ACTMSK+INTMSK)                  BILLABLE                     
         DC    AL4(BTGMSK+BIGMSK)                     (TOT ORD-BILL)            
         DC    AL4(0)                              8                            
         DC    AL4(0)                                                           
HEADER3  DC    CL10'   TIME   '                                                 
         DC    CL10'   INTG   '                                                 
         DC    CL10'  TOT ORD '                                                 
         DC    CL10'  CLEARED '                                                 
         DC    CL10'UNCLEARED'                                                  
         DC    CL10'  BILLED  '                                                 
         DC    CL10' BILLABLE '                                                 
         DC    CL10' '                                                          
PER3     DC    XL1'03'             PCT=CLEARED/ORDERED                          
         DC    XL1'02'                                                          
         DC    CL10'CLEARED'                                                    
*                                                                               
*                                                                               
FORMAT4  DS    0C                                                               
         DC    AL4(ACTMSK)                         TIME                         
         DC    AL4(0)                                                           
         DC    AL4(INTMSK)                         INTEG                        
         DC    AL4(0)                                                           
         DC    AL4(ACTMSK+INTMSK)                  TOT ORD                      
         DC    AL4(0)                                                           
         DC    AL4(PTGMSK+PIGMSK)                  CLEARED                      
         DC    AL4(0)                                                           
         DC    AL4(ACTMSK+INTMSK)                  UNCLEARED                    
         DC    AL4(PTGMSK+PIGMSK)                   (TOT ORD-CLEAR)             
         DC    AL4(BTGMSK+BIGMSK)                  BILLED                       
         DC    AL4(0)                                                           
         DC    AL4(ACTMSK+INTMSK)                  BILLABLE                     
         DC    AL4(BTGMSK+BIGMSK)                     (TOT ORD-BILL)            
         DC    AL4(0)                              8                            
         DC    AL4(0)                                                           
HEADER4  DC    CL10'   TIME   '                                                 
         DC    CL10'   INTG   '                                                 
         DC    CL10'  TOT ORD '                                                 
         DC    CL10'  CLEARED '                                                 
         DC    CL10'UNCLEARED'                                                  
         DC    CL10'  BILLED  '                                                 
         DC    CL10' BILLABLE '                                                 
         DC    CL10' '                                                          
PER4     DC    XL1'05'             PCT=BILLED/ORDERED                           
         DC    XL1'02'                                                          
         DC    CL10'BILLED'                                                     
*                                                                               
*                                                                               
FORMAT5  DS    0C                                                               
         DC    AL4(ACTMSK+INTMSK)                  ACTUAL ORDERED               
         DC    AL4(0)                                                           
         DC    AL4(PTGMSK+PIGMSK)                  CLEARED                      
         DC    AL4(0)                                                           
         DC    AL4(ACTMSK+INTMSK)                  UNCLEARED                    
         DC    AL4(PTGMSK+PIGMSK)                   (ACT ORD-CLEAR)             
         DC    AL4(CASSMSK+INTMSK)                 ASSIGNED ORDERED             
         DC    AL4(0)                                                           
         DC    AL4(BTGMSK+BIGMSK)                  BILLED                       
         DC    AL4(0)                                                           
         DC    AL4(CASSMSK+INTMSK)                 BILLABLE                     
         DC    AL4(BTGMSK+BIGMSK)                     (ACT ORD-BILL)            
         DC    AL4(0)                              7                            
         DC    AL4(0)                                                           
         DC    AL4(0)                              8                            
         DC    AL4(0)                                                           
HEADER5  DC    CL10'ORD ACTUAL'                                                 
         DC    CL10'  CLEARED '                                                 
         DC    CL10'UNCLEARED '                                                 
         DC    CL10'ORD ASSIGN'                                                 
         DC    CL10'  BILLED  '                                                 
         DC    CL10' BILLABLE '                                                 
         DC    CL10' '                                                          
         DC    CL10' '                                                          
PER5     DC    XL1'01'             PCT=CLEARED/ACT ORDERED                      
         DC    XL1'00'                                                          
         DC    CL10'CLEARED'                                                    
*                                                                               
FORMAT6  DS    0C                                                               
         DC    AL4(ACTMSK+INTMSK)                  ACTUAL ORDERED               
         DC    AL4(0)                                                           
         DC    AL4(PTGMSK+PIGMSK)                  CLEARED GROSS                
         DC    AL4(0)                                                           
         DC    AL4(PTNMSK+PINMSK)                  CLEARED NET                  
         DC    AL4(0)                                                           
         DC    AL4(BTGMSK+BIGMSK)                  BILLED GROSS                 
         DC    AL4(0)                                                           
         DC    AL4(BTNMSK+BINMSK)                 BILLED NET                    
         DC    AL4(0)                                                           
         DC    AL4(0)                              6                            
         DC    AL4(0)                                                           
         DC    AL4(0)                              7                            
         DC    AL4(0)                                                           
         DC    AL4(0)                              8                            
         DC    AL4(0)                                                           
HEADER6  DC    CL10'ORD ACTUAL'                                                 
         DC    CL10'CLR GROSS '                                                 
         DC    CL10'CLR NET   '                                                 
         DC    CL10'BILL GROSS'                                                 
         DC    CL10'BILL NET  '                                                 
         DC    CL10'          '                                                 
         DC    CL10' '                                                          
         DC    CL10' '                                                          
PER6     DC    XL1'01'             PCT=CLEARGROSS/ACT ORDERED                   
         DC    XL1'00'                                                          
         DC    CL10'CLEARED'                                                    
*      FUDGE FOR DANCER                                                         
FORMAT7  DS    0C                                                               
         DC    AL4(ACTMSK+INTMSK)                  ACTUAL ORDERED               
         DC    AL4(0)                                                           
         DC    AL4(ACTMSK+INTMSK)                  SAME CALL IT UNBILL          
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(0)                              6                            
         DC    AL4(0)                                                           
         DC    AL4(0)                              7                            
         DC    AL4(0)                                                           
         DC    AL4(0)                              8                            
         DC    AL4(0)                                                           
HEADER7  DC    CL10' ORDERED  '                                                 
         DC    CL10'UNBILLABLE'                                                 
         DC    CL10'          '                                                 
         DC    CL10'          '                                                 
         DC    CL10'          '                                                 
         DC    CL10'          '                                                 
         DC    CL10' '                                                          
         DC    CL10' '                                                          
PER7     DC    XL1'01'             PCT=CLEARGROSS/ACT ORDERED                   
         DC    XL1'00'                                                          
         DC    CL10'CLEARED'                                                    
*                                                                               
         EJECT                                                                  
*                                                                               
       ++INCLUDE NEMEDFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE NEMEDFDD                                                       
*                                                                               
         EJECT                                                                  
         PRINT OFF                                                              
***  INCLUDE NETINCLS *******                                                   
       ++INCLUDE NETINCLS                                                       
         PRINT ON                                                               
       ++INCLUDE SPGENAGY                                                       
*                                                                               
WORKD    DSECT                                                                  
       ++INCLUDE NEAGSMINCL                                                     
***                                                                             
*** ARGS TO PRINT                                                               
HOLDAGNM DS    CL33                                                             
PRNOCLI  DS    CL1                 FLAG SET IF NO CLI DETAILS                   
*                                    SHOULD BE PRINTED                          
HOLDAGBP DS    H                                                                
PRMARGA  DS    CL1                 FLAG SET IF MARGA DETAILS PRINTED            
MBILCOL  DS    CL1                 IF SET, THIS IS COL MARGA BILLS OFF          
PRDOVR   DS    CL1                 CALENDAR/BROADCAST OVERRIDE                  
MANONLY  DS    CL1                 SPECIAL ONLY MANUAL BILLS RUN                
STWRDLST DS    CL256                                                            
*                                                                               
YES      EQU   C'Y'                                                             
NO       EQU   C'N'                                                             
*                                                                               
DDSRUN   DS    CL1                                                              
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'005NEMED0D   02/18/04'                                      
         END                                                                    
