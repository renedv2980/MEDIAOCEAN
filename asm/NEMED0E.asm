*          DATA SET NEMED0E    AT LEVEL 017 AS OF 08/10/00                      
*PHASE T31E0EA                                                                  
*                                                                               
         TITLE 'T31E0E - EDIT FOR BILLING PREP'                                 
*****************************************************************               
* T31E0E (NEMED0E)                                                              
*   BILLING PREP - MUCH BORROWED FROM NEMED0D-AGENCY SUMMARY.                   
*                   SAME INCLUDE FILE NEAGSMINCL USED FOR BOTH.                 
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
******************************************************************              
*                                                                               
         PRINT NOGEN                                                            
T31E0E   CSECT                                                                  
         NMOD1 0,**BPED**                                                       
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING T31EFFD,RA                                                       
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASYSD            NETWORK SYSTEM DSECT                         
         USING NETSYSD,R9                                                       
         L     R6,ANETWS2          ARGS TO PRINT                                
         A     R6,=F'500'                                                       
         USING WORKD,R6                                                         
         EJECT                                                                  
*************  INITIALIZE NETBLOCK*************                                 
*                             ASSUMES NETBLOCK IS ALREADY INITIALIZED           
*                             DONE BY CALL TO NVAGY OR NVAGYOUT                 
*                                                                               
         MVI   NBQINIT,0           DO ALL VALIDATIONS EVERY TIME                
*                                    NEEDED IF ALREADY VALIDATED BITS           
*                                    NOT USED                                   
         MVI   NBDATA,C'P'         WILL WANT PACKAGE RECORD FIRST               
         L     R2,ANETWS1          USE 1ST W/S AREA TO PASS CLIENT              
         ST    R2,NBACLI           I/O AREA TO SAVE CLIENT RECORD               
*                                    TO PASS TO PRINT MODULE                    
*                                                                               
         LA    R2,SPLCLIH          CLIENT (REQUIRED. ALL ALLOWED)               
         NETGO NVCLIALL,DMCB,SPLCLIN      AND FILL IN UNLCLIN.                  
*                                  (RETURNS CLIENT RECORD IN ANETWS1)           
         OI    SPLCLINH+6,X'80'    TRANSMIT CLIN                                
*                                                                               
         MVI   FTERMFLG,1          OPTIONAL                                     
         LA    R2,SPLPROH          PRODUCT                                      
         NETGO NVPRDALL,DMCB,SPLPRON                                            
         OI    SPLPROH+6,X'80'     XMIT PRODUCT                                 
*                                                                               
         LA    R2,SPLESTH          ESTIMATE                                     
         NETGO NVESTRNG,DMCB,SPLESTN                                            
         OI    SPLESTH+6,X'80'     XMIT ESTIMATE                                
*                                                                               
         MVI   OPTALLOC,C'Y'       DEFAULT TO PRINT ALLOCATED                   
         MVI   OPTUNAL,C'Y'         AND UNALOCATED UNITS                        
         LA    R2,SPLPOPTH         PROD OPTION                                  
         NETGO NVGETFLD,DMCB                                                    
         BZ    DTPER                                                            
         CLI   FLD,C'B'            IF BOTH, SAME AS DEFAULT, GO ON              
         BE    DTPER                                                            
         CLI   FLD,C'A'            ALLOCATED ONLY                               
         BNE   CKO2                                                             
         MVI   OPTUNAL,0                                                        
         B     DTPER                                                            
CKO2     CLI   FLD,C'U'            UNALLOCATED ONLY                             
         BNE   CKO4                                                             
         MVI   OPTALLOC,0                                                       
         B     DTPER                                                            
CKO4     MVI   ERROR,INVALID                                                    
         B     DTERR                                                            
*                                                                               
DTPER    LA    R2,SPLPERH          PERIOD TYPE                                  
         NETGO NVGETFLD,DMCB                                                    
         BZ    DTSTRT                                                           
         CLI   FLD,C'C'            CALENDAR                                     
         BE    DT40                                                             
         CLI   FLD,C'B'            BROADCAST                                    
         BE    DT40                                                             
         CLI   FLD,C'S'                                                         
         BE    DT35                                                             
         MVI   ERROR,INVALID                                                    
         B     DTERR                                                            
DT35     MVI   ERROR,INVALID       SPECIAL PERIOD LOGIC                         
         B     DTSTRT                  GOES HERE                                
DT40     MVC   NBUSER+2(1),FLD                                                  
*                                                                               
DTSTRT   LA    R2,SPLSTRTH         START DATE                                   
         NETGO NVSTRDAT,DMCB                                                    
*                                                                               
         LA    R2,SPLENDH          END DATE                                     
         NETGO NVENDDAT,DMCB                                                    
*                                                                               
         MVI   FTERMFLG,1          FOLLOWING ARE OPTIONAL                       
         LA    R2,SPLEOPH                                                       
         NETGO NVGETFLD,DMCB                                                    
         BZ    DOASS                                                            
         CLI   FLD,C'A'            ACTIVE                                       
         BE    DOASS                                                            
         CLI   FLD,C'I'            INACTIVE                                     
         BE    DOASS                                                            
         MVI   ERROR,INVALID       BAD INPUT                                    
         B     DTERR                                                            
*                                                                               
DOASS    LA    R2,SPLASSH          ASSIGNED COSTS?                              
         NETGO NVGETFLD,DMCB                                                    
         BZ    DOREPOPT                                                         
         CLI   FLD,C'Y'            USE ASSIGNED                                 
         BE    DOREPOPT                                                         
         CLI   FLD,C'N'            DEFAULT. ALLOW IT                            
         BE    DOREPOPT                                                         
         MVI   ERROR,INVALID       BAD INPUT                                    
         B     DTERR                                                            
*                                                                               
DOREPOPT LA    R2,SPLROPH          FORMAT                                       
         MVI   FRMTYPE,0                                                        
         NETGO NVGETFLD,DMCB                                                    
         BNZ   DT18                                                             
         LA    R4,1                DEFAULT TO FORMAT 1                          
         B     DTOFORM                                                          
*                                                                               
DT18     LA    R4,2                FORMAT 2 - CLEARED ONLY                      
         CLI   FLD,C'C'                                                         
         BE    DTOFORM                                                          
         LA    R4,3                FORMAT 3 - BILLED ONLY                       
         MVI   FRMTYPE,3                    INCLUDES MANUAL BILLING             
         CLI   FLD,C'B'                                                         
         BE    DTOFORM                                                          
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
*                                                                               
DTEND    MVI   PRZLFLG,X'0'                                                     
         MVI   PRZCFLG,X'0'                                                     
         MVI   PRBEFFLG,0          DEFAULT TO NO BEFORE, AFTER                  
         MVI   PRAFTFLG,0                                                       
*                                                                               
         MVC   NUMCOLS,=AL2(MAXCOSTS)                                           
*                                                                               
         LA    R2,SPLNETH                                                       
         NETGO NVNETALL,DMCB                                                    
*                                                                               
         LA    R2,SPLCLIH          NORMAL END OF EDIT                           
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
*****  NOT USED FOR BILLING PREP                                                
*****  L'(PCT INFO)=2+10                                                        
KMAXFORM EQU   3                   MAX FORMATS                                  
FORMTBL  DS    0D                  TABLE                                        
FORMAT1  DS    0C                                                               
         DC    AL4(CASSMSK+INTMSK)                 ORDERED                      
         DC    AL4(0)                                                           
         DC    AL4(PTGMSK+PIGMSK)                  CLEARED                      
         DC    AL4(0)                                                           
         DC    AL4(CASSMSK+INTMSK)                 UNCLEARED                    
         DC    AL4(PTGMSK+PIGMSK)                    (ORD-CLR)                  
         DC    AL4(BTGMSK+BIGMSK+CBMSK4)         B4 BILLED                      
         DC    AL4(0)                                                           
         DC    AL4(BTGMSK+BIGMSK+CBMSK5)         B5 BILLED                      
         DC    AL4(0)                                                           
         DC    AL4(BTGMSK+BIGMSK+CBMSK6)         B6 BILLED                      
         DC    AL4(0)                                                           
         DC    AL4(BTGMSK+BIGMSK+CBMSK7)         B7 BILLED                      
         DC    AL4(0)                                                           
         DC    AL4(CASSMSK+INTMSK)                 BILLABLE                     
         DC    AL4(BTGMSK+BIGMSK)                     (ORD-BILL)                
HEADER1  DC    CL10'  ORDERED '                                                 
         DC    CL10'  CLEARED '                                                 
         DC    CL10'UNCLEARED '                                                 
         DC    CL10'B4 BILLED '                                                 
         DC    CL10'B5 BILLED '                                                 
         DC    CL10'B6 BILLED '                                                 
         DC    CL10'B7 BILLED '                                                 
         DC    CL10' BILLABLE '                                                 
PER1     DC    XL1'00'             PCT FIELDS NOT USED                          
         DC    XL1'00'                                                          
         DC    CL10'       '                                                    
FORMAT2  DS    0C                                                               
         DC    AL4(CASSMSK+INTMSK)                 ORDERED                      
         DC    AL4(0)                                                           
         DC    AL4(PTGMSK+PIGMSK)                  CLEARED                      
         DC    AL4(0)                                                           
         DC    AL4(CASSMSK+INTMSK)                 UNCLEARED                    
         DC    AL4(PTGMSK+PIGMSK)                    (ORD-CLR)                  
         DC    AL4(0)                              4                            
         DC    AL4(0)                                                           
         DC    AL4(0)                              5                            
         DC    AL4(0)                                                           
         DC    AL4(0)                              6                            
         DC    AL4(0)                                                           
         DC    AL4(0)                              7                            
         DC    AL4(0)                                                           
         DC    AL4(0)                              8                            
         DC    AL4(0)                                                           
HEADER2  DC    CL10'  ORDERED '                                                 
         DC    CL10'  CLEARED '                                                 
         DC    CL10'UNCLEARED'                                                  
         DC    CL10'          '                                                 
         DC    CL10'          '                                                 
         DC    CL10' '                                                          
         DC    CL10' '                                                          
         DC    CL10' '                                                          
PER2     DC    XL1'00'             PCT FIELDS UNUSED                            
         DC    XL1'00'                                                          
         DC    CL10'       '                                                    
FORMAT3  DS    0C                                                               
         DC    AL4(CASSMSK+INTMSK)                 ORDERED                      
         DC    AL4(0)                                                           
         DC    AL4(BTGMSK+BIGMSK+CBMSK4)         B4 BILLED                      
         DC    AL4(0)                                                           
         DC    AL4(BTGMSK+BIGMSK+CBMSK5)         B5 BILLED                      
         DC    AL4(0)                                                           
         DC    AL4(BTGMSK+BIGMSK+CBMSK6)         B6 BILLED                      
         DC    AL4(0)                                                           
         DC    AL4(BTGMSK+BIGMSK+CBMSK7)         B7 BILLED                      
         DC    AL4(0)                                                           
         DC    AL4(BTGMSK+CBMSK8)                B8 MANUAL BILLING              
         DC    AL4(0)                                                           
         DC    AL4(BTGMSK+BIGMSK)                  BILLED                       
         DC    AL4(0)                                                           
         DC    AL4(CASSMSK+INTMSK)                 BILLABLE                     
         DC    AL4(BTGMSK+BIGMSK)                     (ORD-BILL)                
HEADER3  DC    CL10'  ORDERED '                                                 
         DC    CL10'B4 BILLED '                                                 
         DC    CL10'B5 BILLED'                                                  
         DC    CL10'B6 BILLED '                                                 
         DC    CL10'B7 BILLED '                                                 
         DC    CL10'MANUAL BIL'                                                 
         DC    CL10'TOT BILLED'                                                 
         DC    CL10'BILLABLE  '                                                 
PER3     DC    XL1'00'             PCT NOT USED                                 
         DC    XL1'00'                                                          
         DC    CL10'      '                                                     
*                                                                               
         EJECT                                                                  
*                                                                               
       ++INCLUDE NEMEDFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE NEMEDFED                                                       
*                                                                               
         EJECT                                                                  
***  INCLUDE NETINCLS *******                                                   
         PRINT OFF                                                              
       ++INCLUDE NETINCLS                                                       
         PRINT ON                                                               
*                                                                               
WORKD    DSECT                                                                  
OPTALLOC DS    CL1                 Y- PRINT ALLOCATED UNITS                     
OPTUNAL  DS    CL1                 Y- PRINT UNALLOCATED UNITS                   
FRMTYPE  DS    CL1                 3=INCLUDES MANUAL BILLING                    
       ++INCLUDE NEAGSMINCL                                                     
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'017NEMED0E   08/10/00'                                      
         END                                                                    
