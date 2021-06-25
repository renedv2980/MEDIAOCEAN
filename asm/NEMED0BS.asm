*          DATA SET NEMED0BS   AT LEVEL 021 AS OF 05/01/02                      
*PHASE T31E0BA                                                                  
*                                                                               
         TITLE 'T31EOB - EDIT FOR NETWORK FLEX DEMO REPORT'                     
*****************************************************************               
* T31E0B (NEMED0B)                                                              
*  EDITS SCREEN FOR FLEX DEMO REPORT                                            
* GLOBALS - R2 - POINTS TO CURRENT FIELD ON SCREEN                              
*                                                                               
*  OUTPUTS - ADRIVE - A(DRIVE TABLE)                                            
*            AINTERN -A(START OF INTERNAL RECORD AREA)                          
*  CALLS TO -                                                                   
*   NVVALID - VALIDATION ROUTINES.                                              
******************************************************************              
*                                                                               
         PRINT NOGEN                                                            
T31E0B   CSECT                                                                  
         NMOD1 0,**ACFD**,RR=R2                                                 
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING T31EFFD,RA                                                       
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASYSD            NETWORK SYSTEM DSECT                         
         USING NETSYSD,R9                                                       
         L     R7,ANETWS2          ARGS TO PRINT                                
         USING OVERD,R7                                                         
         ST    R2,RELO                                                          
*                                                                               
*                                                                               
         EJECT                                                                  
*************  INITIALIZE NETBLOCK*************                                 
*                             ASSUMES NETBLOCK IS ALREADY INITIALIZED           
*                             DONE BY CALL TO NVAGY OR NVAGYOUT                 
*                                                                               
         MVI   NBQINIT,0           DO ALL VALIDATIONS EVERY TIME                
*                                    NOT USED                                   
         MVI   NBDATA,C'P'         WILL WANT PACKAGE RECORD FIRST               
         L     R2,ANETWS1          USE 1ST W/S AREA TO PASS CLIENT              
         ST    R2,NBACLI           I/O AREA TO SAVE CLIENT RECORD               
*                                    TO PASS TO PRINT MODULE                    
         LA    R2,NDDEMBLK                                                      
         ST    R2,NBADEM                                                        
****                                                                            
*                                                                               
*                                                                               
         LA    R2,SPLCLIH          CLIENT (REQUIRED. ALL ALLOWED)               
         NETGO NVCLIALL,DMCB,CLINAME      AND FILL IN UNLCLIN.                  
*                                  (RETURNS CLIENT RECORD IN ANETWS1)           
*                                                                               
         LA    R2,SPLPROH          PRODUCT. REQUIRED.                           
         NETGO NVPRD,DMCB,PRDNAME                                               
*                                                                               
         MVI   FTERMFLG,1          FOLLOWING ARE OPTIONAL                       
         LA    R2,SPLESTH          ESTIMATE. OPTIONAL.                          
         NETGO NVEST,DMCB,ESTNAME,NDDEMBLK                                      
*                                                                               
         LA    R2,SPLNETH          NETWORK (. 'ALL' ALLOWED)                    
         NETGO NVNETALL,DMCB                                                    
*                                                                               
         LA    R2,SPLDPTH          DAYPART. OPTIONAL                            
         NETGO NVDPT,DMCB                                                       
*                                                                               
         LA    R2,SPLPAKH          PACKAGE. OPTIONAL.                           
         NETGO NVPAK,DMCB,PAKNAME  OPTIONAL.                                    
*                                                                               
         LA    R2,SPLSTRTH         START DATE                                   
         NETGO NVSTRDAT,DMCB                                                    
*                                                                               
         LA    R2,SPLENDH          END DATE                                     
         NETGO NVENDDAT,DMCB                                                    
*                                                                               
         LA    R2,SPLFLAVH         FLAVOR (E,A,P)                               
         NETGO NVGETFLD,DMCB                                                    
         BZ    ACTFLAV                                                          
         CLI   FLD,C'A'                                                         
         BE    ACTFLAV                                                          
         CLI   FLD,C'E'                                                         
         BE    ESTFLAV                                                          
         CLI   FLD,C'P'                                                         
         BE    POSFLAV                                                          
         MVI   ERROR,INVALID                                                    
         B     DTERR                                                            
ACTFLAV  MVI   NBACTOPT,C'Y'       GET ACTUAL DEMOS                             
         B     DTSOFT                                                           
ESTFLAV  MVI   NBESTOPT,C'Y'       GET ESTIMATED DEMOS                          
         B     DTSOFT                                                           
POSFLAV  MVI   NBACTOPT,C'Y'       GET ESTIMATED AND ACTUAL DEMOS               
         MVI   NBESTOPT,C'Y'                                                    
*                                                                               
DTSOFT   LA    R2,SPLSOFTH         SOFT DESCRIPTION                             
         NETGO NVGETFLD,DMCB                                                    
         BZ    DTFORM                                                           
*                                                                               
         GOTO1 SCANNER,DMCB,(R2),(12,BLOCK)                                     
         LA    R3,BLOCK            INTERPRET BLOCK                              
         ZIC   R4,DMCB+4                                                        
         LTR   R4,R4               IF NONE THEN USE FORMAT FIELD                
         BZ    DTFORM                                                           
         BAS   RE,FILLDTAB         FILL DATA TABLE TO PASS                      
         B     DTOPTS              IGNORE FORMAT FIELD                          
*                                                                               
DTFORM   LA    R2,SPLFORMH         FORMAT                                       
         NETGO NVGETFLD,DMCB                                                    
         BNZ   DT18                                                             
         LA    R4,1                DEFAULT TO FORMAT 1                          
         B     DT20                                                             
*                                                                               
DT18     LTR   R4,R0               R0 IS NUMERIC VALUE,SAVE IT IN R4            
         BNZ   DT20                                                             
         MVI   ERROR,NOTNUM        NON-NUMERIC                                  
         B     DTERR                                                            
DT20     EQU   *                                                                
         BCTR  R4,0                DECREMENT R4 FOR OFFSET                      
         SLL   R4,2                MULT BY 4                                    
         L     R3,RIGHTSID(R4)     GET LIST OF FIELDS                           
         A     R3,RELO                                                          
         GOTO1 SCANNER,DMCB,(C'C',(R3)),(12,BLOCK)                              
         LA    R3,BLOCK            INTERPRET BLOCK                              
         ZIC   R4,DMCB+4                                                        
         LTR   R4,R4               IF NONE THEN USE FORMAT FIELD                
         BZ    DTERR1                                                           
         BAS   RE,FILLDTAB         FILL DATA TABLE TO PASS                      
         B     DTOPTS                                                           
DTERR1   DC    H'0'                                                             
*                                                                               
*                                                                               
*                                                                               
DTOPTS   EQU   *                                                                
*                                                                               
DTDEMO   LA    R2,SPLDEMOH                                                      
         NETGO NVDEM,DMCB,DBLOCK,NDDEMBLK                                       
*                                                                               
DTSEQ    MVI   NBSEQ,C'D'          DEFAULT SEQUENCE                             
         LA    R2,SPLSEQH                                                       
         NETGO NVGETFLD,DMCB                                                    
         BZ    DT60                                                             
         MVC   NBSEQ(1),FLD                                                     
*                                                                               
DT60     CLI   NBSEQ,C'X'                                                       
         BE    DT6X                                                             
         CLI   NBSEQ,C'D'                                                       
         BE    DT6D                                                             
         CLI   NBSEQ,C'P'                                                       
         BE    DT6P                                                             
         CLI   NBSEQ,C'N'                                                       
         BE    DT6N                                                             
         CLI   NBSEQ,C'Q'                                                       
         BE    DT6Q                                                             
         DC    H'0'                                                             
*                                                                               
DT6D     EQU   *                                                                
*****    MVI   DATSUBT,C'Y'                                                     
****     MVI   TOTSUBT,C'Y'                                                     
         B     DT65                                                             
DT6N     EQU   *                                                                
DT6X     EQU   *                                                                
*****    MVI   NETSUBT,C'Y'                                                     
****     MVI   DATSUBT,C'Y'                                                     
****     MVI   TOTSUBT,C'Y'                                                     
         B     DT65                                                             
*                                                                               
DT6P     EQU   *                                                                
DT6Q     EQU   *                                                                
*****    MVI   NETSUBT,C'Y'                                                     
******   MVI   TOTSUBT,C'Y'                                                     
         B     DT65                                                             
*                                                                               
DT65     MVI   NBSELUOP,C'A'       DEFAULT SCHEDULE                             
         LA    R2,SPLSKEDH                                                      
         NETGO NVGETFLD,DMCB                                                    
         BZ    DT70                                                             
         MVC   NBSELUOP(1),FLD                                                  
         CLI   NBSELUOP,C'B'       FOR BOTH                                     
         BNE   DT70                                                             
         MVI   NBSELUOP,0          LEAVE BLANK                                  
*                                                                               
DT70     LA    R2,SPLUNITH                                                      
         NETGO NVGETFLD,DMCB                                                    
         BZ    DT80                                                             
         CLI   FLD,C'N'            IF N USE PROG TOTS                           
         BNE   DT80                                                             
*****    MVI   PRGSUBT,C'Y'                                                     
*****    MVI   UNPRINF,C'N'                                                     
         CLI   NBSEQ,C'D'                                                       
         BNE   DT80                                                             
*****    MVI   PRGSUBT,0           FOR D DONT PRINT PROG TOTS                   
DT80     EQU   *                                                                
         B     XMOD                                                             
*                                                                               
DTERR    GOTO1 ERREX,DMCB          ERROR                                        
*                                                                               
XMOD     XIT1  REGS=(R2)                                                        
         SPACE 2                                                                
         EJECT                                                                  
**********************************                                              
* FILLDTAB                                                                      
*  INPUTS - R4 - # OF ENTRIES IN BLOCK                                          
*           BLOCK - SCANNERIZED SOFT HEADINGS                                   
*  INTERNALS - R5 - A(CURRENT ENTRY IN LIST TO PASS)                            
*              R6 - A(CURRENT ENTRY IN SOFTTABLE)                               
*              R3 - A(CURRENT ENTRY IN BLOCK)                                   
**********************************                                              
FILLDTAB NTR1                                                                   
         LA    R3,BLOCK                                                         
         LA    R5,RESTWS           SET UP A(LIST TO PASS)                       
         ST    R5,ADRIVE1                                                       
         USING RWINBLOK,R5                                                      
*                                                                               
         LR    R1,R4                                                            
         LA    R1,1(R1)                                                         
         STH   R1,HALF                                                          
*                                                                               
FORM4    ZIC   R1,0(R3)                                                         
         BCTR  R1,0                                                             
         LA    R6,SOFTTBL                                                       
         USING SOFTD,R6                                                         
         SPACE 1                                                                
FORM6    EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R3),0(R6)                                                   
         BE    FORM8                                                            
         LA    R6,SFNTRYLN(R6)                                                  
         CLI   0(R6),X'FF'                                                      
         BE    FORMEX                                                           
         B     FORM6                                                            
         SPACE 1                                                                
FORM8    XC    RWINBLOK(RWBLKLEN),RWINBLOK    INITIALIZE                        
         MVC   RWHEADER(12),SPACES                                              
         MVC   RWHEADER(8),SFALPH      HEADING                                  
         LA    R1,RWHEADER                                                      
         ST    R1,RWAHEDIN                                                      
         MVC   RWINTYPE,SFTYP                                                   
         MVC   RWOUTLEN,SFINLEN                                                 
         MVC   RWPRLEN,SFPRLEN                                                  
         MVC   RWOPT2(1),SFFEND       HEADER CONTROL                            
******   MVC   RWFILTER,SFFILTER                                                
         CLI   NBESTOPT,C'Y'                                                    
         BNE   FORM10                                                           
         MVC   RWOPT1,SFEOPT1      FOR EST OR POST, USE EST FIRST               
         B     FORM12                                                           
FORM10   MVC   RWOPT1,SFAOPT1      ELSE USE ACT DEMOS                           
         SPACE 1                                                                
FORM12   LA    R5,RWBLKLEN(R5)                                                  
         CLI   SFFEND,0            NO MORE                                      
         BE    FORM14                                                           
         LA    R6,SFNTRYLN(R6)    FILL NEXT OF GROUP                            
         B     FORM8                                                            
FORM14   LA    R3,32(R3)                                                        
         BCT   R4,FORM4                                                         
         XC    0(RWBLKLEN,R5),0(R5)    END OF TABLE                             
         LA    R5,RWBLKLEN(R5)                                                  
         ST    R5,AINTERN          PUT INTERNAL RECS AFTER HEADINGS             
         XIT1                                                                   
         SPACE 1                                                                
FORMEX   MVI   ERROR,INVALID                                                    
         B     DTERR                                                            
         EJECT                                                                  
*                                                                               
*                                                                               
         EJECT                                                                  
*                                                                               
         DS    0D                                                               
*                                                                               
RIGHTSID DS    0D                                                               
         DC    AL4(RIGHT1)         FORMAT=1                                     
         DC    AL4(RIGHT2)         FORMAT=2                                     
         DC    AL4(RIGHT3)         FORMAT=3                                     
         DC    AL4(RIGHT4)         FORMAT=4                                     
         DC    AL4(RIGHT5)         FORMAT=5                                     
         DC    AL4(RIGHT6)         FORMAT=6                                     
         DC    AL4(RIGHT7)         FORMAT=7                                     
         DC    AL4(RIGHT8)         FORMAT=8                                     
         DC    AL4(RIGHT9)         FORMAT=9                                     
         DC    AL4(RIGHT10)        FORMAT=10                                    
         DC    AL4(RIGHT11)        FORMAT=11                                    
*                                                                               
RIGHT1   DC    C'DATE,PROG,NETW,DAY,TIME,PROD,GROSS,INT,TOT'                    
         DC    CL50' '                                                          
RIGHT2   DC    C'DATE,PROG,NETW,DAY,TIME,PROD,GRO,INT,TOT,EST,PAK'              
         DC    CL50' '                                                          
RIGHT3   DC    C'DATE,PROG,NETW,DAY,TIME,PROD,ASS,ACT,DIF,INT'                  
         DC    CL50' '                                                          
RIGHT4   DC    C'DATE,PROG,NETW,DAY,TIME,PROD,ASS+,ACT+,DIFF,INT'               
         DC    CL50' '                                                          
RIGHT5   DC    C'DATE,PROG,NETW,DAY,TIME,PROD,GROSS'                            
         DC    CL50' '                                                          
RIGHT6   DC    C'DATE,PROG,NETW,DAY,TIME,PROD,GROSS,NET'                        
         DC    CL50' '                                                          
RIGHT7   DC    C'DATE,PROG,NETW,DAY,TIME,PROD,GROSS,INT,TOT'                    
         DC    CL50' '                                                          
RIGHT8   DC    C'DATE,PROG,NETW,DAY,TIME,PROD,GROSS,INT,TOT,TOTN'               
         DC    CL50' '                                                          
RIGHT9   DC    C'DATE,PROG,NETW,DAY,TIME,PROD,GROSS,COMM,NET'                   
         DC    CL50' '                                                          
RIGHT10  DC    C'DATE,PROG,NETW,DAY,TIME,PROD,GROSS,INT,TOT,TOTN,EST,'          
         DC    C'PAK'                                                           
         DC    CL50' '                                                          
RIGHT11  DC    C'DATE,PROG,NETW,DAY,TIME,PROD,GROSS,INT'                        
         DC    CL50' '                                                          
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*                                                                               
SOFTTBL  DS    0D                                                               
*                                                                               
*                                                                               
         DC    C'EST     ',AL1(NRIEST1),AL4(0),AL4(0)                           
         DC    AL1(1),AL1(3),AL1(0),AL1(0)                                      
*                                                                               
         DC    C'PAK     ',AL1(NRIPAK1),AL4(0),AL4(0)                           
         DC    AL1(1),AL1(3),AL1(0),AL1(0)                                      
*                                                                               
         DC    C'DATE    ',AL1(NRIDATE),AL4(0),AL4(0)                           
         DC    AL1(3),AL1(5),AL1(0),AL1(0)                                      
*                                                                               
         DC    C'PROGRAM ',AL1(NRIPRGNM),AL4(0),AL4(0)                          
         DC    AL1(16),AL1(16),AL1(0),AL1(0)                                    
*                                                                               
         DC    C'PCODE   ',AL1(NRIPRGCD),AL4(0),AL4(0)                          
         DC    AL1(6),AL1(6),AL1(0),AL1(0)                                      
*                                                                               
         DC    C'NETW    ',AL1(NRINET),AL4(0),AL4(0)                            
         DC    AL1(4),AL1(4),AL1(0),AL1(0)                                      
*                                                                               
         DC    C'DAY     ',AL1(NRIDAY3),AL4(0),AL4(0)                           
         DC    AL1(3),AL1(3),AL1(0),AL1(0)                                      
*                                                                               
         DC    C'TIME    ',AL1(NRITIME),AL4(0),AL4(0)                           
         DC    AL1(4),AL1(11),AL1(0),AL1(0)                                     
*                                                                               
         DC    C'PRODUCT ',AL1(NRIPRD3),AL4(0),AL4(0)                           
         DC    AL1(7),AL1(7),AL1(0),AL1(0)                                      
*                                                                               
         DC    C'CLI     ',AL1(NRICLT3),AL4(0),AL4(0)                           
         DC    AL1(3),AL1(3),AL1(0),AL1(0)                                      
*                                                                               
         DC    C'UNITS   ',AL1(NRIUNIT),AL4(0),AL4(0)                           
         DC    AL1(4),AL1(5),AL1(0),AL1(0)                                      
*                                                                               
         DC    C'D1      ',AL1(NRIDEM),CL1'E',AL1(1),AL2(0)                     
         DC    CL1'A',AL1(1),AL2(0),AL1(4),AL1(8),CL1'P',AL1(0)                 
*                                                                               
         DC    C'D2      ',AL1(NRIDEM),CL1'E',AL1(2),AL2(0)                     
         DC    CL1'A',AL1(2),AL2(0),AL1(4),AL1(8),CL1'P',AL1(0)                 
*                                                                               
         DC    C'D3      ',AL1(NRIDEM),CL1'E',AL1(3),AL2(0)                     
         DC    CL1'A',AL1(3),AL2(0),AL1(4),AL1(8),CL1'P',AL1(0)                 
*                                                                               
         DC    C'D4      ',AL1(NRIDEM),CL1'E',AL1(4),AL2(0)                     
         DC    CL1'A',AL1(4),AL2(0),AL1(4),AL1(8),CL1'P',AL1(0)                 
*                                                                               
         DC    C'D5      ',AL1(NRIDEM),CL1'E',AL1(5),AL2(0)                     
         DC    CL1'A',AL1(5),AL2(0),AL1(4),AL1(8),CL1'P',AL1(0)                 
*                                                                               
         DC    C'AFFTIM  ',AL1(NRIAFFT),AL4(0),AL4(0)                           
         DC    AL1(2),AL1(5),AL1(0),AL1(0)                                      
*                                                                               
         DC    C'GAP2    ',AL1(NRIGAP),AL4(0),AL4(0)                            
         DC    AL1(0),AL1(0),AL1(0),AL1(0)                                      
*                                                                               
         DC    C'GAP3    ',AL1(NRIGAP),AL4(0),AL4(0)                            
         DC    AL1(0),AL1(1),AL1(0),AL1(0)                                      
*                                                                               
         DC    C'GAP4    ',AL1(NRIGAP),AL4(0),AL4(0)                            
         DC    AL1(0),AL1(2),AL1(0),AL1(0)                                      
*                                                                               
         DC    C'GAP5    ',AL1(NRIGAP),AL4(0),AL4(0)                            
         DC    AL1(0),AL1(3),AL1(0),AL1(0)                                      
*                                                                               
         DC    C'GAP6    ',AL1(NRIGAP),AL4(0),AL4(0)                            
         DC    AL1(0),AL1(4),AL1(0),AL1(0)                                      
*                                                                               
         DC    C'GAP7    ',AL1(NRIGAP),AL4(0),AL4(0)                            
         DC    AL1(0),AL1(5),AL1(0),AL1(0)                                      
*                                                                               
         DC    C'GAP8    ',AL1(NRIGAP),AL4(0),AL4(0)                            
         DC    AL1(0),AL1(6),AL1(0),AL1(0)                                      
*                                                                               
         DC    C'GAP9    ',AL1(NRIGAP),AL4(0),AL4(0)                            
         DC    AL1(0),AL1(7),AL1(0),AL1(0)                                      
*                                                                               
         DC    C'ASSIGNED',AL1(NRICOST),AL4(ASSMSK),AL4(0)                      
         DC    AL1(9),AL1(10),AL1(0),AL1(0)                                     
*                                                                               
         DC    C'ACTUAL  ',AL1(NRICOST),AL4(ACTMSK),AL4(0)                      
         DC    AL1(9),AL1(10),AL1(0),AL1(0)                                     
*                                                                               
         DC    C'ASS+    ',AL1(NRICOST),AL4(ASSMSK+INTMSK),AL4(0)               
         DC    AL1(9),AL1(10),AL1(0),AL1(0)                                     
*                                                                               
         DC    C'ACT+    ',AL1(NRICOST),AL4(ACTMSK+INTMSK),AL4(0)               
         DC    AL1(9),AL1(10),AL1(0),AL1(0)                                     
*                                                                               
*                                                                               
         DC    C'INT     ',AL1(NRICOST),AL4(INTMSK),AL4(0)                      
         DC    AL1(9),AL1(10),AL1(0),AL1(0)                                     
*                                                                               
         DC    C'TOTN    ',AL1(NRICOST),AL4(NTACTMSK+NTINTMSK),AL4(0)           
         DC    AL1(9),AL1(10),AL1(0),AL1(0)                                     
*                                                                               
         SPACE 1                                                                
         DC    X'FF'                                                            
         EJECT                                                                  
*                                                                               
*                                                                               
         SPACE 3                                                                
*                                                                               
SOFTD    DSECT                                                                  
SFALPH   DS    CL8                 ALPHA NAME                                   
SFTYP    DS    CL1                 INPUT TYPE                                   
SFEOPT1  DS    CL4                 EST OPT 1                                    
SFAOPT1  DS    CL4                 ACT OPT 2                                    
SFINLEN  DS    CL1                 INTERNAL RECORD LENGTH                       
SFPRLEN  DS    CL1                 PRINT LENGTH                                 
SFFILTER DS    CL1                 FILTER  A=AUDIT TRAIL(ACCTG)                 
*                                          P=REPEAT ON POST                     
SFFEND   DS    CL1                 0=LAST OF GROUP                              
*                                  1=GROUP, USE ---- HEAD                       
*                                  2=GROUP, USE DEMO HEAD                       
SFNTRYLN EQU   *-SOFTD                                                          
*                                                                               
*                                                                               
       ++INCLUDE NEMEDFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE NEMEDFBD                                                       
*                                                                               
         EJECT                                                                  
         PRINT OFF                                                              
***  INCLUDE NETINCLS *******                                                   
       ++INCLUDE NETINCLS                                                       
         PRINT ON                                                               
       ++INCLUDE NEGENCOSTS                                                     
*                                                                               
         EJECT                                                                  
       ++INCLUDE NERWINCLS                                                      
*                                                                               
         EJECT                                                                  
       ++INCLUDE NERWEQUS                                                       
*                                                                               
         EJECT                                                                  
OVERD    DSECT                                                                  
       ++INCLUDE NERWBLOCK                                                      
       ++INCLUDE NETDEMOD                                                       
       ++INCLUDE DEDBLOCK                                                       
       ++INCLUDE NEACCTBLK                                                      
*                                                                               
RELO     DS    A                                                                
*                                                                               
RESTWS   EQU   *                   DRIVE TABLE HERE                             
***                                                                             
*                                                                               
YES      EQU   C'Y'                                                             
NO       EQU   C'N'                                                             
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'021NEMED0BS  05/01/02'                                      
         END                                                                    
