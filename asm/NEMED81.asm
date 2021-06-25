*          DATA SET NEMED81    AT LEVEL 112 AS OF 05/01/02                      
*PHASE T31E81A                                                                  
*                                                                               
         TITLE 'T31E81 - EDIT FOR NETWORK ACCOUNTING REPORT'                    
*****************************************************************               
* T31E81 (NEMED81)                                                              
*  EDITS SCREEN FOR ACCOUNTING REPORT                                           
* GLOBALS - R2 - POINTS TO CURRENT FIELD ON SCREEN                              
*                                                                               
*  OUTPUTS - ADRIVE - A(DRIVE TABLE)                                            
*            AINTERN -A(START OF INTERNAL RECORD AREA)                          
*  CALLS TO -                                                                   
*   NVVALID - VALIDATION ROUTINES.                                              
******************************************************************              
*                                                                               
         PRINT NOGEN                                                            
T31E81   CSECT                                                                  
         NMOD1 0,**ACED**,RR=R2                                                 
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING T31EFFD,RA                                                       
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASYSD            NETWORK SYSTEM DSECT                         
         USING NETSYSD,R9                                                       
         L     R7,ANETWS2          ARGS TO PRINT                                
         LA    R7,400(R7)                                                       
         USING ACCTD,R7                                                         
         ST    R2,RELO                                                          
*                                                                               
         LA    R2,NACCBLKD                                                      
         ST    R2,AACCBLK                                                       
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
         NETGO NVCLIALL,DMCB,SPLCLIN      AND FILL IN UNLCLIN.                  
*                                  (RETURNS CLIENT RECORD IN ANETWS1)           
*                                                                               
         LA    R2,SPLPROH          PRODUCT. REQUIRED.                           
         NETGO NVPRD,DMCB,SPLPRON                                               
*                                                                               
         MVI   FTERMFLG,1          FOLLOWING ARE OPTIONAL                       
         LA    R2,SPLESTH          ESTIMATE. OPTIONAL.                          
         NETGO NVESTRNG,DMCB,SPLESTN,NDDEMBLK                                   
*                                                                               
         LA    R2,SPLNETH          NETWORK (. 'ALL' ALLOWED)                    
         NETGO NVNETALL,DMCB                                                    
*                                                                               
         LA    R2,SPLDPTH          DAYPART. OPTIONAL                            
         NETGO NVDPT,DMCB,SPLDPTN                                               
*                                                                               
         LA    R2,SPLPAKH          PACKAGE. OPTIONAL.                           
         NETGO NVPAK,DMCB,SPLPAKN  OPTIONAL.                                    
*                                                                               
         LA    R2,SPLSTRTH         START DATE                                   
         NETGO NVSTRDAT,DMCB                                                    
*                                                                               
         LA    R2,SPLENDH          END DATE                                     
         NETGO NVENDDAT,DMCB                                                    
*                                                                               
         LA    R2,SPLFILTH         ACCTG FILTER.                                
         NETGO NVFILT,DMCB                                                      
*                                                                               
         LA    R2,SPLNOPTH         NET OPTION  .                                
         NETGO NVGETFLD,DMCB                                                    
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
         L     R3,TBLNOFIL(R4)     GET LIST OF FIELDS                           
         CLI   NBSELFLT,0          GET APPROPRIATE TABLE IF A FILTER            
         BE    DT30                                                             
         L     R3,TBLFLT1(R4)                                                   
         CLI   NBSELFLT,1                                                       
         BE    DT30                                                             
         L     R3,TBLFLT2(R4)                                                   
         CLI   NBSELFLT,2                                                       
         BE    DT30                                                             
         L     R3,TBLFLT3(R4)                                                   
         CLI   NBSELFLT,3                                                       
         BE    DT30                                                             
         L     R3,TBLFLT4(R4)                                                   
DT30     A     R3,RELO                                                          
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
         XC    NACSBDT,NACSBDT                                                  
         XC    NACSPDT,NACSPDT     CLEAR DATES                                  
         LA    R2,SPLBSTRH         BILL START                                   
         NETGO NVGETFLD,DMCB                                                    
         BZ    DTBEND                                                           
         GOTO1 DATVAL,DMCB,(0,FLD),DAT6                                         
         OC    DMCB(4),DMCB                                                     
         BZ    DTDTERR                                                          
         GOTO1 DATCON,DMCB,(0,DAT6),(2,NACSBDT)                                 
DTBEND   LA    R2,SPLBENDH         BILL END                                     
         NETGO NVGETFLD,DMCB                                                    
         BZ    DTPSTR                                                           
         GOTO1 DATVAL,DMCB,(0,FLD),DAT6                                         
         OC    DMCB(4),DMCB                                                     
         BZ    DTDTERR                                                          
         GOTO1 DATCON,DMCB,(0,DAT6),(2,NACSBDT+2)                               
DTPSTR   LA    R2,SPLPSTRH         PAY START                                    
         NETGO NVGETFLD,DMCB                                                    
         BZ    DTPEND                                                           
         GOTO1 DATVAL,DMCB,(0,FLD),DAT6                                         
         OC    DMCB(4),DMCB                                                     
         BZ    DTDTERR                                                          
         GOTO1 DATCON,DMCB,(0,DAT6),(2,NACSPDT)                                 
DTPEND   LA    R2,SPLPENDH         PAY END                                      
         NETGO NVGETFLD,DMCB                                                    
         BZ    DTSEQ                                                            
         GOTO1 DATVAL,DMCB,(0,FLD),DAT6                                         
         OC    DMCB(4),DMCB                                                     
         BZ    DTDTERR                                                          
         GOTO1 DATCON,DMCB,(0,DAT6),(2,NACSPDT+2)                               
         B     DTSEQ                                                            
*                                                                               
DTDTERR  MVI   ERROR,INVDATE                                                    
         B     DTERR                                                            
*                                                                               
*                                                                               
DTSEQ    MVI   NBSEQ,C'N'          DEFAULT SEQUENCE                             
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
         MVI   DATSUBT,C'Y'                                                     
         MVI   TOTSUBT,C'Y'                                                     
         B     DT65                                                             
DT6N     EQU   *                                                                
DT6X     MVI   NETSUBT,C'Y'                                                     
         MVI   DATSUBT,C'Y'                                                     
         MVI   TOTSUBT,C'Y'                                                     
         B     DT65                                                             
*                                                                               
DT6P     EQU   *                                                                
DT6Q     EQU   *                                                                
         MVI   NETSUBT,C'Y'                                                     
         MVI   TOTSUBT,C'Y'                                                     
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
         MVI   PRGSUBT,C'Y'                                                     
         MVI   UNPRINF,C'N'                                                     
         CLI   NBSEQ,C'D'                                                       
         BE    DT75                                                             
         CLI   NBSEQ,C'N'                                                       
         BE    DT75                                                             
         B     DT80                                                             
DT75     MVI   PRGSUBT,0           FOR D OR N DONT PRINT PROG TOTS              
DT80     EQU   *                                                                
         LA    R2,SPLAUDH                                                       
         NETGO NVGETFLD,DMCB                                                    
         BZ    DT88                                                             
         MVI   NACSMOD,C'A'        FOR AUDIT TRAIL                              
DT88     EQU   *                                                                
         MVC   NACPCT,=F'10000'    DEFAULT TO 100.00 %                          
         LA    R2,SPLPCTH                                                       
         NETGO NVGETFLD,DMCB                                                    
         BZ    DT90                                                             
         LR    R3,R1               (R1 IS LENGTH OF FIELD)                      
         GOTO1 CASHVAL,DMCB,FLD,(R3)                                            
         CLI   DMCB,X'FF'          TEST FOR ERROR                               
         BE    DTERR                                                            
         MVC   NACPCT,DMCB+4       SAVE VALUE IN ACCPCT                         
*                                                                               
DT90     EQU   *                                                                
         LA    R2,SPLCLIH          NORMAL END OF EDIT                           
         MVI   NBUSER+8,C'Y'       FORCE ACCTG REPORT TO USE ASSIGNED           
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
         ST    R5,ADRIVE                                                        
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
         MVC   RWHEADER(12),SFALPH      HEADING                                 
         LA    R1,RWHEADER                                                      
         ST    R1,RWAHEDIN                                                      
         MVC   RWINTYPE,SFTYP                                                   
         MVC   RWOUTLEN,SFINLEN                                                 
         MVC   RWPRLEN,SFPRLEN                                                  
         MVC   RWFILTER,SFFILTER                                                
         CLI   SPLNOPT,C'Y'                                                     
         BE    FORM10                                                           
         MVC   RWOPT1,SFGOPT1      USE GROSS DOLLARS                            
         MVC   RWOPT2,SFGOPT2                                                   
         B     FORM12                                                           
FORM10   MVC   RWOPT1,SFNOPT1      USE NET DOLLARS                              
         MVC   RWOPT2,SFNOPT2                                                   
         SPACE 1                                                                
FORM12   LA    R3,32(R3)                                                        
         LA    R5,RWBLKLEN(R5)                                                  
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
TBLNOFIL DS    0D                                                               
         DC    AL4(RIGHT10)         FORMAT=1                                    
         DC    AL4(RIGHT20)         FORMAT=2                                    
         DC    AL4(RIGHT30)         FORMAT=3                                    
         DC    AL4(RIGHT40)         FORMAT=4                                    
         DC    AL4(RIGHT50)         FORMAT=5                                    
         DC    AL4(RIGHT60)         FORMAT=6                                    
         DC    AL4(RIGHT70)         FORMAT=7                                    
         DC    AL4(RIGHT80)         FORMAT=8                                    
         DC    AL4(RIGHT90)         FORMAT=9                                    
         DC    AL4(RIGHT100)        FORMAT=10                                   
         DC    AL4(RIGHT110)        FORMAT=11                                   
*                                                                               
TBLFLT1  DS    0D                                                               
         DC    AL4(RIGHT11)         FORMAT=1                                    
         DC    AL4(RIGHT21)         FORMAT=2                                    
         DC    AL4(RIGHT31)         FORMAT=3                                    
         DC    AL4(RIGHT41)         FORMAT=4                                    
         DC    AL4(RIGHT51)         FORMAT=5                                    
         DC    AL4(RIGHT61)         FORMAT=6                                    
         DC    AL4(RIGHT71)         FORMAT=7                                    
         DC    AL4(RIGHT81)         FORMAT=8                                    
         DC    AL4(RIGHT91)         FORMAT=9                                    
         DC    AL4(RIGHT101)        FORMAT=10                                   
         DC    AL4(RIGHT111)        FORMAT=11                                   
*                                                                               
TBLFLT2  DS    0D                                                               
         DC    AL4(RIGHT12)         FORMAT=1                                    
         DC    AL4(RIGHT22)         FORMAT=2                                    
         DC    AL4(RIGHT32)         FORMAT=3                                    
         DC    AL4(RIGHT42)         FORMAT=4                                    
         DC    AL4(RIGHT52)         FORMAT=5                                    
         DC    AL4(RIGHT62)         FORMAT=6                                    
         DC    AL4(RIGHT72)         FORMAT=7                                    
         DC    AL4(RIGHT82)         FORMAT=8                                    
         DC    AL4(RIGHT92)         FORMAT=9                                    
         DC    AL4(RIGHT102)        FORMAT=10                                   
         DC    AL4(RIGHT112)        FORMAT=11                                   
*                                                                               
TBLFLT3  DS    0D                                                               
         DC    AL4(RIGHT13)         FORMAT=1                                    
         DC    AL4(RIGHT23)         FORMAT=2                                    
         DC    AL4(RIGHT33)         FORMAT=3                                    
         DC    AL4(RIGHT43)         FORMAT=4                                    
         DC    AL4(RIGHT53)         FORMAT=5                                    
         DC    AL4(RIGHT63)         FORMAT=6                                    
         DC    AL4(RIGHT73)         FORMAT=7                                    
         DC    AL4(RIGHT83)         FORMAT=8                                    
         DC    AL4(RIGHT93)         FORMAT=9                                    
         DC    AL4(RIGHT103)        FORMAT=10                                   
         DC    AL4(RIGHT113)        FORMAT=11                                   
*                                                                               
TBLFLT4  DS    0D                                                               
         DC    AL4(RIGHT14)         FORMAT=1                                    
         DC    AL4(RIGHT24)         FORMAT=2                                    
         DC    AL4(RIGHT34)         FORMAT=3                                    
         DC    AL4(RIGHT44)         FORMAT=4                                    
         DC    AL4(RIGHT54)         FORMAT=5                                    
         DC    AL4(RIGHT64)         FORMAT=6                                    
         DC    AL4(RIGHT74)         FORMAT=7                                    
         DC    AL4(RIGHT84)         FORMAT=8                                    
         DC    AL4(RIGHT94)         FORMAT=9                                    
         DC    AL4(RIGHT104)        FORMAT=10                                   
         DC    AL4(RIGHT114)        FORMAT=11                                   
*                                                                               
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
*                                                                               
SOFTTBL  DS    0D                                                               
*                                                                               
         DC    C'TIME      '                                                    
         DC    C'TIME        ',AL1(NRITIME),AL4(0),AL4(0),AL4(0)                
         DC    AL4(0),AL1(4),AL1(11),AL1(0)                                     
*                                                                               
         DC    C'ASSIGNED  '                                                    
         DC    C'ASSIGNED    ',AL1(NRICOST),AL4(ASSMSK),AL4(0)                  
         DC    AL4(NTASSMSK),AL4(0)                                             
         DC    AL1(9),AL1(12),AL1(0)                                            
*                                                                               
         DC    C'AS+       '                                                    
         DC    C'ASSIGNED +  ',AL1(NRICOST),AL4(ASSMSK+INTMSK),AL4(0)           
         DC    AL4(NTASSMSK+NTINTMSK),AL4(0)                                    
         DC    AL1(9),AL1(12),AL1(0)                                            
*                                                                               
         DC    C'ACTUAL    '                                                    
         DC    C'ACTUAL      ',AL1(NRICOST),AL4(ACTMSK),AL4(0)                  
         DC    AL4(NTACTMSK),AL4(0)                                             
         DC    AL1(9),AL1(12),AL1(0)                                            
*                                                                               
         DC    C'AC+       '                                                    
         DC    C'ACTUAL +    ',AL1(NRICOST),AL4(ACTMSK+INTMSK),AL4(0)           
         DC    AL4(NTACTMSK+NTINTMSK),AL4(0)                                    
         DC    AL1(9),AL1(12),AL1(0)                                            
*                                                                               
         DC    C'GROSS     '                                                    
         DC    C'GROSS       ',AL1(NRICOST),AL4(ACTMSK),AL4(0)                  
         DC    AL4(NTACTMSK),AL4(0)                                             
         DC    AL1(9),AL1(12),AL1(0)                                            
*                                                                               
         DC    C'DIFF      '                                                    
         DC    C'DIFFERENCE  ',AL1(NRICOST),AL4(ACTMSK),AL4(ASSMSK)             
         DC    AL4(NTACTMSK),AL4(NTASSMSK)                                      
         DC    AL1(9),AL1(12),AL1(0)                                            
*                                                                               
         DC    C'INTEG     '                                                    
         DC    C'INTEGRATION ',AL1(NRICOST),AL4(INTMSK),AL4(0)                  
         DC    AL4(NTINTMSK),AL4(0)                                             
         DC    AL1(9),AL1(12),AL1(0)                                            
*                                                                               
         DC    C'TOTAL     '                                                    
         DC    C'TOTAL       ',AL1(NRICOST),AL4(ACTMSK+INTMSK),AL4(0)           
         DC    AL4(NTACTMSK+NTINTMSK),AL4(0)                                    
         DC    AL1(9),AL1(12),AL1(0)                                            
*                                                                               
         DC    C'TOTG      '                                                    
         DC    C'TOTAL GROSS ',AL1(NRICOST),AL4(ACTMSK+INTMSK),AL4(0)           
         DC    AL4(ACTMSK+INTMSK),AL4(0)                                        
         DC    AL1(9),AL1(12),AL1(0)                                            
*                                                                               
         DC    C'TOTN      '                                                    
         DC    C'TOTAL NET   ',AL1(NRICOST),AL4(NTACTMSK+NTINTMSK)              
         DC    AL4(0),AL4(NTACTMSK+NTINTMSK),AL4(0)                             
         DC    AL1(9),AL1(12),AL1(0)                                            
*                                                                               
         DC    C'NET       '                                                    
         DC    C'NET         ',AL1(NRICOST),AL4(NTACTMSK),AL4(0)                
         DC    AL4(NTACTMSK),AL4(0)                                             
         DC    AL1(9),AL1(12),AL1(0)                                            
*                                                                               
         DC    C'COMM      '                                                    
         DC    C'COMMISION   ',AL1(NRICOST),AL4(ACTMSK),AL4(NTACTMSK)           
         DC    AL4(ACTMSK),AL4(NTACTMSK)                                        
         DC    AL1(9),AL1(12),AL1(0)                                            
*                                                                               
         DC    C'NET+      '                                                    
         DC    C'NET+        ',AL1(NRICOST),AL4(NTACTMSK+NTINTMSK)              
         DC    AL4(0),AL4(NTACTMSK+NTINTMSK),AL4(0)                             
         DC    AL1(9),AL1(12),AL1(0)                                            
*                                                                               
         DC    C'CLEARED   '                                                    
         DC    C'CLEARED     ',AL1(NRICOST),AL4(PTGMSK+PIGMSK),AL4(0)           
         DC    AL4(PTNMSK+PINMSK),AL4(0)                                        
         DC    AL1(9),AL1(12),AL1(0)                                            
*                                                                               
         DC    C'NP        '                                                    
         DC    C'NET PAID    ',AL1(NRICOST),AL4(PTNMSK+PINMSK),AL4(0)           
         DC    AL4(PTNMSK+PINMSK),AL4(0)                                        
         DC    AL1(9),AL1(12),AL1(0)                                            
*                                                                               
         DC    C'IP        '                                                    
         DC    C'INT PAID    ',AL1(NRICOST),AL4(PIGMSK),AL4(0)                  
         DC    AL4(PINMSK),AL4(0)                                               
         DC    AL1(9),AL1(12),AL1(0)                                            
*                                                                               
         DC    C'TP        '                                                    
         DC    C'TIME PAID   ',AL1(NRICOST),AL4(PTGMSK),AL4(0)                  
         DC    AL4(PTNMSK),AL4(0)                                               
         DC    AL1(9),AL1(12),AL1(0)                                            
*                                                                               
         DC    C'GP        '                                                    
         DC    C'GROSS PAID  ',AL1(NRICOST),AL4(PTGMSK),AL4(0)                  
         DC    AL4(PTGMSK),AL4(0)                                               
         DC    AL1(9),AL1(12),AL1(0)                                            
*                                                                               
         DC    C'PAID      '                                                    
         DC    C'PAID        ',AL1(NRICOST),AL4(PTGMSK+PIGMSK),AL4(0)           
         DC    AL4(PTNMSK+PINMSK),AL4(0)                                        
         DC    AL1(9),AL1(12),CL1'A'                                            
*                                                                               
         DC    C'TOTP      '                                                    
         DC    C'TOTAL PAID  ',AL1(NRICOST),AL4(PTGMSK+PIGMSK),AL4(0)           
         DC    AL4(PTNMSK+PINMSK),AL4(0)                                        
         DC    AL1(9),AL1(12),CL1'A'                                            
*                                                                               
         DC    C'UNCLEAR   '                                                    
         DC    C'UNCLEARED   ',AL1(NRICOST),AL4(ACTMSK+INTMSK)                  
         DC    AL4(PTGMSK+PIGMSK),AL4(NTACTMSK+NTINTMSK)                        
         DC    AL4(PTNMSK+PINMSK)                                               
         DC    AL1(9),AL1(12),CL1'A'                                            
*                                                                               
         DC    C'IUNP      '                                                    
         DC    C'INT UNPAID  ',AL1(NRICOST),AL4(INTMSK)                         
         DC    AL4(PIGMSK),AL4(NTINTMSK)                                        
         DC    AL4(PINMSK)                                                      
         DC    AL1(9),AL1(12),CL1'A'                                            
*                                                                               
         DC    C'TUNP      '                                                    
         DC    C'TIME UNPAID ',AL1(NRICOST),AL4(ACTMSK)                         
         DC    AL4(PTGMSK),AL4(NTACTMSK)                                        
         DC    AL4(PTNMSK)                                                      
         DC    AL1(9),AL1(12),CL1'A'                                            
*                                                                               
         DC    C'GUNP      '                                                    
         DC    C'GROSS UNPAID',AL1(NRICOST),AL4(ACTMSK)                         
         DC    AL4(PTGMSK),AL4(ACTMSK)                                          
         DC    AL4(PTGMSK)                                                      
         DC    AL1(9),AL1(12),CL1'A'                                            
*                                                                               
         DC    C'TOTUNP    '                                                    
         DC    C'TOTAL UNPAID',AL1(NRICOST),AL4(ACTMSK+INTMSK)                  
         DC    AL4(PTGMSK+PIGMSK),AL4(NTACTMSK+NTINTMSK)                        
         DC    AL4(PTNMSK+PINMSK)                                               
         DC    AL1(9),AL1(12),CL1'A'                                            
*                                                                               
         DC    C'NUNP      '                                                    
         DC    C'NET UNPAID  ',AL1(NRICOST),AL4(NTACTMSK+NTINTMSK)              
         DC    AL4(PTNMSK+PINMSK),AL4(NTACTMSK+NTINTMSK)                        
         DC    AL4(PTNMSK+PINMSK)                                               
         DC    AL1(9),AL1(12),CL1'A'                                            
*                                                                               
         DC    C'UNPAID    '                                                    
         DC    C'UNPAID      ',AL1(NRICOST),AL4(ACTMSK+INTMSK)                  
         DC    AL4(PTGMSK+PIGMSK),AL4(NTACTMSK+NTINTMSK)                        
         DC    AL4(PTNMSK+PINMSK)                                               
         DC    AL1(9),AL1(12),CL1'A'                                            
*                                                                               
         DC    C'BILLED    '                                                    
         DC    C'BILLED      ',AL1(NRICOST),AL4(BTGMSK+BIGMSK),AL4(0)           
         DC    AL4(BTNMSK+BINMSK),AL4(0)                                        
         DC    AL1(9),AL1(12),CL1'A'                                            
*                                                                               
         DC    C'TOTBIL    '                                                    
         DC    C'TOTAL BILLED',AL1(NRICOST),AL4(BTGMSK+BIGMSK),AL4(0)           
         DC    AL4(BTNMSK+BINMSK),AL4(0)                                        
         DC    AL1(9),AL1(12),CL1'A'                                            
*                                                                               
         DC    C'NBIL      '                                                    
         DC    C'NET BILLED  ',AL1(NRICOST),AL4(BTNMSK+BINMSK),AL4(0)           
         DC    AL4(BTNMSK+BINMSK),AL4(0)                                        
         DC    AL1(9),AL1(12),CL1'A'                                            
*                                                                               
         DC    C'IBILL     '                                                    
         DC    C'INT BILLED  ',AL1(NRICOST),AL4(BIGMSK),AL4(0)                  
         DC    AL4(BINMSK),AL4(0)                                               
         DC    AL1(9),AL1(12),CL1'A'                                            
*                                                                               
         DC    C'ABILL     '                                                    
         DC    C'ACT BILLED  ',AL1(NRICOST),AL4(BTGMSK),AL4(0)                  
         DC    AL4(BTNMSK),AL4(0)                                               
         DC    AL1(9),AL1(12),CL1'A'                                            
*                                                                               
         DC    C'GBILL     '                                                    
         DC    C'GROSS BILLED',AL1(NRICOST),AL4(BTGMSK),AL4(0)                  
         DC    AL4(BTNMSK),AL4(0)                                               
         DC    AL1(9),AL1(12),CL1'A'                                            
*                                                                               
         DC    C'TBILL     '                                                    
         DC    C'TIME BILLED ',AL1(NRICOST),AL4(BTGMSK),AL4(0)                  
         DC    AL4(BTNMSK),AL4(0)                                               
         DC    AL1(9),AL1(12),CL1'A'                                            
*                                                                               
         DC    C'BILLA     '                                                    
         DC    C'BILLABLE    ',AL1(NRICOST),AL4(CASSMSK+INTMSK)                 
         DC    AL4(BTGMSK+BIGMSK),AL4(NTCSSMSK+NTINTMSK)                        
         DC    AL4(BTNMSK+BINMSK)                                               
         DC    AL1(9),AL1(12),CL1'A'                                            
*                                                                               
         DC    C'IBILLA    '                                                    
         DC    C'INT BILLABLE',AL1(NRICOST),AL4(INTMSK)                         
         DC    AL4(BIGMSK),AL4(NTINTMSK)                                        
         DC    AL4(BINMSK)                                                      
         DC    AL1(9),AL1(12),CL1'A'                                            
*                                                                               
         DC    C'TBILLA    '                                                    
         DC    C'TIM BILLABLE',AL1(NRICOST),AL4(CASSMSK)                        
         DC    AL4(BTGMSK),AL4(NTCSSMSK)                                        
         DC    AL4(BTNMSK)                                                      
         DC    AL1(9),AL1(12),CL1'A'                                            
*                                                                               
         DC    C'TOTBILLA  '                                                    
         DC    C'TOT BILLABLE',AL1(NRICOST),AL4(CASSMSK+INTMSK)                 
         DC    AL4(BTGMSK+BIGMSK),AL4(NTCSSMSK+NTINTMSK)                        
         DC    AL4(BTNMSK+BINMSK)                                               
         DC    AL1(9),AL1(12),CL1'A'                                            
*                                                                               
         DC    C'GBILLA    '                                                    
         DC    C'GRO BILLABLE',AL1(NRICOST),AL4(CASSMSK)                        
         DC    AL4(BTGMSK),AL4(NTCSSMSK)                                        
         DC    AL4(BTNMSK)                                                      
         DC    AL1(9),AL1(12),CL1'A'                                            
*                                                                               
         DC    C'NBILLA    '                                                    
         DC    C'NET BILLABLE',AL1(NRICOST),AL4(NTCSSMSK+NTINTMSK)              
         DC    AL4(BTNMSK+BINMSK),AL4(NTCSSMSK+NTINTMSK)                        
         DC    AL4(BTNMSK+BINMSK)                                               
         DC    AL1(9),AL1(12),CL1'A'                                            
*                                                                               
         DC    C'ALLOCATED '                                                    
         DC    C'ALLOCATED   ',AL1(NRICOST),AL4(ASSMSK),AL4(0)                  
         DC    AL4(NTASSMSK),AL4(0)                                             
         DC    AL1(9),AL1(12),AL1(0)                                            
*                                                                               
         DC    C'NETW      '                                                    
         DC    C'NETW        ',AL1(NRINET),AL4(0),AL4(0),AL4(0),AL4(0)          
         DC    AL1(4),AL1(4),AL1(0)                                             
*                                                                               
         DC    C'NETWORK$  '                                                    
         DC    C'NETWORK$    ',AL1(NRICOST),AL4(ACTMSK),AL4(0)                  
         DC    AL4(NTACTMSK),AL4(0)                                             
         DC    AL1(9),AL1(12),AL1(0)                                            
*                                                                               
         DC    C'PROGRAM   '                                                    
         DC    C'PROGRAM     ',AL1(NRIPRGNM),AL4(0),AL4(0),AL4(0)               
         DC    AL4(0),AL1(16),AL1(16),AL1(0)                                    
*                                                                               
         DC    C'PROGTIME  '                                                    
         DC    C'PROG TIME   ',AL1(NRICOST),AL4(ACTMSK),AL4(0)                  
         DC    AL4(NTACTMSK),AL4(0)                                             
         DC    AL1(9),AL1(12),AL1(0)                                            
*                                                                               
         DC    C'NINT      '                                                    
         DC    C'NET INT     ',AL1(NRICOST),AL4(NTINTMSK),AL4(0)                
         DC    AL4(NTINTMSK),AL4(0)                                             
         DC    AL1(9),AL1(12),AL1(0)                                            
*                                                                               
         DC    C'EST       '                                                    
         DC    C'EST         ',AL1(NRIEST1),AL4(0),AL4(0),AL4(0),AL4(0)         
         DC    AL1(1),AL1(3),AL1(0)                                             
*                                                                               
         DC    C'PAK       '                                                    
         DC    C'PAK         ',AL1(NRIPAK1),AL4(0),AL4(0),AL4(0),AL4(0)         
         DC    AL1(1),AL1(3),AL1(0)                                             
*                                                                               
         DC    C'DATE      '                                                    
         DC    C'DATE        ',AL1(NRIDATE),AL4(0),AL4(0),AL4(0),AL4(0)         
         DC    AL1(3),AL1(5),AL1(0)                                             
*                                                                               
         DC    C'PCODE     '                                                    
         DC    C'PCODE       ',AL1(NRIPRGCD),AL4(0),AL4(0),AL4(0)               
         DC    AL4(0),AL1(6),AL1(6),AL1(0)                                      
*                                                                               
         DC    C'DAY       '                                                    
         DC    C'DAY         ',AL1(NRIDAY3),AL4(0),AL4(0),AL4(0),AL4(0)         
         DC    AL1(3),AL1(3),AL1(0)                                             
*                                                                               
         DC    C'LEN       '                                                    
         DC    C'LEN         ',AL1(NRILEN),AL4(0),AL4(0),AL4(0),AL4(0)          
         DC    AL1(4),AL1(3),AL1(0)                                             
*                                                                               
         DC    C'PRODUCT   '                                                    
         DC    C'PRODUCT     ',AL1(NRIPRD3),AL4(0),AL4(0),AL4(0),AL4(0)         
         DC    AL1(7),AL1(7),AL1(0)                                             
*                                                                               
         DC    C'CLI       '                                                    
         DC    C'CLI         ',AL1(NRICLT3),AL4(0),AL4(0),AL4(0),AL4(0)         
         DC    AL1(3),AL1(3),AL1(0)                                             
*                                                                               
         DC    C'UNITS     '                                                    
         DC    C'UNITS       ',AL1(NRIUNIT),AL4(0),AL4(0),AL4(0),AL4(0)         
         DC    AL1(4),AL1(5),AL1(0)                                             
*                                                                               
         DC    C'AFFTIM    '                                                    
         DC    C'AFFTIM      ',AL1(NRIAFFT),AL4(0),AL4(0),AL4(0),AL4(0)         
         DC    AL1(2),AL1(5),AL1(0)                                             
*                                                                               
         DC    C'BDATE     '                                                    
         DC    C'BDATE       ',AL1(NRIBILDT),AL4(0),AL4(0),AL4(0)               
         DC    AL4(0),AL1(2),AL1(5),CL1'A'                                      
*                                                                               
         DC    C'PDATE     '                                                    
         DC    C'PDATE       ',AL1(NRIPAYDT),AL4(0),AL4(0),AL4(0)               
         DC    AL4(0),AL1(2),AL1(5),CL1'A'                                      
*                                                                               
         DC    C'BPRD      '                                                    
         DC    C'BPRD        ',AL1(NRIBILPR),AL4(0),AL4(0),AL4(0)               
         DC    AL4(0),AL1(4),AL1(4),CL1'A'                                      
*                                                                               
         DC    C'GAP2      '                                                    
         DC    C'            ',AL1(NRIGAP),AL4(0),AL4(0),AL4(0),AL4(0)          
         DC    AL1(0),AL1(0),CL1'A'                                             
*                                                                               
         DC    C'GAP3      '                                                    
         DC    C'            ',AL1(NRIGAP),AL4(0),AL4(0),AL4(0),AL4(0)          
         DC    AL1(0),AL1(1),AL1(0)                                             
*                                                                               
         DC    C'GAP4      '                                                    
         DC    C'            ',AL1(NRIGAP),AL4(0),AL4(0),AL4(0),AL4(0)          
         DC    AL1(0),AL1(2),AL1(0)                                             
*                                                                               
         DC    C'GAP5      '                                                    
         DC    C'            ',AL1(NRIGAP),AL4(0),AL4(0),AL4(0),AL4(0)          
         DC    AL1(0),AL1(3),AL1(0)                                             
*                                                                               
         DC    C'GAP6      '                                                    
         DC    C'            ',AL1(NRIGAP),AL4(0),AL4(0),AL4(0),AL4(0)          
         DC    AL1(0),AL1(4),AL1(0)                                             
*                                                                               
         DC    C'GAP7      '                                                    
         DC    C'            ',AL1(NRIGAP),AL4(0),AL4(0),AL4(0),AL4(0)          
         DC    AL1(0),AL1(5),AL1(0)                                             
*                                                                               
         DC    C'GAP8      '                                                    
         DC    C'            ',AL1(NRIGAP),AL4(0),AL4(0),AL4(0),AL4(0)          
         DC    AL1(0),AL1(6),AL1(0)                                             
*                                                                               
         DC    C'GAP9      '                                                    
         DC    C'            ',AL1(NRIGAP),AL4(0),AL4(0),AL4(0),AL4(0)          
         DC    AL1(0),AL1(7),AL1(0)                                             
         SPACE 1                                                                
         DC    X'FF'                                                            
         EJECT                                                                  
RIGHT10  DC    C'DATE,PROG,NETW,DAY,TIME,PROD,ACT,INT,TOT'                      
         DC    CL50' '                                                          
RIGHT11  DC    C'DATE,PROG,NETW,DAY,TIME,PROD,TP,IP,TOTP'                       
         DC    CL50' '                                                          
RIGHT12  DC    C'DATE,PROG,NETW,DAY,TIME,PROD,TUNP,IUNP,TOTUNP'                 
         DC    CL50' '                                                          
RIGHT13  DC    C'DATE,PROG,NETW,DAY,TIME,PROD,TB,IB,TOTB'                       
         DC    CL50' '                                                          
RIGHT14  DC    C'DATE,PROG,NETW,DAY,TIME,PROD,TBILLA,IBILLA,TOTBILLA'           
         DC    CL50' '                                                          
RIGHT20  DC    C'DATE,PROG,NETW,DAY,TIME,PROD,ACT,INT,TOT,EST,PAK'              
         DC    CL50' '                                                          
RIGHT21  DC    C'DATE,PROG,NETW,DAY,TIME,PROD,TP,IP,TOTP,'                      
         DC    C'EST,PAK',CL50' '                                               
RIGHT22  DC    C'DATE,PROG,NETW,DAY,TIME,PROD,TUNP,IUNP,TOTUNP,'                
         DC    C'EST,PAK',CL50' '                                               
RIGHT23  DC    C'DATE,PROG,NETW,DAY,TIME,PROD,TB,IB,TOTB,'                      
         DC    C'EST,PAK',CL50' '                                               
RIGHT24  DC    C'DATE,PROG,NETW,DAY,TIME,PROD,TBILLA,IBILLA,TOTBILLA,'          
         DC    C'EST,PAK',CL50' '                                               
RIGHT30  DC    C'DATE,PROG,NETW,DAY,TIME,PROD,ASS,ACT,DIF,INT'                  
         DC    CL50' '                                                          
RIGHT31  DC    C'DATE,PROG,NETW,DAY,TIME,PROD,TP,IP'                            
         DC    CL50' '                                                          
RIGHT32  DC    C'DATE,PROG,NETW,DAY,TIME,PROD,TUNP,IUNP'                        
         DC    CL50' '                                                          
RIGHT33  DC    C'DATE,PROG,NETW,DAY,TIME,PROD,TB,IB'                            
         DC    CL50' '                                                          
RIGHT34  DC    C'DATE,PROG,NETW,DAY,TIME,PROD,TBILLA,IBILLA'                    
         DC    CL50' '                                                          
RIGHT40  DC    C'DATE,PROG,NETW,DAY,TIME,PROD,AS+,AC+,DIFF,INT'                 
         DC    CL50' '                                                          
RIGHT41  DC    C'DATE,PROG,NETW,DAY,TIME,PROD,TOTP,IP'                          
         DC    CL50' '                                                          
RIGHT42  DC    C'DATE,PROG,NETW,DAY,TIME,PROD,TOTUNP,IUNP'                      
         DC    CL50' '                                                          
RIGHT43  DC    C'DATE,PROG,NETW,DAY,TIME,PROD,TOTB,IB'                          
         DC    CL50' '                                                          
RIGHT44  DC    C'DATE,PROG,NETW,DAY,TIME,PROD,TOTBILLA,IBILLA'                  
         DC    CL50' '                                                          
RIGHT50  DC    C'DATE,PROG,NETW,DAY,TIME,PROD,GROSS'                            
         DC    CL50' '                                                          
RIGHT51  DC    C'DATE,PROG,NETW,DAY,TIME,PROD,GP'                               
         DC    CL50' '                                                          
RIGHT52  DC    C'DATE,PROG,NETW,DAY,TIME,PROD,GUNP'                             
         DC    CL50' '                                                          
RIGHT53  DC    C'DATE,PROG,NETW,DAY,TIME,PROD,GB'                               
         DC    CL50' '                                                          
RIGHT54  DC    C'DATE,PROG,NETW,DAY,TIME,PROD,GBILLA'                           
         DC    CL50' '                                                          
RIGHT60  DC    C'DATE,PROG,NETW,DAY,TIME,PROD,GROSS,NET'                        
         DC    CL50' '                                                          
RIGHT61  DC    C'DATE,PROG,NETW,DAY,TIME,PROD,TOTP,NP'                          
         DC    CL50' '                                                          
RIGHT62  DC    C'DATE,PROG,NETW,DAY,TIME,PROD,TOTUNP,NUNP'                      
         DC    CL50' '                                                          
RIGHT63  DC    C'DATE,PROG,NETW,DAY,TIME,PROD,TOTB,NB'                          
         DC    CL50' '                                                          
RIGHT64  DC    C'DATE,PROG,NETW,DAY,TIME,PROD,TOTBILLA,NBILLA'                  
         DC    CL50' '                                                          
RIGHT70  DC    C'DATE,PROG,NETW,DAY,TIME,PROD,GROSS,INT,TOT'                    
         DC    CL50' '                                                          
RIGHT71  DC    C'DATE,PROG,NETW,DAY,TIME,PROD,GP,IP,TOTP'                       
         DC    CL50' '                                                          
RIGHT72  DC    C'DATE,PROG,NETW,DAY,TIME,PROD,GUNP,IUNP,TOTUNP'                 
         DC    CL50' '                                                          
RIGHT73  DC    C'DATE,PROG,NETW,DAY,TIME,PROD,GB,IB,TB'                         
         DC    CL50' '                                                          
RIGHT74  DC    C'DATE,PROG,NETW,DAY,TIME,PROD,GBILLA,IBILLA,TOTBILLA'           
         DC    CL50' '                                                          
RIGHT80  DC    C'DATE,PROG,NETW,DAY,TIME,PROD,GROSS,INT,TOT,TOTN'               
         DC    CL50' '                                                          
RIGHT81  DC    C'DATE,PROG,NETW,DAY,TIME,PROD,GP,IP,NP'                         
         DC    CL50' '                                                          
RIGHT82  DC    C'DATE,PROG,NETW,DAY,TIME,PROD,GUNP,IUNP,TOTUNP,NUNP'            
         DC    CL50' '                                                          
RIGHT83  DC    C'DATE,PROG,NETW,DAY,TIME,PROD,GB,IB,TOTB,NB'                    
         DC    CL50' '                                                          
RIGHT84  DC    C'DATE,PROG,NETW,DAY,TIME,PROD,GBILLA,IBILLA,NBILLA'             
         DC    CL50' '                                                          
RIGHT90  DC    C'DATE,PROG,NETW,DAY,TIME,PROD,GROSS,COMM,NET'                   
         DC    CL50' '                                                          
RIGHT91  DC    C'DATE,PROG,NETW,DAY,TIME,PROD,TOTP,COMM,NP'                     
         DC    CL50' '                                                          
RIGHT92  DC    C'DATE,PROG,NETW,DAY,TIME,PROD,TOTUNP,COMM,NUNP'                 
         DC    CL50' '                                                          
RIGHT93  DC    C'DATE,PROG,NETW,DAY,TIME,PROD,TB,COMM,NB'                       
         DC    CL50' '                                                          
RIGHT94  DC    C'DATE,PROG,NETW,DAY,TIME,PROD,TOTBILLA,COMM,NBILLA'             
         DC    CL50' '                                                          
RIGHT100 DC    C'DATE,PROG,NETW,DAY,TIME,PROD,GROSS,INT,TOT,TOTN,EST,'          
         DC    C'PAK'                                                           
         DC    CL50' '                                                          
RIGHT101 DC    C'DATE,PROG,NETW,DAY,TIME,PROD,GP,IP,TOTP,NP,'                   
         DC    C'EST,PAK'                                                       
         DC    CL50' '                                                          
RIGHT102 DC    C'DATE,PROG,NETW,DAY,TIME,PROD,GUNP,IUNP,TOTUNP,NUNP,'           
         DC    C'EST,PAK'                                                       
         DC    CL50' '                                                          
RIGHT103 DC    C'DATE,PROG,NETW,DAY,TIME,PROD,GB,IB,TOTB,NB,'                   
         DC    C'EST,PAK'                                                       
         DC    CL50' '                                                          
RIGHT104 DC    C'DATE,PROG,NETW,DAY,TIME,PROD,GBILLA,IBILLA,TOTBILLA,'          
         DC    C'NBILLA,EST,PAK'                                                
         DC    CL50' '                                                          
RIGHT110 DC    C'DATE,PROG,NETW,DAY,TIME,PROD,INT'                              
         DC    CL50' '                                                          
RIGHT111 DC    C'DATE,PROG,NETW,DAY,TIME,PROD,IP'                               
         DC    CL50' '                                                          
RIGHT112 DC    C'DATE,PROG,NETW,DAY,TIME,PROD,IUNP'                             
         DC    CL50' '                                                          
RIGHT113 DC    C'DATE,PROG,NETW,DAY,TIME,PROD,IB'                               
         DC    CL50' '                                                          
RIGHT114 DC    C'DATE,PROG,NETW,DAY,TIME,PROD,IBILLA'                           
         DC    CL50' '                                                          
*                                                                               
         EJECT                                                                  
*                                                                               
         SPACE 3                                                                
*                                                                               
SOFTD    DSECT                                                                  
SFREQ    DS    CL10                REQUESTABLE AS                               
SFALPH   DS    CL12                ALPHA NAME FOR HEADER                        
SFTYP    DS    CL1                 INPUT TYPE                                   
SFGOPT1  DS    CL4                 GROSS OPT 1                                  
SFGOPT2  DS    CL4                 GROSS OPT 2                                  
SFNOPT1  DS    CL4                 NET OPT 1                                    
SFNOPT2  DS    CL4                 NET OPT 2                                    
SFINLEN  DS    CL1                 INTERNAL RECORD LENGTH                       
SFPRLEN  DS    CL1                 PRINT LENGTH                                 
SFFILTER DS    CL1                 FILTER A=PRINT ON AUDIT TRAIL                
SFNTRYLN EQU   *-SOFTD                                                          
*                                                                               
*                                                                               
       ++INCLUDE NEMEDFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE NEMEDD1D                                                       
*                                                                               
         EJECT                                                                  
         PRINT OFF                                                              
*                                  NEGENINCLS                                   
*                                  NEGENCOSTS                                   
*                                  NERWINCLS                                    
*                                  NERWEQUS                                     
*                                  NEACCWS                                      
       ++INCLUDE NEGENINCLS                                                     
       ++INCLUDE NEGENCOSTS                                                     
       ++INCLUDE NERWINCLS                                                      
       ++INCLUDE NERWEQUS                                                       
       ++INCLUDE NEACCWS                                                        
         PRINT ON                                                               
YES      EQU   C'Y'                                                             
NO       EQU   C'N'                                                             
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'112NEMED81   05/01/02'                                      
         END                                                                    
