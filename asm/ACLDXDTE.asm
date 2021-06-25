*          DATA SET ACLDXDTE   AT LEVEL 011 AS OF 07/21/00                      
*PHASE ACLDXDTE                                                                 
*INCLUDE ACRECTYP                                                               
*INCLUDE DATCON                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE PRNTBL                                                                 
         TITLE 'DATE CHECK'                                                     
***********************************************************************         
*                                                                               
* PARAMETER LIST                                                                
*                                                                               
* P1=A(RECORD)      PASS FIRST BYTE X'00'= INITIALISE                           
*                                   X'01'= RECORD IN CORE                       
*                                   X'FF'= END OF FILE                          
*                   RETURN VALUE    X'00'= KEEP RECORD                          
*                                   X'FF'= PURGE RECORD                         
*                                   X'FF'/C'EOJ'=PURGE & CAUSE EOJ              
* P2=A(TAPEOUT)     PASS FIRST BYTE X'80'= TAPE INPUT                           
*                                   X'40'= TAPE OUTPUT                          
*                                   X'20'= RECORD IS I/S FILE RECORD            
* P3=A(PARAM CARD)  PASS FIRST BYTE C'Y' = YOU ASKED ME TO RETURN               
*                   RETURN          C'R' = RETURN BACK TO EXTERNAL              
* P4=A(FILE DEFN)                                                               
* P5=A(PRINTER)                                                                 
* P6=A(CPRINT)                                                                  
*                                                                               
***********************************************************************         
                                                                                
ACLDXDTE CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**DATE*,R9                                                     
         L     RA,VCPRINT                                                       
         USING DPRINT,RA                                                        
         ST    R1,APARM                                                         
         MVC   PLIST,0(R1)                                                      
         B     DMXCTL                                                           
*                                                                               
DMXCTL   CLI   PLIST,X'00'                                                      
         BE    DMXINIT                                                          
         CLI   PLIST,X'01'                                                      
         BE    DMXREC                                                           
         CLI   PLIST,X'FF'                                                      
         BE    DMXEOF                                                           
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* INITIALIZE                                                          *         
***********************************************************************         
                                                                                
DMXINIT  DS    0H                                                               
         LA    R0,2                                                             
         LA    R3,HICH6                                                         
         MVC   WORK(6),=C'990630'                                               
*                                                                               
DMXINIT3 LA    R4,HICH6-HI(R3)                                                  
         GOTO1 DATCON,DMCB,(0,WORK),(0,0(R4))                                   
*                                                                               
         LA    R4,HICH8-HI(R3)                                                  
         GOTO1 DATCON,DMCB,(0,HICH6),(10,0(R4))  MM/DD/YY                       
*                                                                               
         LA    R4,HICH6-HI(R3)                                                  
         LA    R5,HIPW3-HI(R3)                                                  
         GOTO1 DATCON,DMCB,(0,0(R4)),(1,0(R5))   PWO                            
*                                                                               
         MVC   HIPC3-HI(L'HIPC3,R3),HIPW3-HI(R3)   PACKED COMPLEMENT            
         XC    HIPC3-HI(L'HIPC3,R3),FFS                                         
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,7,HIPW3-HI(R3)                                                
         LNR   RF,RF                                                            
         STCM  RF,7,HITC3-HI(R3)                2'S COMPLEMENT                  
*                                                                               
         MVC   HIPW2-HI(L'HIPW2,R3),HIPW3-HI(R3) 2BYTE PACKED                   
*                                                                               
         MVC   HIPC2-HI(L'HIPC2,R3),HIPC3-HI(R3)   2BYTE COMPLEMENT             
*                                                                               
         LA    R4,HICH6-HI(R3)                                                  
         LA    R5,HICO2-HI(R3)                                                  
         GOTO1 DATCON,DMCB,(0,0(R4)),(2,0(R5))  COMPRESSED                      
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,3,HICO2-HI(R3)                                                
         LCR   RF,RF                                                            
         STCM  RF,3,HICC2-HI(R3)                COMPRESSD COMPLEMENT            
*                                                                               
         MVC   WORK(6),=C'690630'                                               
         LA    R3,LOCH6                                                         
         BCT   R0,DMXINIT3                                                      
*                                                                               
         LA    R3,LO-HI                                                         
         GOTO1 PRNTBL,DMCB,(0),HI,C'DUMP',(R3),=C'2D'                           
         GOTO1 PRNTBL,DMCB,(0),LO,C'DUMP',(R3),=C'2D'                           
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* PROCESS RECORD                                                      *         
***********************************************************************         
                                                                                
DMXREC   DS    0H                                                               
*        CP    COUNT,MXCNT                                                      
*        BH    DMXPGEOF                                                         
*                                                                               
         MVI   FLAG,C'R'           RECORD                                       
         L     R2,AREC             IDENTIFY RECORD TYPE                         
         USING ACTRECD,R2                                                       
         GOTO1 ACRECTYP,DMCB,(C'D',ACTRECD)                                     
         MVC   COMPDSP,2(R1)       SAVE DISPLACEMENT TO COMPANY                 
         MVC   RECTYP,0(R1)        RECORD TYPE                                  
         MVC   CURCOMP,1(R1)       CURRENT COMPANY                              
*                                                                               
         TM    ACTRSTA,X'80'                                                    
         BO    EXIT                SKIP DELETED                                 
*                                                                               
**********************************************************************          
*  PROBLEMS CAN BE EXCLUDED HERE.                                               
**********************************************************************          
*                                                                               
*                                                                               
*  TAXKDATE ON SOME OLD RECORDS APPEARS TO BE OFF BY 2 BYTES                    
DMXRX1   CLI   RECTYP,ACRTTAX                                                   
         BNE   DMXRX2                                                           
         USING TAXRECD,R2                                                       
         CLC   TAXKDATE+1(2),BINZ                                               
         BE    EXIT                                                             
DMXRX2   DS    0H                                                               
         DROP  R2                                                               
*                                                                               
         USING ACTRECD,R2                                                       
         CLI   RECTYP,ACRTSRM      STORED REQUEST HAVE SAME TYPE                
         BNE   DMXR3                                                            
         L     R3,=A(SRMR)                                                      
         CLI   SRMKTYP-SRMRECD(R2),SRMKTYPQ                                     
         BE    *+8                                                              
         L     R3,=A(SRFR)                                                      
         B     DMXR5                                                            
*                                                                               
DMXR3    L     R3,=A(RECT)         FIND MATCHING RECORD TYPE                    
DMXR4    CLI   0(R3),X'FF'                                                      
         BE    DMXELM                                                           
         CLC   RECTYP,0(R3)                                                     
         BE    DMXR5                                                            
         LA    R3,L'RECT(R3)                                                    
         B     DMXR4                                                            
DMXR5    BAS   RE,DATE                                                          
         B     DMXELM                                                           
         EJECT                                                                  
***********************************************************************         
* PROCESS ELEMENTS                                                    *         
***********************************************************************         
                                                                                
DMXELM   MVI   FLAG,C'E'           ELEMENT                                      
         SR    R0,R0               MATCHING ELEMENT CODE                        
         LA    R2,ACTRFST                                                       
DMXE3    CLI   0(R2),0                                                          
         BE    DMXKEEP                                                          
**********************************************************************          
*  PROBLEMS CAN BE EXCLUDED HERE.                                               
**********************************************************************          
*  EPRDATE FIELD IS VERY OFTEN INCORRECT                                        
         CLI   0(R2),EPRELQ                                                     
         BE    DMXE9                                                            
*                                                                               
         LA    RF,APAGE+4          FIND THE RIGHT PAGE                          
         ICM   R3,15,0(RF)                                                      
         CLC   0(1,R2),0(R3)       TEST ELEMENT TO NEXT PAGE                    
         BL    *+12                                                             
         LA    RF,4(RF)                                                         
         B     *-18                                                             
*                                                                               
         SH    RF,=H'4'            USE PREVIOUS PAGE                            
         ICM   R3,15,0(RF)                                                      
*                                                                               
DMXE5    CLI   0(R3),X'FF'         END OF PAGE                                  
         BE    DMXE9                                                            
         CLC   0(1,R2),0(R3)       TEST ELEMENT TO NEXT PAGE                    
         BL    *+12                                                             
         BH    DMXE9                                                            
         BAS   RE,DATE                                                          
         LA    R3,L'ELMT(R3)       NEXT TABLE ENTRY                             
         B     DMXE5                                                            
*                                                                               
DMXE9    IC    R0,1(R2)                                                         
         AR    R2,R0               NEXT ELEMENT                                 
         B     DMXE3                                                            
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* DATE CONVERSION                                                     *         
*  NTRY  R2 = A(DATA AREA)                                            *         
*        R3 = A(TABLE ENTRY)                                          *         
***********************************************************************         
                                                                                
DATE     CLI   0(R2),0             TEST ANY DATA                                
         BER   RE                                                               
         CLI   0(R2),X'FF'         TEST HIGH DATA                               
         BER   RE                                                               
         CLI   FLAG,C'E'           ELEMENT DATA                                 
         BNE   DATE1                                                            
         CLC   1(1,R2),1(R3)       TEST ELEMENT LENGTH                          
         BNHR  RE                                                               
DATE1    NTR1  ,                                                                
         MVC   NEWDATE,SPACES                                                   
         SR    R5,R5                                                            
         ICM   R5,1,1(R3)          DISP. TO DATE                                
         AR    R5,R2               R5=DATE FIELD                                
*                                                                               
         SR    RF,RF                                                            
         IC    RF,2(R3)            ROUTINE NUMBER                               
         BCTR  RF,0                                                             
         SLL   RF,2                                                             
         L     RF,BTAB(RF)                                                      
         BR    RF                                                               
*                                                                               
BTAB     DC    A(DCH6)                                                          
         DC    A(DCH8)                                                          
         DC    A(DPW3)                                                          
         DC    A(DPC3)                                                          
         DC    A(DTC3)                                                          
         DC    A(DPW2)                                                          
         DC    A(DPC2)                                                          
         DC    A(DCO2)                                                          
         DC    A(DCC2)                                                          
         EJECT                                                                  
*                                                                               
* CHARACTER - 6 BYTES YYMMDD                                                    
*                                                                               
DCH6     DS    0H                                                               
         LA    R1,6                                                             
         LA    R6,HICH6                                                         
         LA    R7,LOCH6                                                         
         B     DATE5                                                            
         GOTO1 DATCON,DMCB,(0,0(R5)),(0,NEWDATE)                                
         B     DATE5                                                            
                                                                                
*                                                                               
* CHARACTER - 8 BYTES MM/DD/YY                                                  
*                                                                               
DCH8     DS    0H                                                               
         LA    R1,8                                                             
         LA    R6,HICH8                                                         
         LA    R7,LOCH8                                                         
         B     DATE5                                                            
         GOTO1 DATCON,DMCB,(4,0(R5)),(0,NEWDATE)                                
         B     DATE5                                                            
                                                                                
*                                                                               
* PACKED WITHOUT SIGN - 3 BYTES YYMMDD                                          
*                                                                               
DPW3     DS    0H                                                               
         LA    R1,3                                                             
         LA    R6,HIPW3                                                         
         LA    R7,LOPW3                                                         
         B     DATE5                                                            
         GOTO1 DATCON,DMCB,(1,0(R5)),(0,NEWDATE)                                
         B     DATE5                                                            
*                                                                               
* PACKED COMPLEMENT- 2 BYTES YYMM                                               
*                                                                               
DPC3     DS    0H                                                               
         LA    R1,3                                                             
         LA    R7,HIPC3                                                         
         LA    R6,LOPC3                                                         
         B     DATE5                                                            
         MVC   WORK(3),0(R5)                                                    
         XC    WORK(3),FFS                                                      
         GOTO1 DATCON,DMCB,(1,WORK),(0,NEWDATE)                                 
         B     DATE5                                                            
                                                                                
*                                                                               
* TWO'S COMPLEMENT- 3 BYTES YYMMDD                                              
*                                                                               
DTC3     DS    0H                                                               
         LA    R1,3                                                             
         LA    R7,HITC3                                                         
         LA    R6,LOTC3                                                         
         B     DATE5                                                            
         SR    RF,RF                                                            
         ICM   RF,3,0(R5)                                                       
         LPR   RF,RF                                                            
         STCM  RF,3,WORK                                                        
         GOTO1 DATCON,DMCB,(1,WORK),(0,NEWDATE)                                 
         B     DATE5                                                            
         EJECT                                                                  
*                                                                               
* PACKED WITHOUT SIGN - 2 BYTES YYMM                                            
*                                                                               
DPW2     DS    0H                                                               
         LA    R1,2                                                             
         LA    R6,HIPW2                                                         
         LA    R7,LOPW2                                                         
         B     DATE5                                                            
         MVC   WORK(2),0(R5)                                                    
         MVI   WORK+2,1                                                         
         GOTO1 DATCON,DMCB,(1,WORK),(0,NEWDATE)                                 
         B     DATE5                                                            
*                                                                               
* PACKED COMPLEMENT- 2 BYTES YYMM                                               
*                                                                               
DPC2     DS    0H                                                               
         LA    R1,2                                                             
         LA    R7,HIPC2                                                         
         LA    R6,LOPC2                                                         
         B     DATE5                                                            
         MVC   WORK(2),0(R5)                                                    
         XC    WORK(2),FFS                                                      
         MVI   WORK+2,1                                                         
         GOTO1 DATCON,DMCB,(1,WORK),(0,NEWDATE)                                 
         B     DATE5                                                            
                                                                                
*                                                                               
* COMPRESSED - 2 BYTES YYMMDD                                                   
*                                                                               
DCO2     DS    0H                                                               
         LA    R1,2                                                             
         LA    R6,HICO2                                                         
         LA    R7,LOCO2                                                         
         B     DATE5                                                            
         GOTO1 DATCON,DMCB,(2,0(R5)),(0,NEWDATE)                                
         B     DATE5                                                            
                                                                                
*                                                                               
* COMPRESSED COMPLEMENT- 2 BYTES YYMMDD                                         
*                                                                               
DCC2     DS    0H                                                               
         LA    R1,2                                                             
         LA    R7,HICC2                                                         
         LA    R6,LOCC2                                                         
         B     DATE5                                                            
         MVC   WORK(2),0(R5)                                                    
         XC    WORK(2),FFS                                                      
         GOTO1 DATCON,DMCB,(2,WORK),(0,NEWDATE)                                 
         B     DATE5                                                            
         EJECT                                                                  
DATE5    DS    0H                                                               
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R5),BINZ         TEST ZEROS                                  
         BE    EXIT                                                             
*                                                                               
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R5),FFS          TEST FFS                                    
         BE    EXIT                                                             
*                                                                               
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R5),SPACES       TEST SPACES                                 
         BE    EXIT                                                             
*                                                                               
         EX    R1,*+4                                                           
         MVC   TDATE(0),0(R5)      DATA FROM KEY OR ELEMENT                     
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R5),0(R6)       COMPARE TO HIGH                              
         BH    DATE7                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R5),0(R7)       COMPARE TO LOW                               
         BL    DATE7                                                            
         B     EXIT                                                             
*                                                                               
         CLC   NEWDATE,HICH6                                                    
         BH    *+14                                                             
         CLC   NEWDATE,LOCH6                                                    
         BNL   EXIT                                                             
         MVC   TDATE,NEWDATE       BAD DATE                                     
*                                                                               
DATE7    LA    RF,RECCNT           TEST MAX DUMPS PER TYPE                      
         CLI   FLAG,C'R'                                                        
         BE    *+8                                                              
         LA    RF,ELMCNT                                                        
         SR    RE,RE                                                            
         IC    RE,0(R2)                                                         
         AR    RF,RE                                                            
         CLI   0(RF),3             TEST MAX DUMPS                               
         BNL   DATEX                                                            
         IC    RE,0(RF)                                                         
         LA    RE,1(RE)                                                         
         STC   RE,0(RF)                                                         
*                                                                               
         MVC   TTABL,0(R3)         TABLE ENTRY                                  
         XC    TDATA,TDATA                                                      
         SR    R1,R1                                                            
         LA    RF,ACTRFST-ACTRECD  RF=KEY LENGTH                                
         CLI   FLAG,C'R'           TEST RECORD                                  
         BE    *+8                                                              
         IC    RF,1(R2)            RF=ELEMENT LENGTH                            
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   TDATA(0),0(R2)    DATA TO TRACE AREA                             
         LA    RF,12(RF)                                                        
         LA    R7,=C'ERROR'                                                     
         GOTO1 PRNTBL,DMCB,(5,(R7)),TRACE,C'DUMP',(RF),=C'2D'                   
         CLI   FLAG,C'R'                                                        
         BE    DATEX                                                            
         LA    RF,ACTRFST-ACTRECD                                               
         GOTO1 PRNTBL,DMCB,0,AREC,C'DUMP',(RF),=C'2D'                           
*                                                                               
DATEX    AP    COUNT,=P'1'                                                      
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* PRINT TOTALS                                                        *         
***********************************************************************         
                                                                                
DMXEOF   DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* EXIT CONDITIONS                                                     *         
***********************************************************************         
                                                                                
DMXPURGE L     R1,APARM            PURGE RECORD EXIT                            
         MVI   0(R1),X'FF'                                                      
         MVI   8(R1),0                                                          
         B     EXIT                                                             
*                                                                               
DMXKEEP  L     R1,APARM            KEEP RECORD EXIT                             
         MVI   0(R1),0                                                          
         MVI   8(R1),0                                                          
         B     EXIT                                                             
*                                                                               
DMXKERET L     R1,APARM            KEEP RECORD AND RETURN TO ME                 
         MVI   0(R1),0                                                          
         MVI   8(R1),C'R'                                                       
         B     EXIT                                                             
*                                                                               
DMXPGRET L     R1,APARM            PURGE RECORD AND RETURN TO ME                
         MVI   0(R1),X'FF'                                                      
         MVI   8(R1),C'R'                                                       
         B     EXIT                                                             
*                                                                               
DMXPGEOF L     R1,APARM            PURGE AND CAUSE INPUT EOF EXIT               
         MVI   0(R1),X'FF'                                                      
         MVC   1(3,R1),=C'EOF'                                                  
         B     EXIT                                                             
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* WORK AREA                                                           *         
***********************************************************************         
                                                                                
ACRECTYP DC    V(ACRECTYP)                                                      
DATCON   DC    V(DATCON)                                                        
PRINT    DC    V(PRINT)                                                         
PRNTBL   DC    V(PRNTBL)                                                        
*                                                                               
APARM    DS    A                                                                
PLIST    DS    0CL24                                                            
AREC     DS    A                                                                
VTAPEOUT DS    A                                                                
APARAMC  DS    A                                                                
VLDDEFN  DS    A                                                                
VPRINTER DS    A                   VPRINTER                                     
VCPRINT  DS    A                   VCPRINT                                      
*                                                                               
DUB      DS    D                                                                
FULL     DS    F                                                                
DMCB     DS    6F                                                               
WORK     DS    CL20                                                             
BYTE     DS    C                                                                
HALF     DS    H                                                                
FLAG     DS    C                                                                
*                                                                               
COMPDSP  DS    XL1                 SAVE DISPLACEMENT TO COMPANY                 
RECTYP   DS    XL1                 RECORD TYPE                                  
CURCOMP  DS    XL1                 CURRENT COMPANY                              
NEWDATE  DS    CL6                                                              
*                                                                               
HI       DS    0X                                                               
HICH6    DS    CL6                 YYMMDD                                       
HICH8    DS    CL8                 MM/DD/YY                                     
HIPW3    DS    XL3                 PWO                                          
HIPC3    DS    XL3                 PACKED COMPLEMENT                            
HITC3    DS    XL3                 TWO'S COMPLEMENT                             
HIPW2    DS    XL2                 PWO                                          
HIPC2    DS    XL2                 PACKED COMPLEMENT                            
HICO2    DS    XL2                 COMPRESSED                                   
HICC2    DS    XL2                 COMPRESSED COMPLEMENT                        
*                                                                               
LO       DS    0X                                                               
LOCH6    DS    CL6                 YYMMDD                                       
LOCH8    DS    CL8                 MM/DD/YY                                     
LOPW3    DS    XL3                 PWO                                          
LOPC3    DS    XL3                 PACKED COMPLEMENT                            
LOTC3    DS    XL3                 TWO'S COMPLEMENT                             
LOPW2    DS    XL2                 PWO                                          
LOPC2    DS    XL2                 PACKED COMPLEMENT                            
LOCO2    DS    XL2                 COMPRESSED                                   
LOCC2    DS    XL2                 COMPRESSED COMPLEMENT                        
*                                                                               
TRACE    DS    0XL300                                                           
TDATE    DS    CL6                                                              
         DS    XL2                                                              
TTABL    DS    XL3                                                              
         DS    XL1                                                              
TDATA    DS    XL255                                                            
*                                                                               
APAGE    DC    A(EPAGE1)                                                        
         DC    A(EPAGE2)                                                        
         DC    A(EPAGE3)                                                        
         DC    A(EPAGE4)                                                        
         DC    A(EPAGE5)                                                        
         DC    A(EPAGE6)                                                        
         DC    A(EPAGE7)                                                        
         DC    A(EPAGEX)                                                        
*                                                                               
BINZ     DC    X'0000000000000000'                                              
FFS      DC    X'FFFFFFFFFFFFFFFF'                                              
*                                                                               
COUNT    DC    PL4'0'                                                           
MXCNT    DC    P'100'                                                           
*                                                                               
RECCNT   DC    256X'00'                                                         
ELMCNT   DC    256X'00'                                                         
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* RECORD TABLES                                                       *         
***********************************************************************         
                                                                                
CH6      EQU   1                   6 BYTE  - CHARACTER 6 YYMMDD                 
CH8      EQU   2                   8 BYTE  - CHARACTER 8 MM/DD/YY               
PW3      EQU   3                   3 BYTES - PWO                                
PC3      EQU   4                   3 BYTE  - PACKED COMPLEMENT                  
TC3      EQU   5                   3 BYTE  - TWO'S COMPLEMENT                   
PW2      EQU   7                   3 BYTES - PWO                                
PC2      EQU   7                   2 BYTE  - PACKED COMPLEMENT                  
CO2      EQU   8                   2 BYTES - COMPRESSED                         
CC2      EQU   9                   2 BYTES - COMPRESSED COMPLEMENT              
*                                                                               
RECT     DS    0XL3                                                             
* TBAKADDT DS    XL2                 DATE ADDED (COMPRESSED COMPLEMENT)         
         DC    AL1(ACRTNBT),AL1(TBAKADDT-TBARECD),AL1(CC2)                      
* TAXKDATE DS    PL3                 COMPLEMENT OF PWOS START DATE              
         DC    AL1(ACRTTAX),AL1(TAXKDATE-TAXRECD),AL1(PC3)                      
* BATKDATE DS    XL3                 BATCH DATE                                 
         DC    AL1(ACRTBAT),AL1(BATKDATE-BATRECD),AL1(PW3)                      
* RAPKDATE DS    XL2                 ACTIVITY DATE (COMPRESSED)                 
         DC    AL1(ACRTRAP),AL1(RAPKDATE-RAPRECD),AL1(CO2)                      
* AJNKEFF  DS    XL3                 COMPLEMENT OF PWOS EFFECTIVE DATE          
         DC    AL1(ACRTRAP),AL1(RAPKDATE-RAPRECD),AL1(PC2)                      
* PRCKEFF  DS    XL3                 EFFECTIVE DATE                             
         DC    AL1(ACRTPRC),AL1(PRCKEFF-PRCRECD),AL1(PC3)                       
* GRBKBILD DS    PL3                 BILL DATE (COMPLEMENT PWO)                 
         DC    AL1(ACRTGRB),AL1(GRBKBILD-GRBRECD),AL1(PC3)                      
* TSWKEND  DS    PL3                 WEEK ENDING DATE(2'S COMPLEMENT)           
         DC    AL1(ACRTTSW),AL1(TSWKEND-TSWRECD),AL1(TC3)                       
* TSSKEND  DS    PL3                 WEEK ENDING DATE(2'S COMPLEMENT)           
         DC    AL1(ACRTSSAV),AL1(TSSKEND-TSSRECD),AL1(TC3)                      
* TSXKEND  DS    PL3                 WEEK ENDING DATE YYMMDD                    
         DC    AL1(ACRTTPOX),AL1(TSXKEND-TSXRECD),AL1(PW3)                      
* SRMKDTE  DS    PL3                 DATE                                       
SRMR     DC    AL1(ACRTSRM),AL1(SRMKDTE-SRMRECD),AL1(PW3)                       
* SRFKDTE  DS    PL3                 DATE                                       
SRFR     DC    AL1(ACRTSRM),AL1(SRFKDTE-SRFRECD),AL1(PW3)                       
* TRNKDATE DS    PL3                 TRANSACTION DATE                           
         DC    AL1(ACRTTRN),AL1(TRNKDATE-TRNRECD),AL1(PW3)                      
* TIMKPEDT DS    PL3                 PERIOD ENDING DATE                         
         DC    AL1(ACRTTIM),AL1(TIMKPEDT-TIMRECD),AL1(PW3)                      
*                                                                               
         DC    X'FF'                                                            
         EJECT                                                                  
***********************************************************************         
* ELMENT TABLES                                                       *         
***********************************************************************         
                                                                                
ELMT     DS    0XL3                                                             
EPAGE1   DS    0C                                                               
* FHDLDAT  DS    CL8                 DATE LAST LOADED DD/MM/YY                  
         DC    AL1(FHDELQ),AL1(FHDLDAT-FHDELD),AL1(CH8)                         
* FHDLOAD  DS    XL2                 LAST FILE LOAD DATE                        
         DC    AL1(FHDELQ),AL1(FHDLOAD-FHDELD),AL1(CO2)                         
* FHDUPDT  DS    XL2                 LAST UPDATE RUN DATE                       
         DC    AL1(FHDELQ),AL1(FHDUPDT-FHDELD),AL1(CO2)                         
* FHDGOOD  DS    XL2                 LAST SUCCESSFUL UPDATE DATE                
         DC    AL1(FHDELQ),AL1(FHDGOOD-FHDELD),AL1(CO2)                         
* HDRDUMP  DS    CL6                 DATE FILE LAST DUMPED                      
         DC    AL1(HDRELQ),AL1(HDRDUMP-HDRELD),AL1(CH6)                         
* HDRLOAD  DS    XL2                 LAST FILE LOAD DATE                        
         DC    AL1(HDRELQ),AL1(HDRLOAD-HDRELD),AL1(CO2)                         
* HDRUPDT  DS    XL2                 LAST UPDATE RUN DATE                       
         DC    AL1(HDRELQ),AL1(HDRUPDT-HDRELD),AL1(CO2)                         
* HDRGOOD  DS    XL2                 LAST SUCCESSFUL UPDATE DATE                
         DC    AL1(HDRELQ),AL1(HDRGOOD-HDRELD),AL1(CO2)                         
* CPYTMSSD DS    XL2                 TMS START DATE (COMPRESSED)                
         DC    AL1(CPYELQ),AL1(CPYTMSSD-CPYELD),AL1(CO2)                        
* MDTDTE   DS    XL2                 TRANSFER DATE (COMPRESSED)                 
         DC    AL1(MDTELQ),AL1(MDTDTE-MDTELD),AL1(CO2)                          
* LITEDAT  DS    PL3                 EXPIRY DATE (PWOS)                         
         DC    AL1(LITELQ),AL1(LITEDAT-LITELD),AL1(CO2)                         
* JOBCDATE DS    PL3                 ESTIMATED CLOSING DATE                     
         DC    AL1(JOBELQ),AL1(JOBCDATE-JOBELD),AL1(PW3)                        
* JOBADATE DS    PL3                 DATE ADDED TO FILE                         
         DC    AL1(JOBELQ),AL1(JOBADATE-JOBELD),AL1(PW3)                        
* JOBODATE DS    PL3                 JOB OPENING DATE                           
         DC    AL1(JOBELQ),AL1(JOBODATE-JOBELD),AL1(PW3)                        
* JOBRDATE DS    PL3                 REVISION DATE                              
         DC    AL1(JOBELQ),AL1(JOBRDATE-JOBELD),AL1(PW3)                        
* MBTCHNG  DS    XL3                 LAST CHANGE DATE                           
         DC    AL1(MBTELQ),AL1(MBTCHNG-MBTELD),AL1(PW3)                         
* MTPFCHDT DS    XL3                 LAST CHANGE DATE                           
         DC    AL1(MTPELQ),AL1(MTPFCHDT-MTPELD),AL1(PW3)                        
* RSTPBILL DS    XL2                 ON PRODN. LAST BILLED DATE FOR JOB         
         DC    AL1(RSTELQ),AL1(RSTPBILL-RSTELD),AL1(CO2)                        
* RSTBDATE DS    PL3                 DATE BALANCE BROUGHT FORWARD               
         DC    AL1(RSTELQ),AL1(RSTBDATE-RSTELD),AL1(PW3)                        
* RSTTDATE DS    PL3                 DATE LAST TRANSACTION POSTED               
         DC    AL1(RSTELQ),AL1(RSTTDATE-RSTELD),AL1(PW3)                        
* APOPLDT  DS    PL3                 DATE OF THIS PEEL-OFF                      
         DC    AL1(APOELQ),AL1(APOPLDT-APOELD),AL1(PW3)                         
* APOLBDT  DS    PL3                 DATE OF LAST BAL B/FRWD                    
         DC    AL1(APOELQ),AL1(APOLBDT-APOELD),AL1(PW3)                         
* BUDSTRT  DS    PL2                 BUDGET START DATE (PWOS)                   
         DC    AL1(BUDELQ),AL1(BUDSTRT-BUDELD),AL1(PW3)                         
* BUDEND   DS    PL2                 BUDGET END DATE   (PWOS)                   
         DC    AL1(BUDELQ),AL1(BUDEND-BUDELD),AL1(PW3)                          
* CEXDTE   DS    PL3                 EFFECTIVE DATE                             
         DC    AL1(CEXELQ),AL1(CEXDTE-CEXELD),AL1(PW3)                          
* WPBDATE  DS    PL3                 BILLING DATE                               
         DC    AL1(WPBELQ),AL1(WPBDATE-WPBELD),AL1(PW3)                         
* XPRDUE   DS    PL2                NUMBER OF DAYS TO WORK OUT DUE DATE         
         DC    AL1(XPRELQ),AL1(XPRDUE-XPRELD),AL1(PW3)                          
         DC    X'FF'                                                            
         EJECT                                                                  
EPAGE2   DS    0C                                                               
* PRTSTRT  DS    PL3                 HOURLY RATE EFFECTIVE START DATE           
         DC    AL1(PRTELQ),AL1(PRTSTRT-PRTELD),AL1(PW3)                         
* TRNDATE  DS    PL3                 TRANSACTION DATE (PWOS YMD)                
         DC    AL1(TRNELQ),AL1(TRNDATE-TRNELD),AL1(PW3)                         
* TRN2DAY  DS    XL2                 TODAY'S DATE                               
         DC    AL1(TRNELQ),AL1(TRN2DAY-TRNELD),AL1(CO2)                         
* TRNUNBIL DS    XL2                 UNBILLED DATE                              
         DC    AL1(TRNELQ),AL1(TRNUNBIL-TRNELD),AL1(CO2)                        
* TRNSK2SI DS    XL2                 DATE SK POSTED TO SI                       
         DC    AL1(TRNELQ),AL1(TRNSK2SI-TRNELD),AL1(CO2)                        
* MXPIDAT  DS    PL3                 INSERTION/TRANSMISSION DATE (YMD)          
         DC    AL1(MXPELQ),AL1(MXPIDAT-MXPELD),AL1(PW3)                         
* MXPADAT  DS    PL3                 ACTIVITY DATE (YMD)                        
         DC    AL1(MXPELQ),AL1(MXPADAT-MXPELD),AL1(PW3)                         
* VBIDATE  DS    PL3                 VAT EFFECTIVE DATE                         
         DC    AL1(VBIELQ),AL1(VBIDATE-VBIELD),AL1(PW3)                         
* RBIDAT   DS    XL2                 ORIGINAL BILLING DATE (COMPRESSED)         
         DC    AL1(RBIELQ),AL1(RBIDAT-RBIELD),AL1(CO2)                          
* RBIUND   DS    XL2                 REVERSE BILLING DATE  (COMPRESSED)         
         DC    AL1(RBIELQ),AL1(RBIUND-RBIELD),AL1(CO2)                          
* XBIDAT   DS    PL3                 INVOICE DATE (PACKED YMD)                  
         DC    AL1(XBIELQ),AL1(XBIDAT-XBIELD),AL1(PW3)                          
* BNDDTE   DS    XL2                 BILL DATE (2-BYTE COMPRESSED)              
         DC    AL1(BNDELQ),AL1(BNDDTE-BNDELD),AL1(CO2)                          
* BNDRUN   DS    XL2                 RUN DATE                                   
         DC    AL1(BNDELQ),AL1(BNDRUN-BNDELD),AL1(CO2)                          
* PXDDATE  DS    CL3                 TRANSFER DATE                              
         DC    AL1(PXDELQ),AL1(PXDDATE-PXDELD),AL1(PW3)                         
* MSABEG   DS    PL2                 START DATE                                 
         DC    AL1(MSAELQ),AL1(MSABEG-MSAELD),AL1(CO2)                          
* MSAEND   DS    PL2                 END DATE                                   
         DC    AL1(MSAELQ),AL1(MSAEND-MSAELD),AL1(CO2)                          
* TCIDTE   DS    PL3                 EFFECTIVE DATE (PWOS)                      
         DC    AL1(TCIELQ),AL1(TCIDTE-TCIELD),AL1(PW3)                          
* OCNDPSR  DS    XL2                 DATE OF PENDING SOON REGISTER              
         DC    AL1(OCNELQ),AL1(OCNDPSR-OCNELD),AL1(CO2)                         
* OCNDPLR  DS    XL2                 DATE OF PENDING LOCAL REGISTER             
         DC    AL1(OCNELQ),AL1(OCNDPLR-OCNELD),AL1(CO2)                         
* EMPHIR   DS    PL3                 HIRE DATE PACKED YMD                       
         DC    AL1(EMPELQ),AL1(EMPHIR-EMPELD),AL1(PW3)                          
* EMPTRM   DS    PL3                 TERMINATION DATE PACKED YMD                
         DC    AL1(EMPELQ),AL1(EMPTRM-EMPELD),AL1(PW3)                          
* EMPLOCK  DS    PL3                 LOCK TIMESHEET DATE                        
         DC    AL1(EMPELQ),AL1(EMPLOCK-EMPELD),AL1(PW3)                         
* EMPSALKD DS    PL3                 SALARY LOCKED DATE                         
         DC    AL1(EMPELQ),AL1(EMPSALKD-EMPELD),AL1(PW3)                        
* MFCDATE  DS    CL3                 PWOS LATEST RE-VALUATION DATE              
         DC    AL1(MFCELQ),AL1(MFCDATE-MFCELD),AL1(PW3)                         
* CUBDTE   DS    XL2                 BILLED DATE                                
         DC    AL1(CUBELQ),AL1(CUBDTE-CUBELD),AL1(CO2)                          
* CUBUNDTE DS    XL2                 UNBILLED DATE                              
         DC    AL1(CUBELQ),AL1(CUBUNDTE-CUBELD),AL1(CO2)                        
* CUBRUN   DS    XL2                 RUN DATE OF BILLING                        
         DC    AL1(CUBELQ),AL1(CUBRUN-CUBELD),AL1(CO2)                          
* CALPCST  DS    XL2                 COMPRESSED START DATE                      
         DC    AL1(CALELQ),AL1(CALPCST-CALELD),AL1(CO2)                         
* CALPCND  DS    XL2                 COMPRESSED END DATE                        
         DC    AL1(CALELQ),AL1(CALPCND-CALELD),AL1(CO2)                         
* SUTEFF   DS    PL3                 EFFECTIVE DATE                             
         DC    AL1(SUTELQ),AL1(SUTEFF-SUTELD),AL1(PW3)                          
         DC    X'FF'                                                            
         EJECT                                                                  
EPAGE3   DS    0C                                                               
* TRSDATE  DS    XL2                 ACTIVITY DATE                              
         DC    AL1(TRSELQ),AL1(TRSDATE-TRSELD),AL1(CO2)                         
* TRSREVD  DS    XL2                 DATE ITEM MARKED AS REVERSAL               
         DC    AL1(TRSELQ),AL1(TRSREVD-TRSELD),AL1(CO2)                         
* TRSUPDT  DS    XL2                 DATE ITEM MARKED BY GLU                    
         DC    AL1(TRSELQ),AL1(TRSUPDT-TRSELD),AL1(CO2)                         
* TRSUDAT  DS    XL2                 USED DATE (COMPRESSED)                     
         DC    AL1(TRSELQ),AL1(TRSUDAT-TRSELD),AL1(CO2)                         
* TRSPDAT  DS    XL2                 PEEL DATE (COMPRESSED)                     
         DC    AL1(TRSELQ),AL1(TRSPDAT-TRSELD),AL1(CO2)                         
* TRSBSTDT DS    XL2                 BANK STATEMENT DATE (CASH LEDGER)          
         DC    AL1(TRSELQ),AL1(TRSBSTDT-TRSELD),AL1(CO2)                        
* TRSEFDT  DS    XL2                 TRANSACTION EFFECTIVE (LIVE) DATE          
         DC    AL1(TRSELQ),AL1(TRSEFDT-TRSELD),AL1(CO2)                         
* DUEDATE  DS    XL2                 DUE DATE FOR RECEIVABLES                   
         DC    AL1(DUEELQ),AL1(DUEDATE-DUEELD),AL1(CO2)                         
* MPYDTE   DS    XL2                 DATE OF CHECK                              
         DC    AL1(MPYELQ),AL1(MPYDTE-MPYELD),AL1(CO2)                          
* ORDDATE  DS    PL3                 ORDER DATE                                 
         DC    AL1(ORDELQ),AL1(ORDDATE-ORDELD),AL1(PW3)                         
* ORDAMDT  DS    PL3                 LATEST AMENDMENT DATE                      
         DC    AL1(ORDELQ),AL1(ORDAMDT-ORDELD),AL1(PW3)                         
*&&UK                                                                           
* ORDDELDT DS    XL2                 'DELETE DATE' SET BY ACPC                  
         DC    AL1(ORDELQ),AL1(ORDDELDT-ORDELD),AL1(CO2)                        
*&&                                                                             
* ORDDDTE  DS    PL3                 DUE DATE                                   
         DC    AL1(ORDELQ),AL1(ORDDDTE-ORDELD),AL1(PW3)                         
* OAMLAST  DS    PL3                 LAST ACTIVITY DATE                         
         DC    AL1(OAMELQ),AL1(OAMLAST-OAMELD),AL1(PW3)                         
* FAREFF   DS    XL2                 EFFECTIVE DATE (OPTIONAL)                  
         DC    AL1(FARELQ),AL1(FAREFF-FARELD),AL1(PW3)                          
* ADSPD    DS    XL2                 PURCHASE DATE (COMPRESSED)                 
         DC    AL1(ADSELQ),AL1(ADSPD-ADSELD),AL1(CO2)                           
* ADSBDTE  DS    XL2                 BOOK 'AS AT' DATE                          
         DC    AL1(ADSELQ),AL1(ADSBDTE-ADSELD),AL1(CO2)                         
* ADSTDTE  DS    XL2                 TAX 'AS AT' DATE                           
         DC    AL1(ADSELQ),AL1(ADSTDTE-ADSELD),AL1(CO2)                         
* ADIDATE  DS    XL2                 DISPOSAL DATE                              
         DC    AL1(ADIELQ),AL1(ADIDATE-ADIELD),AL1(CO2)                         
* RTEDATE  DS    XL2      )           AS OF DATE                                
         DC    AL1(RTEELQ),AL1(RTEDATE-RTEELD),AL1(CO2)                         
         DC    X'FF'                                                            
         EJECT                                                                  
EPAGE4   DS    0C                                                               
* MRHDAT   DS    XL2                 INSERTION/TRANSMISSION DATE                
         DC    AL1(MRHELQ),AL1(MRHDAT-MRHELD),AL1(CO2)                          
* SIDDAT   DS    XL3                 DATE                                       
         DC    AL1(SIDELQ),AL1(SIDDAT-SIDELD),AL1(PW3)                          
* MRXDAT   DS    PL3                 DATE                                       
         DC    AL1(MRXELQ),AL1(MRXDAT-MRXELD),AL1(PW3)                          
* MRXISSD  DS    XL2                 DATE VERSCHILLEN NOTE PRINTED              
         DC    AL1(MRXELQ),AL1(MRXISSD-MRXELD),AL1(CO2)                         
* NOTDATE  DS    XL2                 DATE                                       
         DC    AL1(NOTELQ),AL1(NOTDATE-NOTELD),AL1(CO2)                         
* PTADATE  DS    XL2                 ACTIVITY DATE                              
         DC    AL1(PTAELQ),AL1(PTADATE-PTAELD),AL1(CO2)                         
* PTARDATE DS    XL2                 BILLING DATE                               
         DC    AL1(PTAELQ),AL1(PTARDATE-PTAELD),AL1(CO2)                        
* PTARBLDT DS    XL2                 BILL DATE                                  
         DC    AL1(PTAELQ),AL1(PTARBLDT-PTAELD),AL1(CO2)                        
* PTARORGD DS    XL2                 ORIGINAL BILL DATE   (REVERSAL)            
         DC    AL1(PTAELQ),AL1(PTARORGD-PTAELD),AL1(CO2)                        
* PTARUNBD DS    XL2                 UNBILL RUN DATE (REVERSED)                 
         DC    AL1(PTAELQ),AL1(PTARUNBD-PTAELD),AL1(CO2)                        
* PTAWDAT  DS    PL3                 TRANSACTION DATE                           
         DC    AL1(PTAELQ),AL1(PTAWDAT-PTAELD),AL1(PW3)                         
* BLHCRED  DS    XL2                 CREATED DATE                               
         DC    AL1(BLHELQ),AL1(BLHCRED-BLHELD),AL1(CO2)                         
* BLHBILD  DS    XL2                 BILLED DATE (ZERO=DRAFT BILL)              
         DC    AL1(BLHELQ),AL1(BLHBILD-BLHELD),AL1(CO2)                         
* BLHEXPD  DS    XL2                 EXPIRY DATE                                
         DC    AL1(BLHELQ),AL1(BLHEXPD-BLHELD),AL1(CO2)                         
* BLHTRND  DS    XL2                 TRANSACTION DATE (PRINTED ON BILL)         
         DC    AL1(BLHELQ),AL1(BLHTRND-BLHELD),AL1(CO2)                         
* BLHDUED  DS    XL2                 DUE DATE    (FOR DEBTORS POSTING)          
         DC    AL1(BLHELQ),AL1(BLHDUED-BLHELD),AL1(CO2)                         
* BLHPDATE DS    XL2                 DATE BILL LAST SENT TO PRINT QUEUE         
         DC    AL1(BLHELQ),AL1(BLHPDATE-BLHELD),AL1(CO2)                        
* UNPSTRT  DS    PL3                 EFFECTIVE START DATE                       
         DC    AL1(UNPELQ),AL1(UNPSTRT-UNPELD),AL1(PW3)                         
* PBIDATE  DS    PL3                 PST EFFECTIVE DATE                         
         DC    AL1(PBIELQ),AL1(PBIDATE-PBIELD),AL1(PW3)                         
* DOADTE   DS    XL2                 DATE OF ALLOCATION (COMPRESSED)            
         DC    AL1(DOAELQ),AL1(DOADTE-DOAELD),AL1(CO2)                          
* LOCSTART DS    PL3                 DATE THIS LOCATION STARTED                 
         DC    AL1(LOCELQ),AL1(LOCSTART-LOCELD),AL1(PW3)                        
* LOCEND   DS    PL3                 DATE THIS LOCATION ENDED                   
         DC    AL1(LOCELQ),AL1(LOCEND-LOCELD),AL1(PW3)                          
* LOCLOCK  DS    PL3                 TIMESHEET LOCKED DATE                      
         DC    AL1(LOCELQ),AL1(LOCLOCK-LOCELD),AL1(PW3)                         
* LOCSALKD DS    PL3                 SALARY LOCKED DATE                         
         DC    AL1(LOCELQ),AL1(LOCSALKD-LOCELD),AL1(PW3)                        
         DC    X'FF'                                                            
         EJECT                                                                  
EPAGE5   DS    0C                                                               
* PDEDTE   DS    XL3                 DATE (PWOS)                                
         DC    AL1(PDEELQ),AL1(PDEDTE-PDEELD),AL1(PW3)                          
* TMRSTART DS    XL3                 PACKED YEAR STARTING DATE                  
         DC    AL1(TMRELQ),AL1(TMRSTART-TMRELD),AL1(PW3)                        
* TMREND   DS    XL3                 PACKED YEAR ENDING DATE                    
         DC    AL1(TMRELQ),AL1(TMREND-TMRELD),AL1(PW3)                          
* TMPSTART DS    XL3                 PACKED PERIOD STARTING DATE                
         DC    AL1(TMPELQ),AL1(TMPSTART-TMPELD),AL1(PW3)                        
* TMPEND   DS    XL3                 PACKED PERIOD ENDING DATE                  
         DC    AL1(TMPELQ),AL1(TMPEND-TMPELD),AL1(PW3)                          
* SHRSTART DS    PL3                 START DATE FOR HOURS                       
         DC    AL1(SHRELQ),AL1(SHRSTART-SHRELD),AL1(PW3)                        
* SHREND   DS    PL3                 END DATE FOR HOURS                         
         DC    AL1(SHRELQ),AL1(SHREND-SHRELD),AL1(PW3)                          
* TIMADAT  DS    PL3                 ACTIVITY DATE                              
         DC    AL1(TIMELQ),AL1(TIMADAT-TIMELD),AL1(PW3)                         
* TIMREFF  DS    PL3                 RATE EFFECTIVE DATE                        
         DC    AL1(TIMELQ),AL1(TIMREFF-TIMELD),AL1(PW3)                         
*&&UK                                                                           
* TIMCREFF DS    PL3                 COST RATE EFFECTIVE DATE                   
         DC    AL1(TIMELQ),AL1(TIMCREFF-TIMELD),AL1(PW3)                        
*&&                                                                             
* TIMTEFF  DS    PL3                 TAX EFFECTIVE DATE                         
         DC    AL1(TIMELQ),AL1(TIMTEFF-TIMELD),AL1(PW3)                         
* TIMXPEDT DS    PL3                 ACTUAL PERIOD END DATE                     
         DC    AL1(TIMELQ),AL1(TIMXPEDT-TIMELD),AL1(PW3)                        
* TIMXPSDT DS    PL3                 ACTUAL PERIOD START DATE                   
         DC    AL1(TIMELQ),AL1(TIMXPSDT-TIMELD),AL1(PW3)                        
* PTHEDT   DS    PL3                 PERIOD ENDING DATE                         
         DC    AL1(PTHELQ),AL1(PTHEDT-PTHELD),AL1(PW3)                          
* TSCDATE  DS    XL2                 DATE OF CHANGE(COMPRESSED)                 
         DC    AL1(TSCELQ),AL1(TSCDATE-TSCELD),AL1(CO2)                         
* VPDACT   DS    XL2                 ACTIVITY DATE (COMPRESSED)                 
         DC    AL1(VPDELQ),AL1(VPDACT-VPDELD),AL1(CO2)                          
* VPDUPDT  DS    XL2                 DATE ITEM MARKED BY GLU                    
         DC    AL1(VPDELQ),AL1(VPDUPDT-VPDELD),AL1(CO2)                         
* VPDVOID  DS    XL2                 DATE ITEM VOIDED                           
         DC    AL1(VPDELQ),AL1(VPDVOID-VPDELD),AL1(CO2)                         
* VPDDTE   DS    XL2                 DATE OF CHECK                              
         DC    AL1(VPDELQ),AL1(VPDDTE-VPDELD),AL1(CO2)                          
* CNTDELDT DS    PL3                 LAST DATE OF DELIVERY - MEMO BILL          
         DC    AL1(CNTELQ),AL1(CNTDELDT-CNTELD),AL1(PW3)                        
* CPOLAST  DS    PL3                 LAST ACTIVITY DATE                         
         DC    AL1(CPOELQ),AL1(CPOLAST-CPOELD),AL1(PW3)                         
* PACDATE  DS    PL3                 LAST ACTIVITY DATE                         
         DC    AL1(PACELQ),AL1(PACDATE-PACELD),AL1(PW3)                         
* UFSCUT   DS    PL3                 CUTOFF DATE                                
         DC    AL1(UFSELQ),AL1(UFSCUT-UFSELD),AL1(PW3)                          
* OPDLAST  DS    PL3                 DATE OPTION WAS CHANGED                    
         DC    AL1(OPDELQ),AL1(OPDLAST-OPDELD),AL1(PW3)                         
* EUPADD   DS    PL3                 DATE ADDED                                 
         DC    AL1(EUPELQ),AL1(EUPADD-EUPELD),AL1(PW3)                          
* EUPLAST  DS    PL3                 DATE LAST CHANGED                          
         DC    AL1(EUPELQ),AL1(EUPLAST-EUPELD),AL1(PW3)                         
* EAPINP   DS    PL3                 DATE INPUT                                 
         DC    AL1(EAPELQ),AL1(EAPINP-EAPELD),AL1(PW3)                          
* EAPDATE  DS    PL3                 APPROVAL DATE                              
         DC    AL1(EAPELQ),AL1(EAPDATE-EAPELD),AL1(PW3)                         
* PPAADD   DS    PL3                 DATE ADDED                                 
         DC    AL1(PPAELQ),AL1(PPAADD-PPAELD),AL1(PW3)                          
* PPALAST  DS    PL3                 DATE LAST CHANGED                          
         DC    AL1(PPAELQ),AL1(PPALAST-PPAELD),AL1(PW3)                         
         DC    X'FF'                                                            
         EJECT                                                                  
EPAGE6   DS    0C                                                               
* EPRINP   DS    PL3                 DATE INPUT                                 
         DC    AL1(EPRELQ),AL1(EPRINP-EPRELD),AL1(PW3)                          
* EPRDATE  DS    PL3                 PREPARED DATE                              
         DC    AL1(EPRELQ),AL1(EPRDATE-EPRELD),AL1(PW3)                         
* BDAFDTE  DS    PL2                 FROM DATE (YM)                             
         DC    AL1(BDAELQ),AL1(BDAFDTE-BDAELD),AL1(PW2)                         
* BDATDTE  DS    PL2                 TO DATE (YM)                               
         DC    AL1(BDAELQ),AL1(BDATDTE-BDAELD),AL1(PW2)                         
* BDARUN   DS    PL3                 RUN DATE (PWO)                             
         DC    AL1(BDAELQ),AL1(BDARUN-BDAELD),AL1(PW3)                          
* BDADUE   DS    PL3                 DUE DATE (PWO)                             
         DC    AL1(BDAELQ),AL1(BDADUE-BDAELD),AL1(PW3)                          
* BDAUNDT  DS    PL3                 UNBILL DATE (PWO)                          
         DC    AL1(BDAELQ),AL1(BDAUNDT-BDAELD),AL1(PW3)                         
* BCYBILD  DS    XL2                 CYCLE BILLED DATE(COMPRESSED)              
         DC    AL1(BCYELQ),AL1(BCYBILD-BCYELD),AL1(CO2)                         
* BCYUNBD  DS    XL2                 CYCLE UNBILLED DATE(COMPRESSED)            
         DC    AL1(BCYELQ),AL1(BCYUNBD-BCYELD),AL1(CO2)                         
* EPTRDATE DS    PL3                 EFFECTIVE DATE FOR RATE OR ZEROES          
         DC    AL1(EPTELQ),AL1(EPTRDATE-EPTELD),AL1(PW3)                        
* XTHSTDTE DS    PL3                 START DATE                                 
         DC    AL1(XTHELQ),AL1(XTHSTDTE-XTHELD),AL1(PW3)                        
* XTHEND   DS    PL3                 END   DATE                                 
         DC    AL1(XTHELQ),AL1(XTHEND-XTHELD),AL1(PW3)                          
* XTHSDTE  DS    PL3                 SUBMIT  DATE                               
         DC    AL1(XTHELQ),AL1(XTHSDTE-XTHELD),AL1(PW3)                         
* XTHADTE  DS    PL3                 APPROVE DATE                               
         DC    AL1(XTHELQ),AL1(XTHADTE-XTHELD),AL1(PW3)                         
* XTHUDTE  DS    PL3                 UPLOAD  DATE                               
         DC    AL1(XTHELQ),AL1(XTHUDTE-XTHELD),AL1(PW3)                         
* RCLSTDT  DS    XL2                 BINARY NUMBER FOR START DATE RANGE         
         DC    AL1(RCLELQ),AL1(RCLSTDT-RCLELD),AL1(CO2)                         
* RCLENDT  DS    XL2                 BINARY NUMBER FOR END DATE RANGE           
         DC    AL1(RCLELQ),AL1(RCLENDT-RCLELD),AL1(CO2)                         
* IESDAT   DS    XL2                 POSTING DATE (YMD) COMPRESSED              
         DC    AL1(IESELQ),AL1(IESDAT-IESELD),AL1(CO2)                          
* IESDTO   DS    XL2                 ORIGINAL POSTING DATE (COMPRESSED)         
         DC    AL1(IESELQ),AL1(IESDTO-IESELD),AL1(CO2)                          
* IESUSED  DS    XL2                 USED DATE - COMPRESSED                     
         DC    AL1(IESELQ),AL1(IESUSED-IESELD),AL1(CO2)                         
* JCBXDAT  DS    XL2                 DATE EXCHANGE RATE SET                     
         DC    AL1(JCBELQ),AL1(JCBXDAT-JCBELD),AL1(CO2)                         
* XXPDSCHM DS    CL2                 DATE SCHEME OR BINARY ZEROES               
         DC    AL1(XXPELQ),AL1(XXPDSCHM-XXPELD),AL1(CO2)                        
* DATDONE  DS    PL3                 PROJECT COMPLETION DATE (YYMMDD)           
         DC    AL1(DATELQ),AL1(DATDONE-DATELD),AL1(PW3)                         
         DC    X'FF'                                                            
         EJECT                                                                  
EPAGE7   DS    0C                                                               
* BHDDELDT DS    XL2                 DATE BATCH DELETED                         
*        DC    AL1(BHDELQ),AL1(BHDDELDT-BHDELD),AL1(CO2)                        
* ITCEFFD  DS    PL3                 EFFECTIVE DATE                             
         DC    AL1(ITCELQ),AL1(ITCEFFD-ITCELD),AL1(PW3)                         
* GDADATE  DS    PL3                 DATE                                       
         DC    AL1(GDAELQ),AL1(GDADATE-GDAELD),AL1(PW3)                         
* GDADATE2 DS    PL3                 DATE                                       
         DC    AL1(GDAELQ),AL1(GDADATE2-GDAELD),AL1(PW3)                        
* BICDAT   DS    PL3                 CHECK DATE                                 
         DC    AL1(BICELQ),AL1(BICDAT-BICELD),AL1(PW3)                          
* BICDEP   DS    PL3                 DEPOSIT DATE                               
         DC    AL1(BICELQ),AL1(BICDEP-BICELD),AL1(PW3)                          
* LGLDATE  DS    XL3                 DATE LAST LOCKED                           
         DC    AL1(LGLELQ),AL1(LGLDATE-LGLELD),AL1(PW3)                         
* LOKDATE  DS    XL3                 DATE LAST LOCKED                           
         DC    AL1(LOKELQ),AL1(LOKDATE-LOKELD),AL1(PW3)                         
* HLDDATE  DS    XL2                 DATE HOLD STATUS LAST CHANGED              
         DC    AL1(HLDELQ),AL1(HLDDATE-HLDELD),AL1(CO2)                         
* MBITDAT  DS    XL2                DATE ACCOUNT ORIGINALLY TRANSFERRED         
         DC    AL1(MBIELQ),AL1(MBITDAT-MBIELD),AL1(CO2)                         
* MBIRDAT  DS    XL2                 DATE ACCOUNT REVERSED BY THE MY            
         DC    AL1(MBIELQ),AL1(MBIRDAT-MBIELD),AL1(CO2)                         
* SPYCPYDT DS    XL2                 PAYMENT DATE                               
         DC    AL1(SPYELQ),AL1(SPYCPYDT-SPYELD),AL1(CO2)                        
* SPYBVUDT DS    XL2                 VOID/UNVOID ACTIVITY DATE                  
         DC    AL1(SPYELQ),AL1(SPYBVUDT-SPYELD),AL1(CO2)                        
* DTSDATE  DS    XL2                 DATE LAST CHANGED (COMPRESSED)             
         DC    AL1(DTSELQ),AL1(DTSDATE-DTSELD),AL1(CO2)                         
***                                                                             
EPAGEX   DC    X'FF'                                                            
         EJECT                                                                  
*                                                                               
* TBAHKADT DS    XL2   PASSIVE       DATE BATCH ADDED TO FILE                   
* TBAHKEDT DS    XL2   PASSIVE       EFFECTIVE DATE OF BATCH                    
* TBAHKUDT DS    XL2   PASSIVE       DATE BATCH UPDATED TO FILE                 
* TBAHRADT DS    XL2   PASSIVE       DATE BATCH ADDED TO FILE                   
* TBAHREDT DS    XL2   PASSIVE       EFFECTIVE DATE OF BATCH                    
* TBAHRUDT DS    XL2   PASSIVE       DATE BATCH UPDATED TO FILE                 
* BATSDAT  DS    XL2   STATUS        BATCH UPDATED DATE                         
* BATRDAT  DS    XL2   STATUS        BATCH UPDATED DATE                         
* MOSPDATE DS    PL3   PASSIVE       TRANSACTION DATE                           
* TCPPDATE DS    PL3   PASSIVE       TRANSACTION DATE (OR ZEROES)               
* BDPPBILD DS    XL2   PASSIVE       BILL DATE                                  
* IDJKDATE DS    PL3   PASSIVE       POSTING DATE (YYMMDD) PWO                  
* IDJKPDAT DS    XL2   STATUS        COMPRESSED POSTING DATE                    
* PBRPCRED DS    XL2   PASSIVE       CREATED DATE                               
* PBRPBILD DS    XL2   PASSIVE       BILLED DATE  (ZERO=DRAFT BILL)             
* PBRKEXPD DS    XL2   PASSIVE       EXPIRY DATE  (PASSIVE ONLY)                
* PBRKBILD DS    XL2   STATUS        BILLED DATE  (ZERO=DRAFT BILL)             
* PBRREXPD DS    XL2   PASSIVE       EXPIRY DATE  (PASSIVE ONLY)                
* PBRRBILD DS    XL2   STATUS        BILLED DATE  (ZERO=DRAFT BILL)             
* CASPEDTE DS    PL3   PASSIVE       END DATE                                   
* CASPSDTE DS    PL3   PASSIVE       START DATE                                 
* STDKSTDT DS    XL3   STATUS        FIRST DATE ON RECORD                       
* STDKENDT DS    XL3   STATUS        LAST DATE ON RECORD                        
* STDRSTDT DS    XL3   STATUS        FIRST DATE ON RECORD                       
* STDRENDT DS    XL3   STATUS        LAST DATE ON RECORD                        
* EDTKSTDT DS    XL3   STATUS        FIRST DATE ON RECORD                       
* EDTKENDT DS    XL3   STATUS        LAST DATE ON RECORD                        
* EDTRSTDT DS    XL3   STATUS        FIRST DATE ON RECORD                       
* EDTRENDT DS    XL3   STATUS        LAST DATE ON RECORD                        
* TRNKSDUE DS    0XL2  STATUS        DUE DATE                                   
* TRNRSDUE DS    0XL2  STATUS        DUE DATE                                   
         EJECT                                                                  
***********************************************************************         
* OTHER INCLUDES                                                      *         
***********************************************************************         
                                                                                
* DDDPRINT                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDDPRINT                                                       
         PRINT ON                                                               
* ACGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
* ACRECEQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACRECEQUS                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'011ACLDXDTE  07/21/00'                                      
         END                                                                    
