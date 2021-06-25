*          DATA SET SRCOL00    AT LEVEL 004 AS OF 05/01/02                      
*PHASE T11B00A                                                                  
         TITLE '$COLOUR - DISPLAY/SET TERMINAL COLOURS'                         
         PRINT NOGEN                                                            
CLR      CSECT                                                                  
         NMOD1 SRWORKX-SRWORKD,**$CLR**                                         
         USING SRWORKD,RC                                                       
         USING SRPARMD,R1                                                       
         L     RA,SRPARM6          RA=A(TWA)                                    
         USING SRCOLFFD,RA                                                      
         L     R9,SRPARM1          R9=A(SYSFACS)                                
         USING SYSFACD,R9                                                       
         L     R5,SRPARM3          R5=A(UTL ENTRY)                              
         USING UTLD,R5                                                          
         L     RE,SRPARM8          RE=A(TIOB)                                   
         OI    TIOBINDS-TIOBD(RE),TIOBCLR SET WANT COLOUR ACTIVE                
*                                                                               
CLR1     MVC   MASKUTL,TCLRMSK     EXTRACT COLOUR MASK FROM UTL                 
         OC    MASKUTL,MASKUTL                                                  
         BNZ   CLR2                                                             
         MVC   MASKUTL,DMASKUTL    USE DEFAULT IF NOT ALREADY SET               
         TM    TTYPE,TTYPECOL                                                   
         BO    *+8                                                              
         NI    MASKUTL,255-X'80'   SET COLOUR NOP                               
CLR2     ICM   R0,15,MASKUTL                                                    
         LA    RE,MASK+7                                                        
         LA    RF,8                                                             
CLR3     SRDL  R0,4                EXPAND MASK INTO BYTE VALUES                 
         SRL   R1,28                                                            
         STC   R1,0(RE)                                                         
         BCTR  RE,0                                                             
         BCT   RF,CLR3                                                          
CLR4     MVC   SVMASK,MASK         SAVE ORIGINAL MASK                           
         SPACE 1                                                                
CLR5     TM    TSVCREQ,X'01'       TEST FIRST TIME                              
         BZ    CLR6                                                             
         OI    TSVCREQ,X'02'       SET CURSOR FLAG                              
         MVC   SRVIC1(1),MASK+1    MOVE UTL MASK CHR IN                         
         OI    SRVIC1,C'0'                                                      
         MVC   SRVIC2(1),MASK+2                                                 
         OI    SRVIC2,C'0'                                                      
         MVC   SRVIC3(1),MASK+3                                                 
         OI    SRVIC3,C'0'                                                      
         MVC   SRVIC4(1),MASK+4                                                 
         OI    SRVIC4,C'0'                                                      
         MVC   SRVIC5(1),MASK+5                                                 
         OI    SRVIC5,C'0'                                                      
         MVC   SRVIC6(1),MASK+6                                                 
         OI    SRVIC6,C'0'                                                      
         MVC   SRVIC7(1),MASK+7                                                 
         OI    SRVIC7,C'0'                                                      
         SPACE 1                                                                
CLR6     EQU   *                                                                
         EJECT                                                                  
***********************************************************************         
* VALIDATE/DISPLAY INPUT FIELDS                                       *         
***********************************************************************         
         SPACE 1                                                                
P1VAL    LA    R4,SRVP1H           P1=YES/NO FOR COLOUR                         
         CLI   SRVP1H+5,0                                                       
         BNE   P1VAL1                                                           
         MVI   SRVP1,C'N'          SET VALUE IF NO INPUT                        
         MVI   CLRYON,C'N'                                                      
         TM    TTYPE,TTYPECOL                                                   
         BZ    P1VALX                                                           
         TM    MASK,X'08'                                                       
         BZ    P1VALX                                                           
         MVI   SRVP1,C'Y'                                                       
*                                                                               
P1VAL1   CLI   SRVP1,C'N'          N=COLOURS OFF                                
         BNE   P1VAL2                                                           
         NI    TTYPE,255-TTYPECOL                                               
         MVI   CLRYON,C'N'                                                      
         B     P1VALX                                                           
*                                                                               
P1VAL2   CLI   SRVP1,C'Y'          Y=COLOURS ON                                 
         BNE   P1VAL3                                                           
         OI    TTYPE,TTYPECOL                                                   
         MVI   CLRYON,C'Y'                                                      
         B     P1VALX                                                           
*                                                                               
P1VAL3   CLI   SRVP1,C'S'          S=COLOURS STANDARD                           
         BNE   ERROR2                                                           
         OI    TTYPE,TTYPECOL                                                   
         MVI   CLRYON,C'Y'                                                      
         MVI   SRVIC1,C'1'         SET STANDARD COLOURS                         
         MVI   SRVIC2,C'2'                                                      
         MVI   SRVIC3,C'3'                                                      
         MVI   SRVIC4,C'4'                                                      
         MVI   SRVIC5,C'5'                                                      
         MVI   SRVIC6,C'6'                                                      
         MVI   SRVIC7,C'7'                                                      
         MVI   SRVP1,C'Y'          RESET ACTION TO COLOUR ON                    
P1VALX   EQU   *                                                                
         SPACE 1                                                                
C1VAL    LA    R4,SRVIC1H          VALIDATE COLOUR#1                            
         MVC   CLRNUM,MASK+1                                                    
         BAS   RE,CLRVAL                                                        
         BE    ERROR2                                                           
         MVC   MASK+1(1),CLRINP                                                 
         SPACE 1                                                                
C2VAL    LA    R4,SRVIC2H          VALIDATE COLOUR#2                            
         MVC   CLRNUM,MASK+2                                                    
         BAS   RE,CLRVAL                                                        
         BE    ERROR2                                                           
         MVC   MASK+2(1),CLRINP                                                 
         SPACE 1                                                                
C3VAL    LA    R4,SRVIC3H          VALIDATE COLOUR#3                            
         MVC   CLRNUM,MASK+3                                                    
         BAS   RE,CLRVAL                                                        
         BE    ERROR2                                                           
         MVC   MASK+3(1),CLRINP                                                 
         SPACE 1                                                                
C4VAL    LA    R4,SRVIC4H          VALIDATE COLOUR#4                            
         MVC   CLRNUM,MASK+4                                                    
         BAS   RE,CLRVAL                                                        
         BE    ERROR2                                                           
         MVC   MASK+4(1),CLRINP                                                 
         SPACE 1                                                                
C5VAL    LA    R4,SRVIC5H          VALIDATE COLOUR#5                            
         MVC   CLRNUM,MASK+5                                                    
         BAS   RE,CLRVAL                                                        
         BE    ERROR2                                                           
         MVC   MASK+5(1),CLRINP                                                 
         SPACE 1                                                                
C6VAL    LA    R4,SRVIC6H          VALIDATE COLOUR#6                            
         MVC   CLRNUM,MASK+6                                                    
         BAS   RE,CLRVAL                                                        
         BE    ERROR2                                                           
         MVC   MASK+6(1),CLRINP                                                 
         SPACE 1                                                                
C7VAL    LA    R4,SRVIC7H          VALIDATE COLOUR#7                            
         MVC   CLRNUM,MASK+7                                                    
         BAS   RE,CLRVAL                                                        
         BE    ERROR2                                                           
         MVC   MASK+7(1),CLRINP                                                 
         EJECT                                                                  
***********************************************************************         
* SET NEW MASK BACK IN UTL                                            *         
***********************************************************************         
         SPACE 1                                                                
CLRSET   NI    MASK,255-X'08'      SET COLOUR OFF                               
         CLI   CLRYON,C'Y'                                                      
         BNE   CLRSET1                                                          
         OI    MASK,X'08'                                                       
*                                                                               
CLRSET1  NI    MASK,255-X'04'      SET STANDARD MASK                            
         CLC   MASK+1(7),DMASK+1                                                
         BE    CLRSET4                                                          
         OI    MASK,X'04'          SET AND DISPLAY NON STANDARD COLOURS         
         TM    MASK,X'08'                                                       
         BZ    CLRSET4             BUT ONLY IF COLOUR ON                        
         OI    MASK,X'01'          ASK TO3270 TO USE STANDARD MASK              
         MVC   SRVMC1,SRVRC1                                                    
         MVC   SRVMC1H+4(2),=X'0C80'                                            
         OC    SRVMC1H+5(1),MASK+1                                              
         MVC   SRVMC2,SRVRC2                                                    
         MVC   SRVMC2H+4(2),=X'0C80'                                            
         OC    SRVMC2H+5(1),MASK+2                                              
         MVC   SRVMC3,SRVRC3                                                    
         MVC   SRVMC3H+4(2),=X'0C80'                                            
         OC    SRVMC3H+5(1),MASK+3                                              
         MVC   SRVMC4,SRVRC4                                                    
         MVC   SRVMC4H+4(2),=X'0C80'                                            
         OC    SRVMC4H+5(1),MASK+4                                              
         MVC   SRVMC5,SRVRC5                                                    
         MVC   SRVMC5H+4(2),=X'0C80'                                            
         OC    SRVMC5H+5(1),MASK+5                                              
         MVC   SRVMC6,SRVRC6                                                    
         MVC   SRVMC6H+4(2),=X'0C80'                                            
         OC    SRVMC6H+5(1),MASK+6                                              
         MVC   SRVMC7,SRVRC7                                                    
         MVC   SRVMC7H+4(2),=X'0C80'                                            
         OC    SRVMC7H+5(1),MASK+7                                              
*                                                                               
CLRSET4  LA    RE,MASK             PACK MASK INTO UTL FORMAT                    
         LA    RF,8                                                             
CLRSET5  IC    R1,0(RE)                                                         
         SLL   R1,28                                                            
         SLDL  R0,4                                                             
         LA    RE,1(RE)                                                         
         BCT   RF,CLRSET5                                                       
         STCM  R0,15,MASKUTL                                                    
         MVC   TCLRMSK,MASKUTL                                                  
*                                                                               
         OI    SRVP1H+6,X'40'      POSN CURSOR TO P1 FIELD                      
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ERRORS AND EXIT CODE                                                *         
***********************************************************************         
         SPACE 1                                                                
ERROR1   LA    R1,ERRMSG1          MISSING INPUT FIELD                          
         B     ERRXIT                                                           
ERROR2   LA    R1,ERRMSG2          INVALID INPUT FIELD                          
         B     ERRXIT                                                           
*                                                                               
ERRXIT   XC    SRVMSG,SRVMSG                                                    
         MVC   SRVMSG(8),=C'ED-0000 '                                           
         MVC   SRVMSG+8(40),0(R1)                                               
         OI    6(R4),X'40'         POSITION CURSOR TO ERROR FIELD               
*                                                                               
EXIT     XMOD1 1                                                                
         EJECT                                                                  
***********************************************************************         
* VALIDATE COLOUR CODE IN FIELD - R4=A(FIELD HEADER)                  *         
***********************************************************************         
         SPACE 1                                                                
CLRVAL   ST    RE,SAVERE                                                        
         SR    R1,R1               R1=COLOUR NUMBER FROM MASK                   
         IC    R1,CLRNUM                                                        
         MVI   CLRINP,0            SET INPUT COLOUR CODE INVALID                
         MVC   CLRCHR,8(R4)                                                     
         OI    CLRCHR,C' '         GET UPPER CASE VALUE OF COLOUR CHR           
         CLI   CLRCHR,C' '                                                      
         BNE   *+12                                                             
         STC   R1,CLRINP           USE MASK VALUE IF NOT INPUT                  
         B     CLRVAL3                                                          
         LA    RF,CLRTAB                                                        
CLRVAL1  CLI   0(RF),X'FF'         SEARCH TABLE OF CODES/NUMBERS                
         BE    CLRVALX                                                          
         CLC   0(1,RF),CLRCHR                                                   
         BE    CLRVAL2                                                          
         LA    RF,2(RF)                                                         
         B     CLRVAL1                                                          
CLRVAL2  MVC   CLRINP,1(RF)        EXTRACT NUMBER                               
         SR    R1,R1                                                            
         IC    R1,CLRINP                                                        
CLRVAL3  SLL   R1,4                                                             
         LA    R1,CLRTXT(R1)       GET TEXT THAT GOES WITH IT                   
         MVC   8(16,R4),0(R1)                                                   
         TM    1(R4),X'02'         TEST EXTENDED FIELD                          
         BZ    CLRVALX                                                          
         SR    R1,R1                                                            
         IC    R1,0(R4)                                                         
         AR    R1,R4               POINT TO EXTENDED HEADER                     
         SH    R1,=H'8'                                                         
         NI    5(R1),B'11111000'                                                
         OC    5(1,R1),CLRINP      SET NEW COLOUR IN EXTENDED HEADER            
*                                                                               
CLRVALX  CLI   CLRINP,0            EXIT WITH CC=EQL IF INVALID                  
         L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* TABLES OF COLOURS AND CODES                                         *         
***********************************************************************         
         SPACE 1                                                                
CLRTAB   DS    0XL2                                                             
         DC    C'1',AL1(1)                                                      
         DC    C'B',AL1(1)                                                      
         DC    C'2',AL1(2)                                                      
         DC    C'R',AL1(2)                                                      
         DC    C'3',AL1(3)                                                      
         DC    C'P',AL1(3)                                                      
         DC    C'4',AL1(4)                                                      
         DC    C'G',AL1(4)                                                      
         DC    C'5',AL1(5)                                                      
         DC    C'T',AL1(5)                                                      
         DC    C'6',AL1(6)                                                      
         DC    C'Y',AL1(6)                                                      
         DC    C'7',AL1(7)                                                      
         DC    C'W',AL1(7)                                                      
CLRTABX  DC    X'FF'                                                            
         SPACE 1                                                                
CLRTXT   DC    CL16' '                                                          
         DC    CL16'1 Blue'                                                     
         DC    CL16'2 Red'                                                      
         DC    CL16'3 Pink'                                                     
         DC    CL16'4 Green'                                                    
         DC    CL16'5 Turquoise'                                                
         DC    CL16'6 Yellow'                                                   
         DC    CL16'7 White'                                                    
         SPACE 1                                                                
DMASK    DC    X'0801020304050607'                                              
DMASKUTL DC    X'81234567'                                                      
         LTORG                                                                  
         EJECT                                                                  
ERRMSG1  DC    CL40'Missing input field'                                        
ERRMSG2  DC    CL40'Invalid input field'                                        
         EJECT                                                                  
SRWORKD  DSECT                                                                  
DUB      DS    D                                                                
FULL     DS    F                                                                
SAVERE   DS    F                                                                
SVMASK   DS    XL8                                                              
MASK     DS    XL8                                                              
MASKUTL  DS    XL4                                                              
DMCB     DS    6F                                                               
CLRINP   DS    X                                                                
CLRNUM   DS    X                                                                
CLRCHR   DS    C                                                                
CLRYON   DS    C                                                                
SRWORKX  DS    0C                                                               
         EJECT                                                                  
* FADSECTS                                                                      
       ++INCLUDE FADSECTS                                                       
         SPACE 1                                                                
* DDCOMFACS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
         EJECT                                                                  
SRCOLFFD DSECT                                                                  
         DS    CL64                                                             
* SRCOLFFD                                                                      
       ++INCLUDE SRCOLFFD                                                       
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'004SRCOL00   05/01/02'                                      
         END                                                                    
