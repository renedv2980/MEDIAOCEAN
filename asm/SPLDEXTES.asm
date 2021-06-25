*          DATA SET SPLDEXTES  AT LEVEL 010 AS OF 07/18/05                      
*          DATA SET SPLDEXTCAN AT LEVEL 040 AS OF 07/31/96                      
*PHASE SP@XTESA                                                                 
*INCLUDE HEXOUT                                                                 
         TITLE 'DMLDEXTES - CONVERT SPOT ESTIMATE HEADERS'                      
*                                                                               
*                                                                               
* PARAMETER LIST                                                                
*                                                                               
* P1=A(RECORD)  PASS FIRST BYTE X'00'= INITIALISE                               
*                               X'01'= RECORD IN CORE                           
*                               X'FF'= END OF FILE                              
*               RETURN VALUE    X'00'= KEEP RECORD                              
*                               X'FF'= PURGE RECORD                             
*                               X'FF'/C'EOJ'=PURGE & CAUSE EOJ                  
* P2=A(TAPEOUT) PASS FIRST BYTE X'80'= TAPE INPUT                               
*                               X'40'= TAPE OUTPUT                              
*                               X'20'= RECORD IS I/S FILE RECORD                
* P3=A(PARAM CARD)                                                              
* P4=A(FILE DEFN)                                                               
* P5=A(PRINTER)                                                                 
* P6=A(CPRINT)                                                                  
*                                                                               
         PRINT NOGEN                                                            
DMLDEXT  CSECT                                                                  
         NMOD1 20,DMLDEXT                                                       
         USING WORKD,RC                                                         
         EJECT                                                                  
*                                                                               
* CONTROL FLOW LOGIC                                                            
*                                                                               
DMXCTL   ST    R1,APARM            SAVE PARAMETERS                              
         MVC   PLIST,0(R1)                                                      
         L     RA,VCPRINT          SET UP FOR PRINTING                          
         USING DPRINT,RA                                                        
         L     RF,=V(HEXOUT)                                                    
         ST    RF,VHEXOUT                                                       
         SPACE 2                                                                
         CLI   PLIST,X'00'                                                      
         BE    DMXINIT             INITIALISE                                   
         CLI   PLIST,X'01'                                                      
         BE    DMXREC              PROCESS                                      
         CLI   PLIST,X'FF'                                                      
         BE    DMXEOF              END-OF-FILE                                  
         B     DMXIT                                                            
         SPACE 2                                                                
DMXKEEP  L     R1,APARM            KEEP RECORD EXIT                             
         MVI   0(R1),0                                                          
         B     DMXIT                                                            
         SPACE 2                                                                
DMXPURGE L     R1,APARM            PURGE RECORD EXIT                            
         MVI   0(R1),X'FF'                                                      
         B     DMXIT                                                            
         SPACE 2                                                                
DMXPGEOF L     R1,APARM            PURGE AND CAUSE INPUT EOF EXIT               
         MVI   0(R1),X'FF'                                                      
         MVC   1(3,R1),=C'EOF'                                                  
         B     DMXIT                                                            
         SPACE 2                                                                
DMXIT    XIT1                                                                   
         SPACE 2                                                                
*                                                                               
* INITIALIZE LOGIC                                                              
*                                                                               
DMXINIT  DS    0H                                                               
         B     DMXIT                                                            
         EJECT                                                                  
*                                                                               
* PROCESS RECORD LOGIC                                                          
*                                                                               
DMXREC   DS    0H                                                               
         L     R6,AREC                                                          
         USING ESTHDRD,R6                                                       
*                                                                               
         CLI   0(R6),0             TEST HEADER                                  
         BNE   DMXKEEP                                                          
         CLI   7(R6),0             TEST ESTIMATE NUMBER                         
         BE    DMXKEEP             NO                                           
         CLI   8(R6),0             TEST BILL                                    
         BNE   DMXKEEP                                                          
* THIS IS AN ESTIMATE HEADER                                                    
         CLC   ELEN,=Y(ESTHDRLN)   TEST NEW LEN                                 
         BE    DMXKEEP                                                          
*                                                                               
         CLC   COUNT,=F'100'                                                    
         BH    NOPRINT                                                          
         GOTO1 VHEXOUT,DMCB,(R6),P,13,=C'TOG'                                   
         GOTO1 VPRINTER                                                         
*                                                                               
NOPRINT  L     R0,COUNT                                                         
         AHI   R0,1                                                             
         ST    R0,COUNT                                                         
* CONVERT                                                                       
         MVC   ELEN,=Y(ESTHDRLN)                                                
         MVC   SVEORDN,XEORDN                                                   
         MVC   SVEPAIDN,XEPAIDN                                                 
         MVC   SVEAUTHN,XEAUTHN                                                 
*                                                                               
         XC    ESPARE1,ESPARE1                                                  
         XC    ESPARE2,ESPARE2                                                  
         XC    ESPARE3,ESPARE3                                                  
         ZAP   ECURPDN,=P'0'                                                    
*                                                                               
         LA    R0,26                                                            
         LA    R1,SVEORDN                                                       
         LA    R2,EORD                                                          
*                                                                               
DMX10    L     RE,0(R1)                                                         
         CVD   RE,DUB                                                           
         ZAP   0(6,R2),DUB                                                      
         LA    R1,4(R1)                                                         
         LA    R2,6(R2)                                                         
         BCT   R0,DMX10                                                         
*                                                                               
         LA    R0,26                                                            
         LA    R1,SVEPAIDN                                                      
         LA    R2,EPAID                                                         
*                                                                               
DMX20    L     RE,0(R1)                                                         
         CVD   RE,DUB                                                           
         ZAP   0(6,R2),DUB                                                      
         LA    R1,4(R1)                                                         
         LA    R2,6(R2)                                                         
         BCT   R0,DMX20                                                         
*                                                                               
         LA    R0,13                                                            
         LA    R1,SVEAUTHN                                                      
         LA    R2,EAUTH                                                         
*                                                                               
DMX30    L     RE,0(R1)                                                         
         CVD   RE,DUB                                                           
         ZAP   0(6,R2),DUB                                                      
         LA    R1,4(R1)                                                         
         LA    R2,6(R2)                                                         
         BCT   R0,DMX30                                                         
         B     DMXKEEP                                                          
         DROP  R6                                                               
                                                                                
DMXEOF   DS    0H                                                               
         MVC   P(15),=C'RECORDS CHANGED'                                        
         EDIT  (4,COUNT),(8,P+25),ZERO=NOBLANK                                  
         GOTO1 VPRINTER                                                         
         B     DMXIT                                                            
         EJECT                                                                  
                                                                                
EXIT     XIT1                                                                   
         EJECT                                                                  
         GETEL R6,24,ELCODE                                                     
*                                                                               
BYTE     DS    X                                                                
COUNT    DS    F                                                                
ELCODE   DS    X                                                                
*                                                                               
         DS    0D                                                               
SVEORDN  DS    XL104                                                            
SVEPAIDN DS    XL104                                                            
SVEAUTHN DS    XL52                                                             
         LTORG                                                                  
         SPACE 2                                                                
WORKD    DSECT                                                                  
DUB      DS    D                                                                
WORK     DS    CL64                                                             
DMCB     DS    6F                                                               
APARM    DS    A                                                                
VHEXOUT  DS    A                                                                
         DS    0F                                                               
PLIST    DS    0CL24                                                            
AREC     DS    A                                                                
VTAPEOUT DS    A                                                                
APARAMC  DS    A                                                                
VLDDEFN  DS    A                                                                
VPRINTER DS    A                                                                
VCPRINT  DS    A                                                                
         EJECT                                                                  
*DMLDDEFN                                                                       
       ++INCLUDE DMLDDEFN                                                       
         EJECT                                                                  
*DDDPRINT                                                                       
       ++INCLUDE DDDPRINT                                                       
         SPACE 2                                                                
         PRINT OFF                                                              
ESTHDRD   DSECT                                                                 
       ++INCLUDE SPGENEST                                                       
         PRINT OFF                                                              
BUYRECD   DSECT                                                                 
       ++INCLUDE SPGENBUY                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'010SPLDEXTES 07/18/05'                                      
         END                                                                    
