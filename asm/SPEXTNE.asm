*          DATA SET SPEXTNE    AT LEVEL 012 AS OF 07/07/00                      
*PHASE SPEXTNE                                                                  
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE PRTREC                                                                 
*INCLUDE RECUP                                                                  
         TITLE 'DMLDEXTNE - DELETE DUPLICATE SPILL ELEMENTS'                    
* MEDIA T, CLT ZZZ                                                              
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
*                                                                               
         L     RE,VLDDEFN                                                       
         USING LDDEFND,RE                                                       
         MVC   VHEXOUT,LHEXOUT                                                  
         MVC   VPRINT,LPRINT                                                    
         DROP  RE                                                               
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
DMXIT    XMOD1 1                                                                
         SPACE 2                                                                
*                                                                               
* INITIALISE LOGIC                                                              
*                                                                               
DMXINIT  DS    0H                                                               
         B     DMXIT                                                            
         EJECT                                                                  
*                                                                               
* PROCESS RECORD LOGIC                                                          
*                                                                               
DMXREC   DS    0H                                                               
         L     R3,AREC             POINT TO RECORD                              
         USING BUYRECD,R3                                                       
*                                                                               
         CLC   =X'71E739FF',0(R3)                                               
         BNE   DMXKEEP                                                          
*                                                                               
         LA    R6,BDELEM                                                        
         MVI   ELCDLO,3                                                         
         MVI   ELCDHI,3                                                         
         MVI   UPDFLAG,C'N'                                                     
*                                                                               
DMX2     BAS   RE,NEXTEL                                                        
         BNE   DMX10                                                            
*                                                                               
         LR    R7,R6               SAVE 03 ELEM POINTER                         
*                                                                               
DMX4     BAS   RE,NEXTEL                                                        
         BNE   DMX8                NO DUP                                       
         CLC   4(2,R6),4(R7)       SAME MARKET                                  
         BNE   DMX4                                                             
*                                                                               
         CLI   UPDFLAG,C'Y'                                                     
         BE    DMX6                                                             
         MVI   UPDFLAG,C'Y'                                                     
         CP    RECCT,=P'10'                                                     
         BH    DMX6                                                             
         MVC   P(6),=C'BEFORE'                                                  
         GOTO1 VPRINTER                                                         
         GOTO1 =V(PRTREC),DMCB,(C'E',(R3)),(24,13),VPRINT,VHEXOUT               
*                                                                               
DMX6     GOTO1 =V(RECUP),DMCB,(0,(R3)),(R6)                                     
         AP    RECCT,=P'1'                                                      
*                                                                               
DMX8     LR    R6,R7               RESTORE 03 EL POINTER                        
         B     DMX2                                                             
*                                                                               
DMX10    CLI   UPDFLAG,C'Y'                                                     
         BNE   DMXKEEP                                                          
         CP    RECCT,=P'10'                                                     
         BH    DMXKEEP                                                          
         MVC   P(5),=C'AFTER'                                                   
         GOTO1 VPRINTER                                                         
         GOTO1 =V(PRTREC),DMCB,(C'E',(R3)),(24,13),VPRINT,VHEXOUT               
         B     DMXKEEP                                                          
                                                                                
*                                                                               
DMXEOF   DS    0H                                                               
         EDIT  RECCT,(10,P+10),0,COMMAS=YES,MINUS=YES                           
         MVC   P+20(10),=C'RECS FOUND'                                          
         GOTO1 VPRINTER                                                         
         B     DMXIT                                                            
         EJECT                                                                  
NEXTEL   CLI   0(R6),0                                                          
         JE    NEXTELX                                                          
         SR    R0,R0                                                            
         IC    R0,1(R6)                                                         
         LTR   R0,R0                                                            
         JNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R6,R0                                                            
NEXTEL2  CLI   0(R6),0                                                          
         JE    NEXTELX                                                          
         CLC   ELCDLO,0(R6)                                                     
         JH    NEXTEL                                                           
         CLC   ELCDHI,0(R6)                                                     
         JL    NEXTEL                                                           
         CR    RB,RB                                                            
         J     *+6                                                              
NEXTELX  LTR   RB,RB                                                            
         BR    RE                                                               
         EJECT                                                                  
         DS    0D                                                               
WORK     DS    CL64                                                             
RECCT    DC    PL4'0'                                                           
ELCDLO   DS    X                                                                
ELCDHI   DS    X                                                                
UPDFLAG  DS    C                                                                
         LTORG                                                                  
         SPACE 2                                                                
WORKD    DSECT                                                                  
DUB      DS    D                                                                
DMCB     DS    6F                                                               
APARM    DS    A                                                                
PLIST    DS    0CL24                                                            
AREC     DS    A                                                                
VTAPEOUT DS    A                                                                
APARAMC  DS    A                                                                
VLDDEFN  DS    A                                                                
VPRINTER DS    A                                                                
VCPRINT  DS    A                                                                
* FOLLOWING MOVED FROM DMLDDEFN                                                 
VHEXOUT  DS    A                                                                
VPRINT   DS    A                                                                
         EJECT                                                                  
*DMLDDEFN                                                                       
       ++INCLUDE DMLDDEFN                                                       
         EJECT                                                                  
*DDDPRINT                                                                       
       ++INCLUDE DDDPRINT                                                       
         SPACE 2                                                                
         PRINT OFF                                                              
BUYRECD   DSECT                                                                 
       ++INCLUDE SPGENBUY                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'012SPEXTNE   07/07/00'                                      
         END                                                                    
