*          DATA SET SPLDEXTOTO AT LEVEL 053 AS OF 04/01/96                      
*PHASE SPEXTOTO                                                                 
*INCLUDE HEXOUT                                                                 
*INCLUDE RECUP                                                                  
*INCLUDE PRNTBL                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE CLUNPK                                                                 
*INCLUDE MSUNPK                                                                 
         TITLE 'DMLDEXTOTO - CHECK FOR 2 0C MINUS ELEMS AND DEL'                
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
*                                                                               
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
* INITIALIZE LOGIC                                                              
*                                                                               
DMXINIT  DS    0H                                                               
         B     DMXIT                                                            
*                                                                               
* PROCESS RECORD LOGIC                                                          
*                                                                               
DMXREC   DS    0H                                                               
         L     R6,AREC                                                          
         CLI   0(R6),X'10'                                                      
         BNH   DMXPURGE            NOT A BUY RECORD                             
*                                                                               
         L     R1,COUNT            NUMBER OF BUY RECORDS READ                   
         LA    R1,1(R1)                                                         
         ST    R1,COUNT                                                         
*                                                                               
         USING BUYREC,R6                                                        
*                                                                               
         TM    BDSTAT,X'80'                                                     
         BZ    DMXPURGE                                                         
*                                                                               
         LA    R6,BDELEM                                                        
         MVI   ELCODLO,X'0C'                                                    
         MVI   ELCODHI,X'0C'                                                    
DMREC05  BAS   RE,NEXTEL                                                        
         BNE   DMXPURGE                                                         
*                                                                               
         TM    6(R6),X'80'         MINUS SPOT?                                  
         BZ    DMREC05                                                          
         SR    R0,R0                                                            
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),X'0C'         2 OC ELEMS IN A ROW                          
         BNE   DMREC05                                                          
         TM    6(R6),X'80'         2ND IN A ROW IS MINUSED?                     
         BZ    DMREC05                                                          
*                                                                               
         BAS   RE,PRINTREC                                                      
         B     DMXKEEP                                                          
*                                                                               
         DROP  R6                                                               
*                                                                               
DMXEOF   DS    0H                                                               
         MVC   P(12),=C'RECORDS READ'                                           
         EDIT  (4,COUNT),(8,P+25),ZERO=NOBLANK                                  
         GOTO1 VPRINTER                                                         
         MVC   P(22),=C'NUMBER OF BAD CHANGES:'                                 
         EDIT  (4,COUNT2),(8,P+25),ZERO=NOBLANK                                 
         GOTO1 VPRINTER                                                         
         B     DMXIT                                                            
*                                                                               
PRINTREC NTR1                                                                   
         L     R5,AREC                                                          
         GOTO1 =V(CLUNPK),DMCB,1(R5),P+1                                        
*                                                                               
         GOTO1 =V(MSUNPK),DMCB,4(R5),P+6,P+12                                   
*                                                                               
         SR    R0,R0                                                            
         IC    R0,9(R5)                                                         
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+20(3),DUB                                                      
         MVI   P+23,C'-'                                                        
*                                                                               
         SR    R0,R0                                                            
         IC    R0,10(R5)                                                        
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+24(3),DUB                                                      
         GOTO1 VPRINTER                                                         
*                                                                               
         SR    R3,R3                                                            
         ICM   R3,3,13(R5)                                                      
         GOTO1 =V(PRNTBL),DMCB,=C'BUYREC',(R5),C'DUMP',(R3),=C'1D00'            
         GOTO1 VPRINTER                                                         
*                                                                               
         GOTO1 VHEXOUT,DMCB,(R5),P+1,13,=C'TOG'                                 
         GOTO1 VPRINTER                                                         
*                                                                               
         GOTO1 VHEXOUT,DMCB,(R6),P+1,12,=C'TOG'                                 
         GOTO1 VPRINTER                                                         
*                                                                               
         L     R5,AREC                                                          
*                                                                               
         L     R1,COUNT2           NUMBER OF BAD CHANGES WE MADE                
         LA    R1,1(R1)                                                         
         ST    R1,COUNT2                                                        
         XIT1                                                                   
*                                                                               
NEXTEL   CLI   0(R6),0                                                          
         BE    NEXTELX                                                          
         ZIC   R0,1(R6)                                                         
         LTR   R0,R0                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R6,R0                                                            
NEXTEL2  CLI   0(R6),0                                                          
         BE    NEXTELX                                                          
         CLC   ELCODLO,0(R6)                                                    
         BH    NEXTEL                                                           
         CLC   ELCODHI,0(R6)                                                    
         BL    NEXTEL                                                           
         CR    RB,RB                                                            
         B     *+6                                                              
NEXTELX  LTR   RB,RB                                                            
         BR    RE                                                               
         EJECT                                                                  
*                                                                               
BYTE     DS    X                                                                
COUNT    DS    F                                                                
COUNT2   DS    F                                                                
ELCODLO  DS    X                                                                
ELCODHI  DS    X                                                                
FLAG     DS    X                                                                
CHNGREC  EQU   X'80'                                                            
*                                                                               
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
*                                                                               
*DMLDDEFN                                                                       
       ++INCLUDE DMLDDEFN                                                       
         EJECT                                                                  
*DDDPRINT                                                                       
       ++INCLUDE DDDPRINT                                                       
         SPACE 2                                                                
         PRINT OFF                                                              
SPGENBUYD DSECT                                                                 
       ++INCLUDE SPGENBUY                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'053SPLDEXTOTO04/01/96'                                      
         END                                                                    
