*          DATA SET SPLDEXTLIN AT LEVEL 007 AS OF 04/11/96                      
*PHASE SPLDMH                                                                   
*INCLUDE HEXOUT                                                                 
         TITLE 'DMLDEXTFRD - CHECK FOR LINE NUM 0 IN 05 ELEM'                   
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
         BNH   DMXIT               NOT A BUY RECORD                             
*                                                                               
         L     R1,COUNT            NUMBER OF BUY RECORDS READ                   
         LA    R1,1(R1)                                                         
         ST    R1,COUNT                                                         
*                                                                               
         USING BUYREC,R6                                                        
*                                                                               
         LA    R6,BDELEM                                                        
         MVI   ELCODLO,X'05'                                                    
         MVI   ELCODHI,X'05'                                                    
DMREC05  BAS   RE,NEXTEL                                                        
         BNE   DMXIT                                                            
*                                                                               
         ZIC   R1,1(R6)            LENGTH OF ELEM                               
         SH    R1,=H'3'            NUMBER OF LINE NUMBERS IN ELEM               
         LTR   R1,R1                                                            
         BZ    DMREC05                                                          
*                                                                               
         LA    R2,3(R6)                                                         
DMREC10  CLI   0(R2),0             IS LINE NUM ZERO?                            
         BE    DMREC20             PRINT OUT KEY AND ELEM                       
         LA    R2,1(R2)            BUMP POINTER TO NEXT LINE NUMBER             
         BCT   R1,DMREC10                                                       
         B     DMREC05             NONE OF THE LINE NUMS WERE ZERO              
*                                                                               
DMREC20  L     R5,AREC                                                          
         MVC   P+1(11),=C'KEY OF BUY:'                                          
         GOTO1 VHEXOUT,DMCB,(R5),P+20,13,=C'TOG'                                
         GOTO1 VPRINTER                                                         
*                                                                               
         ZIC   R2,1(R6)                                                         
         MVC   P+1(22),=C'05 ELEM WITH 0 LINE #:'                               
         GOTO1 VHEXOUT,DMCB,(R6),P+25,(R2),=C'TOG'                              
         GOTO1 VPRINTER                                                         
*                                                                               
         L     R1,COUNT2           NUMBER OF ELEMS WITH ZERO LINE NUM           
         LA    R1,1(R1)                                                         
         ST    R1,COUNT2                                                        
*                                                                               
         B     DMREC05                                                          
         DROP  R6                                                               
*                                                                               
DMXEOF   DS    0H                                                               
         MVC   P(16),=C'BUY RECORDS READ'                                       
         EDIT  (4,COUNT),(8,P+28),ZERO=NOBLANK                                  
         GOTO1 VPRINTER                                                         
         MVC   P(24),=C'05 ELEMS WITH 0 LINE NUM'                               
         EDIT  (4,COUNT2),(8,P+28),ZERO=NOBLANK                                 
         GOTO1 VPRINTER                                                         
         B     DMXIT                                                            
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
SPGENBUYD DSECT                                                                 
       ++INCLUDE SPGENBUY                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'007SPLDEXTLIN04/11/96'                                      
         END                                                                    
