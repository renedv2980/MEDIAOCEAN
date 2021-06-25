*          DATA SET SPLDEXTDOV AT LEVEL 106 AS OF 12/12/96                      
*PHASE SPEXTDOV                                                                 
*INCLUDE RECUP                                                                  
*INCLUDE PRNTBL                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE HEXOUT                                                                 
         TITLE 'SPLDEXTDOV - FIXED LENGTH OF DEMO OVERRIDE ELEMENTS'            
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
         NMOD1 WORKX-WORKD,DMLDEXT                                              
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
         L     RF,=V(RECUP)                                                     
         ST    RF,VRECUP                                                        
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
* INITIALIZE LOGIC                                                              
*                                                                               
DMXINIT  DS    0H                                                               
         B     DMXIT                                                            
         EJECT                                                                  
*                                                                               
* PROCESS RECORD LOGIC                                                          
*                                                                               
*                                                                               
****************FIX LENGTH '05' ELEMENTS AT 25******************                
DMXREC   DS    0H                                                               
         L     R6,AREC                                                          
         USING DOVRECD,R6                                                       
         CLC   DOVKTYP,=X'0D17'                                                 
         BNE   DMXKEEP                                                          
         USING DOVEL01,R6                                                       
         MVI   ELCODE,X'01'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   DOVUTBK,=X'5D00'     USE TILL BOOK DATE BEFORE '93?              
         BL    PURGEREC                                                         
         L     R6,AREC                                                          
         MVI   ELCODE,X'05'                                                     
         BAS   RE,GETEL                                                         
         BNE   DMX30                                                            
         USING DOVEL05,R6                                                       
         B     DMX10                                                            
GET05LP  BAS   RE,NEXTEL                                                        
         BNE   DMX30                                                            
*                                                                               
DMX10    XC    ELEM,ELEM                                                        
         ZIC   R1,1(R6)                                                         
         LTR   R1,R1                                                            
         BZ    DMXKEEP                                                          
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   ELEM(0),0(R6)                                                    
         LA    R1,25                                                            
         STC   R1,ELEM+1           NEW FIXED LENGTH OF 25                       
         GOTO1 VRECUP,DMCB,(C'S',AREC),(R6),0      DELETE  ELEMENT              
         GOTO1 VRECUP,DMCB,(C'S',AREC),ELEM,(C'R',(R6)) ADD W/ LEN 25           
         CLI   DMCB+8,X'00'                                                     
         BNE   GET05LP                                                          
         L     R6,AREC                                                          
         GOTO1 VHEXOUT,DMCB,(R6),P,13 PRINT KEY                                 
         MVC   P+30(14),=C'RECORD TOO BIG'                                      
         GOTO1 VPRINTER                                                         
         B     DMXKEEP                                                          
*                                                                               
DMX20    L     R1,COUNT            NUMBER OF RECORDS CHANGED                    
         LA    R1,1(R1)                                                         
         ST    R1,COUNT                                                         
         B     GET05LP                                                          
*                                                                               
***************FIX LENGTH '01' ELEMENTS AT 42******************                 
DMX30    DS    0H                                                               
         L     R6,AREC                                                          
         USING DOVRECD,R6                                                       
         CLC   DOVKTYP,=X'0D17'                                                 
         BNE   DMXKEEP                                                          
         MVI   ELCODE,X'01'                                                     
         BAS   RE,GETEL                                                         
         BNE   DMXKEEP                                                          
         USING DOVEL01,R6                                                       
         B     DMX40                                                            
GET01LP  BAS   RE,NEXTEL                                                        
         BNE   DMXKEEP                                                          
*                                                                               
DMX40    DS    0H                                                               
         XC    ELEM,ELEM                                                        
         ZIC   R1,1(R6)                                                         
         LTR   R1,R1                                                            
         BZ    DMXKEEP                                                          
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   ELEM(0),0(R6)                                                    
         LA    R1,42                                                            
         STC   R1,ELEM+1           NEW FIXED LENGTH OF 42                       
         GOTO1 VRECUP,DMCB,(C'S',AREC),(R6),0      DELETE  ELEMENT              
         GOTO1 VRECUP,DMCB,(C'S',AREC),ELEM,(C'R',(R6)) ADD W/ LEN 42           
         CLI   DMCB+8,X'00'                                                     
         BNE   DMX50                                                            
         L     R6,AREC                                                          
         GOTO1 VHEXOUT,DMCB,(R6),P,13 PRINT KEY                                 
         MVC   P+30(14),=C'RECORD TOO BIG'                                      
         GOTO1 VPRINTER                                                         
         B     DMXKEEP                                                          
*                                                                               
DMX50    DS    0H                                                               
         L     R1,COUNT            NUMBER OF RECORDS CHANGED                    
         LA    R1,1(R1)                                                         
         ST    R1,COUNT                                                         
         B     GET01LP                                                          
*                                                                               
PURGEREC L     R1,PRGCOUNT                                                      
         LA    R1,1(R1)                                                         
         ST    R1,PRGCOUNT                                                      
         B     DMXPURGE                                                         
*                                                                               
DMXEOF   DS    0H                                                               
         MVC   P(15),=C'RECORDS CHANGED'                                        
         EDIT  (4,COUNT),(8,P+25),ZERO=NOBLANK                                  
         GOTO1 VPRINTER                                                         
         GOTO1 VPRINTER                                                         
         MVC   P(14),=C'RECORDS PURGED'                                         
         EDIT  (4,PRGCOUNT),(8,P+25),ZERO=NOBLANK                               
         GOTO1 VPRINTER                                                         
         B     DMXIT                                                            
         EJECT                                                                  
*                                                                               
         GETEL R6,24,ELCODE                                                     
*                                                                               
*&&DO                                                                           
***********************PRINT RECORDS*************************                   
DMPUT    NTR1                                                                   
         L     R9,AREC                                                          
         AP    PDUMP,=P'1'                                                      
         CP    PDUMP,MAXDUMP                                                    
         BH    DUMPX                                                            
*        TM    FLAG,AFTCHNGE       IS IT REC W/CHANGES OR W/O?                  
*        BZ    *+12                                                             
         LA    R7,=C'PUT'                                                       
*        B     *+8                                                              
*        LA    R7,=C'GET'                                                       
         MVC   HALF,DOVLEN-DOVRECD(R9)                                          
         SPACE                                                                  
DUMP     LH    R8,HALF                                                          
         GOTO1 PRNTBL,DMCB,(3,(R7)),AREC,C'DUMP',(R8),=C'2D'                    
         SPACE                                                                  
DUMPX    XIT1                                                                   
*&&                                                                             
*                                                                               
COUNT    DS    F                                                                
PRGCOUNT DS    F                                                                
ELCODE   DS    X                                                                
PRNTBL   DC    V(PRNTBL)                                                        
PRINT    DC    V(PRINT)                                                         
HALF     DS    H                                                                
PDUMP    DC    PL4'1'                                                           
MAXDUMP  DC    PL4'5000'                                                        
         LTORG                                                                  
         SPACE 2                                                                
WORKD    DSECT                                                                  
DUB      DS    D                                                                
WORK     DS    CL64                                                             
ELEM     DS    XL256                                                            
SVELEM   DS    XL256                                                            
DMCB     DS    6F                                                               
APARM    DS    A                                                                
VHEXOUT  DS    A                                                                
VRECUP   DS    A                                                                
         DS    0F                                                               
PLIST    DS    0CL24                                                            
AREC     DS    A                                                                
VTAPEOUT DS    A                                                                
APARAMC  DS    A                                                                
VLDDEFN  DS    A                                                                
VPRINTER DS    A                                                                
VCPRINT  DS    A                                                                
WORKX    EQU   *                                                                
         EJECT                                                                  
*SPGENNDOV                                                                      
       ++INCLUDE SPGENNDOV                                                      
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
**PAN#1  DC    CL21'106SPLDEXTDOV12/12/96'                                      
         END                                                                    
