*          DATA SET SPLDEXTBSA AT LEVEL 032 AS OF 02/03/99                      
*PHASE SPEXTBS                                                                  
*INCLUDE HEXOUT                                                                 
         TITLE 'DMLDEXTBS - FIND CLOSED OUT INV RECORDS FOR BSNY'               
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
DMXREC   DS    0H                                                               
         L     R6,AREC                                                          
         CLI   0(R6),X'0B'         INVOICE RECS                                 
         BNE   DMXPURGE                                                         
         MVC   BYTE,1(R6)          AGENCY/MEDIA                                 
         NI    BYTE,X'F0'                                                       
         CLI   BYTE,X'20'          IS IT BSNY?  BACK THEN = X'20'               
         BNE   DMXPURGE                                                         
*                                                                               
*        GOTO1 VHEXOUT,DMCB,(R6),P,16,=C'TOG'                                   
*        GOTO1 VPRINTER                                                         
*                                                                               
         LA    R5,CLTABLE          THE TABLE OF ALL MILLER CLIENT CODE          
DMXREC01 CLI   0(R5),X'FF'                                                      
         BE    DMXPURGE            END OF TABLE, NOT A MILLER RECORD            
         CLC   5(2,R6),0(R5)                                                    
         BE    DMXREC05                                                         
         LA    R5,2(R5)                                                         
         B     DMXREC01                                                         
*                                                                               
DMXREC05 DS    0H                                                               
         TM    15(R6),X'C0'        CLOSE OUT INVOICE?                           
         BNO   DMXPURGE                                                         
*                                                                               
         NI    15(R6),X'FF'-X'C0'  FLIP BIT                                     
*                                                                               
         L     R1,COUNT            NUMBER OF INVOICE KEPT                       
         LA    R1,1(R1)                                                         
         ST    R1,COUNT                                                         
*                                                                               
DMXREC10 CLC   COUNT,=F'100'                                                    
         BH    DMXKEEP                                                          
         GOTO1 VHEXOUT,DMCB,(R6),P,16,=C'TOG'                                   
         GOTO1 VPRINTER                                                         
         B     DMXKEEP                                                          
*                                                                               
DMXEOF   DS    0H                                                               
         MVC   P(12),=C'RECORDS KEPT'                                           
         EDIT  (4,COUNT),(8,P+25),ZERO=NOBLANK                                  
         GOTO1 VPRINTER                                                         
         B     DMXIT                                                            
         EJECT                                                                  
         GETEL R6,24,ELCODE                                                     
*                                                                               
BYTE     DS    X                                                                
COUNT    DS    F                                                                
ELCODE   DS    X                                                                
*                                                                               
CLTABLE  DC    XL2'B169'           MILLER CLIENT CODE TABLE                     
         DC    XL2'B229'                                                        
         DC    XL2'B17A'                                                        
         DC    XL2'B23A'                                                        
         DC    XL2'B17B'                                                        
         DC    XL2'B23B'                                                        
         DC    XL2'B25B'                                                        
         DC    XL2'A19C'                                                        
         DC    XL2'B01C'                                                        
         DC    XL2'B0FC'                                                        
         DC    XL2'B17C'                                                        
         DC    XL2'B23C'                                                        
         DC    XL2'B25C'                                                        
         DC    XL2'BA5C'                                                        
         DC    X'FF'                                                            
*                                                                               
CLTAB2   DC    XL2'B01B'           MILLER CLIENT CODE TABLE                     
         DC    XL2'B01C'                                                        
         DC    XL2'B01D'                                                        
         DC    XL2'B01E'                                                        
         DC    XL2'B029'                                                        
         DC    XL2'B03A'                                                        
         DC    XL2'B03B'                                                        
         DC    XL2'B03C'                                                        
         DC    XL2'B03D'                                                        
         DC    XL2'B03E'                                                        
         DC    XL2'B05E'                                                        
         DC    XL2'B09E'                                                        
         DC    XL2'B0BA'                                                        
         DC    XL2'B0FC'                                                        
         DC    XL2'B0FD'                                                        
         DC    XL2'B0FE'                                                        
         DC    XL2'B11D'                                                        
         DC    XL2'B11E'                                                        
         DC    XL2'B227'                                                        
         DC    XL2'B228'                                                        
         DC    XL2'B229'                                                        
         DC    XL2'B23A'                                                        
         DC    XL2'B23B'                                                        
         DC    XL2'B23C'                                                        
         DC    XL2'B23D'                                                        
         DC    XL2'B23E'                                                        
         DC    XL2'B247'                                                        
         DC    XL2'B248'                                                        
         DC    XL2'B249'                                                        
         DC    XL2'B252'                                                        
         DC    XL2'B25A'                                                        
         DC    XL2'B25B'                                                        
         DC    XL2'B25C'                                                        
         DC    XL2'B25D'                                                        
         DC    XL2'B25E'                                                        
         DC    XL2'B27B'                                                        
         DC    XL2'B27C'                                                        
         DC    XL2'B27D'                                                        
         DC    XL2'B27E'                                                        
         DC    XL2'B2FD'                                                        
         DC    X'FF'                                                            
         EJECT                                                                  
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
**PAN#1  DC    CL21'032SPLDEXTBSA02/03/99'                                      
         END                                                                    
