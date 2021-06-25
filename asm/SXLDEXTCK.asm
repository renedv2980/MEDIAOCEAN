*          DATA SET SXLDEXTCK  AT LEVEL 012 AS OF 01/14/99                      
*PHASE SXEXTCK,*                                                                
*INCLUDE HEXOUT                                                                 
         TITLE 'SXLDEXTCK - DELETE COKE INCH AND BUCH RECS FOR 99 ESTS'         
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
         NMOD1 (WORKX-WORKD),SXLDEXT                                            
         USING WORKD,RC                                                         
         EJECT                                                                  
* CONTROL FLOW LOGIC                                                            
*                                                                               
DMXCTL   ST    R1,APARM            SAVE PARAMETERS                              
         MVC   PLIST,0(R1)                                                      
         L     RA,VCPRINT          SET UP FOR PRINTING                          
         USING DPRINT,RA                                                        
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
         EJECT                                                                  
* INITIALISE LOGIC                                                              
*                                                                               
DMXINIT  DS    0H                                                               
*                                                                               
         B     DMXIT                                                            
*                                                                               
* PROCESS RECORD LOGIC                                                          
*                                                                               
DMXREC   DS    0H                                                               
         L     R3,AREC                                                          
         CLC   0(2,R3),=X'0E04'                                                 
         BE    DMXREC05                                                         
         CLC   0(2,R3),=X'0E05'                                                 
         BNE   DMXKEEP                                                          
*                                                                               
DMXREC05 MVC   BYTE,2(R3)                                                       
         NI    BYTE,X'F0'                                                       
         CLI   BYTE,X'B0'          IS IT COKE?                                  
         BNE   DMXKEEP                                                          
*                                                                               
DMXREC10 LA    R4,ESTTABLE                                                      
DMXREC20 CLI   0(R4),X'FF'                                                      
         BE    DMXKEEP                                                          
         CLC   7(1,R3),1(R4)                                                    
         BE    DMXREC30                                                         
         LA    R4,2(R4)                                                         
         B     DMXREC20                                                         
*                                                                               
DMXREC30 DS    0H                                                               
         BAS   RE,COKEPRNT         PRINT OUT SOME INFO OF THE RECORD            
         AP    RECDEL,=P'1'                                                     
         B     DMXPURGE                                                         
*                                                                               
         EJECT                                                                  
* END-OF-FILE LOGIC                                                             
*                                                                               
COKEPRNT NTR1                                                                   
         GOTO1 =V(HEXOUT),DMCB,0(R3),P+6,32        KEY                          
         EDIT  (B1,7(R3)),(3,P+2),FILL=0           ESTIMATE                     
         GOTO1 VPRINTER            PRINT OUT THE KEY AND THE ESTIMATE           
         XIT1                                                                   
*                                                                               
DMXEOF   DS    0H                                                               
         MVI   P,C' '                                                           
         MVC   P+1(L'P-1),P                                                     
         MVC   P+10(12),=C'DELETED RECS'                                        
         EDIT  (P4,RECDEL),(8,P),ZERO=NOBLANK                                   
         GOTO1 VPRINTER                                                         
*                                                                               
         B     DMXIT                                                            
         EJECT                                                                  
ESTTABLE DC    H'01'                                                            
         DC    H'02'                                                            
         DC    H'03'                                                            
         DC    H'04'                                                            
         DC    H'05'                                                            
         DC    H'07'                                                            
         DC    H'09'                                                            
         DC    H'10'                                                            
         DC    H'11'                                                            
         DC    H'12'                                                            
         DC    H'13'                                                            
         DC    H'15'                                                            
         DC    H'16'                                                            
         DC    H'17'                                                            
         DC    H'18'                                                            
         DC    H'19'                                                            
         DC    H'20'                                                            
         DC    H'21'                                                            
         DC    H'22'                                                            
         DC    H'23'                                                            
         DC    H'25'                                                            
         DC    H'26'                                                            
         DC    H'28'                                                            
         DC    H'30'                                                            
         DC    H'31'                                                            
         DC    H'32'                                                            
         DC    H'33'                                                            
         DC    H'34'                                                            
         DC    H'35'                                                            
         DC    H'36'                                                            
         DC    H'37'                                                            
         DC    H'38'                                                            
         DC    H'40'                                                            
         DC    H'41'                                                            
         DC    H'42'                                                            
         DC    H'43'                                                            
         DC    H'44'                                                            
         DC    H'45'                                                            
         DC    H'46'                                                            
         DC    H'47'                                                            
         DC    H'48'                                                            
         DC    H'49'                                                            
         DC    H'50'                                                            
         DC    H'51'                                                            
         DC    H'52'                                                            
         DC    H'53'                                                            
         DC    H'54'                                                            
         DC    H'55'                                                            
         DC    H'56'                                                            
         DC    H'57'                                                            
         DC    H'58'                                                            
         DC    H'59'                                                            
         DC    H'60'                                                            
         DC    H'61'                                                            
         DC    H'62'                                                            
         DC    H'63'                                                            
         DC    H'64'                                                            
         DC    H'65'                                                            
         DC    H'66'                                                            
         DC    H'67'                                                            
         DC    H'68'                                                            
         DC    H'69'                                                            
         DC    H'70'                                                            
         DC    H'71'                                                            
         DC    H'72'                                                            
         DC    H'73'                                                            
         DC    H'74'                                                            
         DC    H'75'                                                            
         DC    H'76'                                                            
         DC    H'77'                                                            
         DC    H'78'                                                            
         DC    H'79'                                                            
         DC    H'80'                                                            
         DC    H'81'                                                            
         DC    H'82'                                                            
         DC    H'83'                                                            
         DC    H'206'                                                           
         DC    X'FF'                                                            
*                                                                               
RECDEL   DC    PL4'0'              NUMBER OF RECORD BEING DELETED               
*                                                                               
         LTORG                                                                  
         SPACE 2                                                                
         EJECT                                                                  
WORKD    DSECT                                                                  
BYTE     DS    X                                                                
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
*                                                                               
WORK     DS    CL128                                                            
WORKX    DS    0C                                                               
         EJECT                                                                  
*DMLDDEFN                                                                       
       ++INCLUDE DMLDDEFN                                                       
         EJECT                                                                  
*DDDPRINT                                                                       
       ++INCLUDE DDDPRINT                                                       
         SPACE 2                                                                
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'012SXLDEXTCK 01/14/99'                                      
         END                                                                    
