*          DATA SET STEXTAB    AT LEVEL 009 AS OF 05/01/02                      
*PHASE STEXTAB,*                                                                
*INCLUDE MSUNPK                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE PRNTBL                                                                 
         TITLE 'DMLDEXT - LOAD/DUMP MODEL EXTERNAL ROUTINE'                     
* SPOTH MARKET FIX FOR BNNA RADIO STATIONS                                      
* SEP17/93                                                                      
         SPACE                                                                  
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
         NMOD1 20,DMLDEXT,R6,R7                                                 
         USING WORKD,RC                                                         
         EJECT                                                                  
*                                                                               
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
         SPACE                                                                  
         AP    TOTRD,=P'1'                                                      
         SPACE                                                                  
         USING STARECD,R3                                                       
         CLI   STAKTYPE,C'S'       STATION REC?                                 
         BNE   DMX10                                                            
*                                                                               
         CLI   STAKCLT,C'0'                                                     
         BNE   DMX5                                                             
         SPACE                                                                  
         CLI   STAKCALL,C'W'       START WITH W ?                               
         BE    DMXKEEP                                                          
         CLI   STAKCALL,C'K'                                                    
         BE    DMXKEEP                                                          
*                                                                               
DMX5     AP    SCTR,=P'1'                                                       
         BC    0,DMXPURGE                                                       
         OI    *-3,X'F0'                                                        
         BAS   RE,PRT                                                           
         B     DMXPURGE                                                         
*                                                                               
DMX10    CLI   STAKTYPE,C'A'       ADDRESS RECORD                               
         BNE   DMX20                                                            
         SPACE                                                                  
         CLI   STAKCALL,C'W'       START WITH W ?                               
         BE    DMXKEEP                                                          
         CLI   STAKCALL,C'K'                                                    
         BE    DMXKEEP                                                          
*                                                                               
         AP    ACTR,=P'1'                                                       
         BC    0,DMXPURGE                                                       
         OI    *-3,X'F0'                                                        
         BAS   RE,PRT                                                           
         B     DMXPURGE                                                         
*                                                                               
DMX20    CLI   STAKTYPE,C'N'       N PASSIVE                                    
         BNE   DMXKEEP                                                          
*                                                                               
         SPACE                                                                  
         CLI   STNKCLT,C'0'                                                     
         BNE   DMXPURGE                                                         
*                                                                               
         GOTO1 =V(MSUNPK),DMCB,STNKMS,WORK,WORK+4                               
         CLI   WORK+4,C'K'                                                      
         BE    DMXKEEP                                                          
         CLI   WORK+4,C'W'                                                      
         BE    DMXKEEP                                                          
*                                                                               
         AP    NCTR,=P'1'                                                       
         BC    0,DMXPURGE                                                       
         OI    *-3,X'F0'                                                        
         BAS   RE,PRT                                                           
         B     DMXPURGE                                                         
         SPACE                                                                  
         DROP  R3                                                               
         SPACE                                                                  
PRT      NTR1                                                                   
         LA    R4,=CL20'BYE BYE RECORD'                                         
         SR    R5,R5                                                            
         ICM   R5,3,15(R3)                                                      
         SPACE                                                                  
         GOTO1 =V(PRNTBL),DMCB,(20,(R4)),(R3),C'DUMP',(R5),=C'0D'               
         XIT1                                                                   
         SPACE                                                                  
*                                                                               
* END-OF-FILE LOGIC                                                             
*                                                                               
DMXEOF   DS    0H                                                               
         MVC   P(25),=C' THESE ARE THE COUNTERS'                                
         GOTO1 VPRINTER                                                         
         LA    R2,TOTCTRS                                                       
         LA    R3,TOTRD                                                         
DMXEOF10 MVC   P+5(28),5(R3)                                                    
         EDIT  (P5,0(R3)),(8,P+33)                                              
         GOTO1 VPRINTER                                                         
         LA    R3,33(,R3)                                                       
         BCT   R2,DMXEOF10                                                      
         SPACE                                                                  
         GOTO1 VPRINTER                                                         
         GOTO1 VPRINTER                                                         
         SPACE                                                                  
         B     DMXIT                                                            
         EJECT                                                                  
TOTRD    DC    PL5'0',CL28'TOT RECS READ'                                       
SCTR     DC    PL5'0',CL28'MASTER RECS'                                         
ACTR     DC    PL5'0',CL28'ADDRESS RECS'                                        
NCTR     DC    PL5'0',CL28'STUPID N RECS'                                       
TOTCTRS  EQU   (*-TOTRD)/33                                                     
          SPACE                                                                 
WORK     DS    CL64                                                             
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
         EJECT                                                                  
*DMLDDEFN                                                                       
       ++INCLUDE DMLDDEFN                                                       
         EJECT                                                                  
*DDDPRINT                                                                       
       ++INCLUDE DDDPRINT                                                       
         SPACE 2                                                                
STARECD   DSECT                                                                 
       ++INCLUDE SPGENSTA                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'009STEXTAB   05/01/02'                                      
         END                                                                    
