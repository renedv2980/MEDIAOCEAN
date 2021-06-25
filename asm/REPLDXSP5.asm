*          DATA SET REPLDXSP5  AT LEVEL 020 AS OF 08/31/00                      
*          DATA SET REPLDXSP5  AT LEVEL 019 AS OF 04/22/97                      
*PHASE UNXREP5A                                                                 
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
*INCLUDE RECUP                                                                  
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE PRNTBL                                                                 
         TITLE 'BLAIR: LOOK FOR A CERTAIN RECORD'                               
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
         SPACE 2                                                                
         PRINT NOGEN                                                            
UNLDXSP  CSECT                                                                  
         NMOD1 WORKX-WORKD,UNLDXSP,RR=R2                                        
         USING WORKD,RC                                                         
         EJECT                                                                  
* CONTROL FLOW LOGIC                                                            
*                                                                               
DMXCTL   ST    R1,APARM            SAVE PARAMETERS                              
         MVC   PLIST,0(R1)                                                      
         L     RA,VCPRINT          SET UP FOR PRINTING                          
         USING DPRINT,RA                                                        
         SPACE 2                                                                
         CLI   PLIST,X'00'         FIRST CALL TO INITILISE                      
         BE    DMXINIT                                                          
         CLI   PLIST,X'01'         NORMAL CALL TO PROCESS RECORD                
         BE    DMXREC                                                           
         CLI   PLIST,X'FF'         LAST CALL ON EOF                             
         BE    DMXEOF                                                           
         B     DMXIT                                                            
         SPACE 1                                                                
DMXKEEP  L     R1,APARM            KEEP RECORD EXIT                             
         MVI   0(R1),0                                                          
         MVI   8(R1),0                                                          
         B     DMXIT                                                            
         SPACE 1                                                                
DMXPURGE GOTO1 =V(PRNTBL),DMCB,=C'BYPS',AREC,C'DUMP',30,=C'1D'                  
         L     R1,APARM            PURGE RECORD EXIT                            
         MVI   0(R1),X'FF'                                                      
         MVI   8(R1),0                                                          
         B     DMXIT                                                            
         SPACE 1                                                                
DMXPGEOF L     R1,APARM            PURGE RECORD EXIT                            
         MVI   0(R1),X'FF'                                                      
         MVC   1(3,R1),=C'EOF'                                                  
         B     DMXIT                                                            
         SPACE 1                                                                
DMXIT    XMOD1 1                                                                
         EJECT                                                                  
* INITIALISE LOGIC - FIRST CALL HERE - NO RETURN ALLOWED                        
*                                                                               
DMXINIT  DS    0H                                                               
         B     DMXIT                                                            
         EJECT                                                                  
*                                                                               
* *********************************************************************         
* THIS IS IT                                                                    
* *********************************************************************         
* PROCESS RECORD LOGIC - RECORD IN AREC                                         
DMXREC   DS    0H                                                               
         USING RINVREC,R6                                                       
         L     R6,AREC            POINT TO RECORD                               
         CLI   0(R6),X'12'         CHECK FOR INVENTORY                          
         BNE   DMXKEEP                                                          
         CLC   RINVKREP,=CL2'BL'   CHECK FOR AGENCY                             
         BNE   DMXKEEP                                                          
         CLC   RINVKSTA,=CL5'KWYBT' CHECK FOR STATION                           
         BNE   DMXKEEP                                                          
         CLC   RINVKINV,=CL4'0200'  CHECK FOR INVENTORY NUMBER                  
         BNE   DMXKEEP                                                          
         CLC   RINVKSTD,=XL3'610318' CHECK FOR EFFECTIVE DATE                   
         BNE   DMXKEEP                                                          
         CLI   RINVKSRC,X'D9'        CHECK FOR KEY SOURCE                       
         BNE   DMXKEEP                                                          
         CLC   RINVKBK,=X'6105'      CHECK FOR BOOK                             
         BNE   DMXKEEP                                                          
         EJECT                                                                  
*                                                                               
** PRINT RECORD **                                                              
*                                                                               
         DS    0H                                                               
         ZICM  R0,RINVLEN,(3)                                                   
         GOTO1 =V(PRNTBL),DMCB,=C'BEFO',RINVREC,C'DUMP',(R0),=C'1D'             
                                                                                
*                                                                               
         B     DMXKEEP                                                          
         DROP  R6                                                               
         EJECT                                                                  
* END-OF-FILE LOGIC - LAST CALL HERE - RETURN ALLOWED                           
*                                                                               
DMXEOF   DS    0H                                                               
         B     DMXIT                                                            
         EJECT                                                                  
* *******************************************************************           
* VARIABLE LIST                                                                 
* *******************************************************************           
DATADISP DC    H'0024'                                                          
                                                                                
                                                                                
CNTAM    DC    F'0'                COUNTS # OF KAMNY RECORDS                    
CNTCQK   DC    F'0'                COUNTS # OF KCONYR/KLJB RECORDS              
CNTCQW   DC    F'0'                COUNTS # OF KCONYR/WAOW RECORDS              
CNTCQ    DC    F'0'                COUNTS # OF KCONYR RECORDS                   
CNTHDR   DC    F'0'                COUNTS # OF HEADERS                          
CNTMKTF  DC    F'0'                COUNTS # OF MARKET FACTS                     
CNTSTAF  DC    F'0'                COUNTS # OF STATION FACTS                    
CNTTRK   DC    F'0'                COUNTS # OF TRACK RECORDS                    
CNTRTNL  DC    F'0'                COUNTS # OF RATIONALES                       
CNTRECHG DC    F'0'                COUNTS # OF RECORDS CHANGED                  
*                                                                               
         SPACE                                                                  
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
         SPACE                                                                  
         LTORG                                                                  
         SPACE 2                                                                
WORKD    DSECT                                                                  
DUB      DS    D                                                                
DMCB     DS    6F                                                               
APARM    DS    A                                                                
*                                                                               
PLIST    DS    0CL24                                                            
AREC     DS    A                                                                
VTAPEOUT DS    A                                                                
APARAMC  DS    A                                                                
VLDDEFN  DS    A                                                                
VPRINTER DS    A                                                                
VCPRINT  DS    A                                                                
*                                                                               
ELCODE   DS    CL1                                                              
BYTE     DS    CL1                                                              
WORK     DS    CL100                                                            
WORK2    DS    CL100                                                            
HALF     DS    H                                                                
WORKX    EQU   *                                                                
*                                                                               
         EJECT                                                                  
*DMLDDEFN                                                                       
         PRINT OFF                                                              
       ++INCLUDE DMLDDEFN                                                       
         PRINT ON                                                               
*DDDPRINT                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDDPRINT                                                       
         PRINT ON                                                               
         EJECT                                                                  
       ++INCLUDE REGENINVA                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'020REPLDXSP5 08/31/00'                                      
         END                                                                    
