*          DATA SET UNLDXSP4   AT LEVEL 005 AS OF 08/10/00                      
*          DATA SET UNLDXSP2   AT LEVEL 053 AS OF 01/24/96                      
*          DATA SET UNLDXSP    AT LEVEL 023 AS OF 11/16/93                      
*PHASE UNXSPTA                                                                  
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
*INCLUDE RECUP                                                                  
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE PRNTBL                                                                 
         TITLE 'UNXSPT - DELETE PROGRAM RECS WITH 77 ELEMENT'                   
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
DMXPURGE L     R1,APARM            PURGE RECORD EXIT                            
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
* THIS SI IT                                                                    
* *********************************************************************         
* PROCESS RECORD LOGIC - RECORD IN AREC                                         
DMXREC   DS    0H                                                               
         L     R6,AREC            POINT TO RECORD                               
         CLI   0(R6),X'12'                                                      
         BE    NPRINT05                                                         
         B     DMXPURGE                                                         
*                                                                               
NPRINT05 DS    0H                                                               
         USING RINVREC,R6                                                       
         CLC   RINVKREP,=C'UT'   NEW RECORD PURGE                               
         BNE   DMXPURGE                                                         
         CLC   COUNT,=F'200'                                                    
         BH    NPRINT10                                                         
         GOTO1 =V(PRNTBL),DMCB,=C'BEFO',AREC,C'DUMP',30,=C'1D'                  
NPRINT10 MVC   RINVKREP,=C'FU'   NEW RECORD PURGE                               
         CLC   COUNT,=F'200'                                                    
         BH    NPRINT20                                                         
         GOTO1 =V(PRNTBL),DMCB,=C'AFTR',AREC,C'DUMP',30,=C'1D'                  
NPRINT20 L     R1,COUNT                                                         
         LA    R1,1(R1)                                                         
         ST    R1,COUNT                                                         
         B     DMXKEEP                                                          
*                                                                               
********************************************************************            
*                                                                               
         MVC   WORK(4),=X'0000E301'                                             
         GOTO1 =V(HELLO),DMCB,(C'D',=C'SPTFILE '),(X'DD',AREC),        X        
               =X'0000E301',0                                                   
         MVC   WORK(4),=X'0000C801'                                             
         GOTO1 =V(HELLO),DMCB,(C'D',=C'SPTFILE '),(X'DD',AREC),        X        
               =X'0000C801',0                                                   
         MVC   WORK(4),=X'0000E394'                                             
         GOTO1 =V(HELLO),DMCB,(C'D',=C'SPTFILE '),(X'DD',AREC),        X        
               =X'0000E394',0                                                   
         MVC   WORK(4),=X'0000C894'                                             
         GOTO1 =V(HELLO),DMCB,(C'D',=C'SPTFILE '),(X'DD',AREC),        X        
               =X'0000C894',0                                                   
***      GOTO1 =V(PRNTBL),DMCB,=C'AFT',AREC,C'DUMP',500,=C'1D'                  
         B     DMXKEEP                                                          
*                                                                               
         EJECT                                                                  
* END-OF-FILE LOGIC - LAST CALL HERE - RETURN ALLOWED                           
*                                                                               
DMXEOF   DS    0H                                                               
         MVI   SPACING+3,C'2'      PRINT A HEADLINE FOR TOTALS                  
         GOTO1 VPRINTER                                                         
         MVI   SPACING+3,C'1'                                                   
         MVC   P+10(26),=C'SUMMARY OF RECORDS CHANGED'                          
         L     R4,COUNT                                                         
         EDIT  (R4),(10,P+37)                                                   
         GOTO1 VPRINTER                                                         
         B     DMXIT                                                            
         EJECT                                                                  
* *******************************************************************           
* VARIABLE LIST                                                                 
* *******************************************************************           
DATADISP DC    H'0024'                                                          
COUNT    DC    F'0'                                                             
ACOUNT   DC    F'0'                                                             
PCOUNT   DC    F'0'                                                             
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
         PRINT OFF                                                              
*DMLDDEFN                                                                       
       ++INCLUDE DMLDDEFN                                                       
         EJECT                                                                  
*DDDPRINT                                                                       
       ++INCLUDE DDDPRINT                                                       
         EJECT                                                                  
         PRINT ON                                                               
*SPGENPRD                                                                       
       ++INCLUDE REGENINVA                                                      
       ++INCLUDE REGENAVLNN                                                     
       ++INCLUDE REGENPRPNN                                                     
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'005UNLDXSP4  08/10/00'                                      
         END                                                                    
