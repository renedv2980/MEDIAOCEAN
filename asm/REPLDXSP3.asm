*          DATA SET REPLDXSP3  AT LEVEL 010 AS OF 08/31/00                      
*          DATA SET REPLDXSP3  AT LEVEL 009 AS OF 05/24/96                      
*PHASE UNXREP3A                                                                 
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
* THIS SI IT                                                                    
* *********************************************************************         
* PROCESS RECORD LOGIC - RECORD IN AREC                                         
DMXREC   DS    0H                                                               
         USING RINVREC,R6                                                       
         L     R6,AREC            POINT TO RECORD                               
         CLI   0(R6),X'12'         CHECK FOR INVENTORY                          
         BNE   DMXKEEP                                                          
         CLI   RINVKSRC,0          CHECK FOR A HEADER                           
         BNE   DMXKEEP                                                          
         CLI   RINVKINV+3,0        CHECK FOR OLD RECORDS                        
         BE    DMXKEEP                                                          
*        CLC   RINVKREP,=CL2'KH'   CHECK FOR REPDEMO                            
*        BNE   DMXKEEP                                                          
*                                                                               
*        GOTO1 =V(PRNTBL),DMCB,=C'BEFO',AREC,C'DUMP',500,=C'1D'                 
*                                                                               
FIX100   GOTO1 =V(HELLO),DMCB,(C'G',=C'REPFILE '),(X'06',AREC),0                
         CLI   12(R1),0            TEST IF AVAIL ELEMENT FOUND                  
         BNE   DMXKEEP             NO                                           
         L     R3,12(R1)                                                        
*                                                                               
         CLI   1(R3),27            CHECK FOR NEW LENGTH                         
         BE    FIX200                                                           
*                                                                               
FIX150   MVC   WORK2(7),2(R3)                                                   
         XC    WORK,WORK                                                        
         MVC   WORK(2),=XL2'061B'  SET NEW LENGTH                               
         MVC   WORK+2(2),RINVKREP  MOVE REP CODE IN                             
         MVC   WORK+4(23),2(R3)    MOVE REST OF ELEMENT IN                      
* - DELETE THE OLD ELEMENT                                                      
      GOTO1 =V(HELLO),DMCB,(C'D',=C'REPFILE '),(X'06',AREC),(7,WORK2)           
* - ADD ELEMENT                                                                 
         GOTO1 =V(HELLO),DMCB,(C'P',=C'REPFILE '),AREC,WORK                     
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         B     FIX100                                                           
*                                                                               
FIX200   ZIC   RE,1(R3)                                                         
         AR    R3,RE                                                            
         CLI   0(R3),6                                                          
         BNE   FIX300                                                           
         CLI   1(R3),27            IS NEW LENGTH SET                            
         BE    FIX200              YES GET NEXT ELEMENT                         
         B     FIX150                                                           
*                                                                               
* FIX300 GOTO1 =V(PRNTBL),DMCB,=C'AFTR',AREC,C'DUMP',500,=C'1D'                 
FIX300   B     DMXKEEP                                                          
*                                                                               
         EJECT                                                                  
APRINT10 DS    0H                                                               
         L     R6,AREC            POINT TO RECORD                               
         USING RAVLREC,R6                                                       
*                                                                               
         CLI   RAVLKDET,0                                                       
         BE    DMXKEEP             NOT A DETAIL BYPASS                          
*                                                                               
         CLC   ACOUNT,=F'20'                                                    
         BH    APRINT20                                                         
         GOTO1 =V(PRNTBL),DMCB,=C'ABEF',AREC,C'DUMP',250,=C'1D'                 
*                                                                               
APRINT20 MVC   WORK2(98),RAVLDEL                                                
         MVC   RAVLDATE(92),WORK2+5                                             
*                                                                               
         EDIT  (B1,RAVLDINV),(2,HALF),FILL=0                                    
         MVC   RAVLDINV(2),HALF                                                 
         MVC   RAVLDINV+2(2),WORK2+3                                            
*                                                                               
         CLC   ACOUNT,=F'20'                                                    
         BH    APRINT30                                                         
         GOTO1 =V(PRNTBL),DMCB,=C'AVAI',AREC,C'DUMP',250,=C'1D'                 
*                                                                               
APRINT30 L     RF,ACOUNT                                                        
         LA    RF,1(RF)                                                         
         ST    RF,ACOUNT                                                        
         B     DMXKEEP                                                          
         DROP  R6                                                               
*                                                                               
         EJECT                                                                  
PPRINT10 DS    0H                                                               
         L     R6,AREC            POINT TO RECORD                               
         USING RPRPREC,R6                                                       
*                                                                               
         CLI   RPRPKDET,0                                                       
         BE    DMXKEEP             NOT A DETAIL BYPASS                          
*                                                                               
         CLC   PCOUNT,=F'20'                                                    
         BH    PPRINT20                                                         
         GOTO1 =V(PRNTBL),DMCB,=C'PBEF',AREC,C'DUMP',150,=C'1D'                 
*                                                                               
PPRINT20 MVC   WORK2(22),RPRPDEL                                                
         MVC   RPRPDATE(16),WORK2+5                                             
*                                                                               
         EDIT  (B1,RPRPDINV),(2,HALF),FILL=0                                    
         MVC   RPRPDINV(2),HALF                                                 
         MVC   RPRPDINV+2(2),WORK2+3                                            
*                                                                               
         CLC   PCOUNT,=F'20'                                                    
         BH    PPRINT30                                                         
         GOTO1 =V(PRNTBL),DMCB,=C'PRIN',AREC,C'DUMP',150,=C'1D'                 
*                                                                               
PPRINT30 L     RF,PCOUNT                                                        
         LA    RF,1(RF)                                                         
         ST    RF,PCOUNT                                                        
         B     DMXKEEP                                                          
         DROP  R6                                                               
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
**PAN#1  DC    CL21'010REPLDXSP3 08/31/00'                                      
         END                                                                    
