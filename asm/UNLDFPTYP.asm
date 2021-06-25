*          DATA SET UNLDFPTYP  AT LEVEL 001 AS OF 09/03/02                      
*PHASE UNLDFP,+0                                                                
*INCLUDE DATCON                                                                 
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
*INCLUDE GETDAY                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE PRNTBL                                                                 
*INCLUDE HEXOUT                                                                 
         TITLE 'UNLDEX - LOAD/DUMP MODEL EXTERNAL ROUTINE'                      
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
UNLDEXT  CSECT                                                                  
         NMOD1 WORKX-WORKD,*UNLDEXT,RR=R5                                       
         USING WORKD,RC                                                         
         EJECT                                                                  
* CONTROL FLOW LOGIC                                                            
*                                                                               
UNXCTL   ST    R1,APARM            SAVE PARAMETERS                              
         MVC   PLIST,0(R1)                                                      
         L     RA,VCPRINT          SET UP FOR PRINTING                          
         USING DPRINT,RA                                                        
         SPACE 1                                                                
         ST    R5,RELO                                                          
         L     RE,=V(DATCON)                                                    
         AR    RE,R5                                                            
         ST    RE,VDATCON                                                       
         L     RE,=V(GETDAY)                                                    
         AR    RE,R5                                                            
         ST    RE,VGETDAY                                                       
         L     RE,=V(PRNTBL)                                                    
         AR    RE,R5                                                            
         ST    RE,VPRNTBL                                                       
         L     RE,=V(HEXOUT)                                                    
         AR    RE,R5                                                            
         ST    RE,VHEXOUT                                                       
         SPACE 2                                                                
         CLI   PLIST,X'00'                                                      
         BE    UNINIT              INITIALISE                                   
         CLI   PLIST,X'01'                                                      
         BE    UNXREC              PROCESS                                      
         CLI   PLIST,X'FF'                                                      
         BE    UNXEOF              END-OF-FILE                                  
         B     UNXIT                                                            
         SPACE 2                                                                
UNXKEEP  L     R1,APARM            KEEP RECORD EXIT                             
         MVI   0(R1),0                                                          
***      GOTO1 VHEXOUT,DMCB,(R3),P,50                                           
****     GOTO1 VPRINTER                                                         
         B     UNXIT                                                            
         SPACE 2                                                                
UNXPURGE L     R1,APARM            PURGE RECORD EXIT                            
         MVI   0(R1),X'FF'                                                      
         B     UNXIT                                                            
         SPACE 2                                                                
UNXIT    XMOD1 1                                                                
         EJECT                                                                  
*************************************************************                   
* INITIALIZE LOGIC                                          *                   
*************************************************************                   
         SPACE                                                                  
UNINIT   B     UNXIT                                                            
         EJECT                                                                  
*******************************************************************             
* PROCESS RECORD LOGIC                                                          
****************************************************************                
UNXREC   DS    0H                                                               
         L     R3,AREC             POINT TO RECORD                              
         CLI   0(R3),X'04'         IS IT UNIT                                   
         BNE   UNXKEEP                                                          
         CLI   1(R3),X'D3'         AGENCY GSTX                                  
         BNE   UNXKEEP                                                          
         CLC   2(2,R3),=X'BF2B'     CLIENT PZL                                  
         BNE   UNXKEEP                                                          
         CLI   17(R3),X'05'        ESTIMATE 5                                   
         BE    UNX100                                                           
         CLI   17(R3),X'06'        ESTIMATE 6                                   
         BE    UNX100                                                           
         CLI   17(R3),X'07'        ESTIMATE 7                                   
         BNE   UNXKEEP                                                          
*                                                                               
* - UNIT RECORD                                                                 
UNX100   DS    0H                                                               
         GOTO1 =V(HELLO),DMCB,(C'G',UNTFILE),(X'10',AREC),0                     
         CLI   12(R1),0                                                         
         BNE   UNXKEEP                                                          
         L     R4,12(R1)                                                        
         USING NUBILD,R4                                                        
         GOTO1 =V(PRNTBL),DMCB,=C'KEY',AREC,C'DUMP',200,=C'1D'                  
UNX200   CLI   NUBILPRD,X'02'      PROD J07                                     
         BNE   UNX300                                                           
         GOTO1 =V(PRNTBL),DMCB,=C'BEFORE',0(R4),C'DUMP',28,=C'1D'               
         MVI   NUBILPRD,X'0F'      CHANGE TO PROD J7                            
         GOTO1 =V(PRNTBL),DMCB,=C'AFTER',0(R4),C'DUMP',28,=C'1D'                
UNX300   ZIC   RF,1(R4)                                                         
         AR    R4,RF                                                            
         CLI   0(R4),X'10'                                                      
         BE    UNX200                                                           
         DROP  R4                                                               
*  MARK STATUS BIT                                                              
         GOTO1 =V(HELLO),DMCB,(C'G',UNTFILE),(X'02',AREC),0                     
         CLI   12(R1),0                                                         
         BNE   UNX400                                                           
         L     R4,12(R1)                                                        
         USING NUSDRD,R4                                                        
         GOTO1 =V(PRNTBL),DMCB,=C'FFFFFFFF',0(R4),C'DUMP',20,=C'1D'             
         OI    NUACT2WY,X'01'                                                   
         GOTO1 =V(PRNTBL),DMCB,=C'ACTIVITY',0(R4),C'DUMP',20,=C'1D'             
UNX400   GOTO1 =V(PRNTBL),DMCB,=C'REC',AREC,C'DUMP',200,=C'1D'                  
         B     UNXKEEP                                                          
         DROP  R4                                                               
***********************************************8                                
*                                                                               
* END-OF-FILE PROCESSING                                                        
*                                                                               
UNXEOF   DS    0H                                                               
         MVC   P(12),=C'RECS CHANGED'                                           
         EDIT  (B4,COUNT),(8,P+13),ALIGN=LEFT                                   
         GOTO1 VPRINTER                                                         
         B     UNXIT                                                            
         EJECT                                                                  
* GETEL MACRO                                                                   
*                                                                               
         GETEL R3,27,ELCODE                                                     
         SPACE 2                                                                
* LITERAL POOL                                                                  
*                                                                               
UNTFILE  DC    CL8'UNTFILE'                                                     
         LTORG                                                                  
         SPACE 2                                                                
PKGCNT   DC    F'0'                                                             
UNTCNT   DC    F'0'                                                             
COUNT    DC    F'0'                                                             
         EJECT                                                                  
WORKD    DSECT                                                                  
DUB      DS    D                                                                
DMCB     DS    6F                                                               
RELO     DS    A                                                                
APARM    DS    A                                                                
PLIST    DS    0CL24                                                            
AREC     DS    A                                                                
VTAPEOUT DS    A                                                                
APARAMC  DS    A                                                                
VLDDEFN  DS    A                                                                
VPRINTER DS    A                                                                
VCPRINT  DS    A                                                                
         SPACE 1                                                                
VDATCON  DS    A                                                                
VGETDAY  DS    A                                                                
VPRNTBL  DS    A                                                                
VHEXOUT  DS    A                                                                
FULL     DS    F                                                                
HALF     DS    H                                                                
STDATE   DS    XL2                                                              
ELCODE   DS    CL1                                                              
WORK     DS    CL64                                                             
WORKX    EQU   *                                                                
         EJECT                                                                  
       ++INCLUDE DDDPRINT                                                       
         EJECT                                                                  
       ++INCLUDE NEGENPACK                                                      
         EJECT                                                                  
       ++INCLUDE NEGENUNIT                                                      
       ++INCLUDE NEGENPUA                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'001UNLDFPTYP 09/03/02'                                      
         END                                                                    
