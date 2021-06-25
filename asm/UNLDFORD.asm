*          DATA SET UNLDFORD   AT LEVEL 032 AS OF 08/10/00                      
*          DATA SET UNLDXSP2   AT LEVEL 081 AS OF 05/08/97                      
*          DATA SET UNLDXSP    AT LEVEL 023 AS OF 11/16/93                      
*PHASE UNXFRDA                                                                  
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
*INCLUDE RECUP                                                                  
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE PRNTBL                                                                 
         TITLE 'UNXSPT - CONVERT JWNY TO FORD'                                  
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
*        GOTO1 =V(PRNTBL),DMCB,=C'BYPS',AREC,C'DUMP',30,=C'1D'                  
DMXPURGE L     R1,APARM            PURGE RECORD EXIT                            
         MVI   0(R1),X'FF'                                                      
         MVI   8(R1),0                                                          
*        DC    H'0'                                                             
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
         CLI   0(R6),X'02'                                                      
         BE    PACK                                                             
         CLI   0(R6),X'04'                                                      
         BE    UNIT                                                             
         B     DMXKEEP                                                          
*                                                                               
*                                                                               
PACK     DS    0H                                                               
         USING NPRECD,R6                                                        
         CLI   NPKAM,X'63'       AGENCY YNR                                     
         BNE   DMXKEEP                                                          
         CLC   NPKCLT,=XL2'8977'       CLIENT CLX                               
         BE    DMXPURGE                                                         
         CLC   NPKCLT,=XL2'BDC1'       CLIENT POB                               
         BE    DMXPURGE                                                         
*        GOTO1 =V(PRNTBL),DMCB,=C'AFT',AREC,C'DUMP',130,=C'1D'                  
         B     DMXKEEP                                                          
         DROP  R6                                                               
*                                                                               
*                                                                               
UNIT     DS    0H                                                               
         USING NURECD,R6                                                        
         CLI   NUKAM,X'63'       AGENCY YNR                                     
         BNE   DMXKEEP                                                          
         CLC   NUKCLT,=XL2'8977'       CLIENT CLX                               
         BE    DMXPURGE                                                         
         CLC   NUKCLT,=XL2'BDC1'       CLIENT POB                               
         BE    DMXPURGE                                                         
*        GOTO1 =V(PRNTBL),DMCB,=C'AFT',AREC,C'DUMP',130,=C'1D'                  
         B     DMXKEEP                                                          
         DROP  R6                                                               
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
PKCOUNT  DC    F'0'                                                             
UNCOUNT  DC    F'0'                                                             
PLCOUNT  DC    F'0'                                                             
PRCOUNT  DC    F'0'                                                             
ACOUNT   DC    F'0'                                                             
PCOUNT   DC    F'0'                                                             
COUNT    DC    F'0'                                                             
*                                                                               
PUPTAB   DC    CL8'ABC 8PRM'                                                    
         DC    CL8'ABC 8NWS'                                                    
         DC    CL8'ABC 8MAZ'                                                    
         DC    CL8'ABC 8NMZ'                                                    
         DC    CL8'ABC 8EMZ'                                                    
         DC    CL8'ABC 8LMD'                                                    
*                                                                               
         DC    CL8'CBS 8FD '                                                    
         DC    CL8'CBS LATE'                                                    
         DC    CL8'CBS 8PRM'                                                    
         DC    CL8'CBS 8MAZ'                                                    
         DC    CL8'CBS 8LAT'                                                    
         DC    CL8'CBS 8LMD'                                                    
*                                                                               
         DC    CL8'NBC 8PRM'                                                    
         DC    CL8'NBC 8LAT'                                                    
         DC    CL8'NBC 8MAZ'                                                    
         DC    CL8'NBC MAZ '                                                    
*                                                                               
         DC    CL8'FOX 8PRM'                                                    
         DC    CL8'FOX B8PR'                                                    
         DC    CL8'FOX 8MAZ'                                                    
         DC    X'FF'                                                            
*                                                                               
UNITTAB  DC    XL1'52',CL4'ARTS',XL1'01',XL4'C33EC39F'                          
         DC    XL1'52',CL4'CCL ',XL1'01',XL4'C33DC39F'                          
         DC    XL1'52',CL4'CMT ',XL1'01',XL4'C33DC39F'                          
         DC    XL1'52',CL4'CNN ',XL1'01',XL4'C33DC39F'                          
         DC    XL1'52',CL4'CNN ',XL1'02',XL4'C341C39F'                          
         DC    XL1'52',CL4'CNII',XL1'01',XL4'C33DC39F'                          
         DC    XL1'52',CL4'ESPN',XL1'01',XL4'C33DC39F'                          
         DC    XL1'52',CL4'ESPN',XL1'02',XL4'C33DC37E'                          
         DC    XL1'52',CL4'ESPN',XL1'03',XL4'C361C37E'                          
         DC    XL1'52',CL4'FAM ',XL1'01',XL4'C341C37E'                          
         DC    XL1'52',CL4'HGN ',XL1'01',XL4'C341C39F'                          
         DC    XL1'52',CL4'LIF ',XL1'01',XL4'C346C39F'                          
         DC    XL1'52',CL4'MTV ',XL1'01',XL4'C341C39F'                          
         DC    XL1'52',CL4'ORL ',XL1'01',XL4'C341C35F'                          
         DC    XL1'52',CL4'TBS ',XL1'01',XL4'C341C39F'                          
         DC    XL1'52',CL4'TBS ',XL1'02',XL4'C33DC39F'                          
         DC    XL1'52',CL4'TDC ',XL1'01',XL4'C33DC39F'                          
         DC    XL1'52',CL4'TNN ',XL1'01',XL4'C341C39F'                          
         DC    XL1'52',CL4'TNT ',XL1'01',XL4'C341C39F'                          
         DC    XL1'52',CL4'USA ',XL1'01',XL4'C33EC39F'                          
         DC    XL1'52',CL4'HCN ',XL1'01',XL4'0000FFFF'                          
*                                                                               
         DC    XL1'55',CL4'ABC ',XL1'01',XL4'C321C441'                          
         DC    XL1'55',CL4'ABC ',XL1'03',XL4'C321C39F'                          
         DC    XL1'55',CL4'ABC ',XL1'02',XL4'C321C441'                          
         DC    XL1'55',CL4'ABC ',XL1'05',XL4'C399C421'                          
         DC    XL1'55',CL4'ABC ',XL1'04',XL4'C321C39F'                          
         DC    XL1'55',CL4'FOX ',XL1'01',XL4'C321C42B'                          
*                                                                               
         DC    XL1'58',CL4'CBS ',XL1'01',XL4'C341C37E'                          
         DC    XL1'58',CL4'CBS ',XL1'02',XL4'C341C35E'                          
*                                                                               
         DC    XL1'51',CL4'ESN ',XL1'01',XL4'C361C422'                          
         DC    XL1'51',CL4'ESN ',XL1'04',XL4'C361C422'                          
         DC    XL1'51',CL4'ESN ',XL1'03',XL4'C361C422'                          
         DC    XL1'51',CL4'ESN ',XL1'05',XL4'C33CC35F'                          
         DC    XL1'51',CL4'ESN ',XL1'07',XL4'C381C39F'                          
         DC    XL1'51',CL4'ESN ',XL1'09',XL4'C381C39F'                          
         DC    XL1'51',CL4'ESN ',XL1'08',XL4'C381C39F'                          
         DC    XL1'51',CL4'EEP ',XL1'01',XL4'C33CC343'                          
         DC    X'FF'                                                            
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
INV      DS    CL3                                                              
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
       ++INCLUDE NEGENPACK                                                      
       ++INCLUDE NEGENUNIT                                                      
       ++INCLUDE NEGENPLAN                                                      
       ++INCLUDE NEGENPUA                                                       
       ++INCLUDE REGENAVLNN                                                     
       ++INCLUDE REGENPRPNN                                                     
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'032UNLDFORD  08/10/00'                                      
         END                                                                    
