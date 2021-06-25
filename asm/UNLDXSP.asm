*          DATA SET UNLDXSP    AT LEVEL 072 AS OF 08/10/00                      
*PHASE UNXSPXZA                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE PRNTBL                                                                 
         TITLE 'UNXSPT - MOVE SPOT RECS'                                        
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
* MOVE PROGRAM REC                                                              
* *********************************************************************         
* PROCESS RECORD LOGIC - RECORD IN AREC                                         
DMXREC   DS    0H                                                               
         L     R3,AREC                                                          
         USING CLSKEY,R3                                                        
         CLI   0(R3),8                                                          
         BE    PRINTIT                                                          
         CLI   0(R3),9                                                          
         BE    PRINTIT                                                          
         B     DMXKEEP                                                          
PRINTIT  GOTO1 =V(PRNTBL),DMCB,=C'REC',0(R3),C'DUMP',60,=C'1D'                  
         B     DMXKEEP                                                          
*                                                                               
         CLC   CLSKSTA,=X'AE0CA3'  TNT                                          
         BNE   *+14                                                             
         MVC   CLSKMKT,=X'0010'    MAKE IT MARKET 16                            
         B     DMX50                                                            
*                                                                               
         CLC   CLSKSTA,=X'76BD83'  NIKS                                         
         BNE   DMX50                                                            
         MVC   CLSKMKT,=X'0011'     MAKE IT MARKET 17                           
                                                                                
DMX50    L     R1,COUNT                                                         
         LA    R1,1(R1)                                                         
         ST    R1,COUNT                                                         
         GOTO1 =V(PRNTBL),DMCB,=C'RC2',0(R3),C'DUMP',40,=C'1D'                  
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
                                                                                
TRANSTBL DS    0H                 JWNY  OMNY                                    
         DC    AL2(1),AL2(1)       ABC                                          
         DC    AL2(3),AL2(3)       NBC                                          
         DC    AL2(5),AL2(1020)    FOX                                          
         DC    AL2(150),AL2(1316)  TPE  RYS                                     
         DC    AL2(69),AL2(5)      IAS  PAS                                     
         DC    AL2(351),AL2(6)     TCF  TWT                                     
         DC    AL2(2),AL2(2)       CBS                                          
         DC    AL2(27),AL2(1200)   BVT  BVTV                                    
         DC    X'0000',X'0000'     END OF FILE                                  
         EJECT                                                                  
* *******************************************************************           
* VARIABLE LIST                                                                 
* *******************************************************************           
DATADISP DC    H'0024'                                                          
COUNT    DC    F'0'                                                             
COUNTER  DC    F'30'                                                            
*                                                                               
         SPACE                                                                  
*                                                                               
         GETEL R3,DATADISP,ELCODE                                               
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
WORK     DS    CL64                                                             
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
*SPGENPROG                                                                      
       ++INCLUDE SPGENPROG                                                      
       ++INCLUDE SPGENSTA                                                       
       ++INCLUDE SPGENBILL                                                      
       ++INCLUDE SPGENCLRST                                                     
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'072UNLDXSP   08/10/00'                                      
         END                                                                    
