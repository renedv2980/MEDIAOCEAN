*          DATA SET UNLDEXTC   AT LEVEL 140 AS OF 03/19/03                      
*PHASE UNLDEXTC,+0                                                              
*INCLUDE PRINT                                                                  
*INCLUDE PRNTBL                                                                 
*INCLUDE HEXOUT                                                                 
*                                                                               
         TITLE 'UNLDEXTC - COPY ALL NET UNIT RECORDS'                           
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
         L     RE,=V(PRNTBL)                                                    
         AR    RE,R5                                                            
         ST    RE,VPRNTBL                                                       
         SPACE 2                                                                
         CLI   PLIST,X'00'                                                      
         BNE   *+14                                                             
         XC    COUNT,COUNT                                                      
         B     UNXIT               INITIALISE                                   
         CLI   PLIST,X'01'                                                      
         BE    UNXREC              PROCESS                                      
         CLI   PLIST,X'FF'                                                      
         BE    UNXEOF              END-OF-FILE                                  
         B     UNXIT                                                            
         SPACE 2                                                                
UNXKEEP  L     R1,APARM            KEEP RECORD EXIT                             
         MVI   0(R1),0                                                          
         B     UNXIT                                                            
         SPACE 2                                                                
UNXPURGE L     R1,APARM            PURGE RECORD EXIT                            
         MVI   0(R1),X'FF'                                                      
         B     UNXIT                                                            
         SPACE 2                                                                
UNXIT    XMOD1 1                                                                
         EJECT                                                                  
*******************************************************************             
* PROCESS RECORD LOGIC                                                          
*******************************************************************             
UNXREC   DS    0H                                                               
         L     R3,AREC                                                          
         LA    R4,NETDEFN          NET RECORD TABLES                            
         USING RECDEFD,R4                                                       
*                                                                               
UNX50    DS    0H                                                               
         TM    RECINDS,X'80'       NOT IN TABLE?                                
         BZ    UNX55                                                            
*                                                                               
         GOTO1 VPRNTBL,DMCB,=C'ERROR',AREC,C'DUMP',20,=C'1D'                    
         B     UNXPURGE                                                         
*                                                                               
UNX55    DS    0H                                                               
         CLC   0(2,R3),RECKEYL     TEST KEY RANGE                               
         BL    UNX60                                                            
         CLC   0(2,R3),RECKEYH                                                  
         BH    UNX60                                                            
         B     UNX70                                                            
*                                                                               
UNX60    DS    0H                                                               
         AHI   R4,RECLEN           BUMP TO NEXT RECORD TYPE                     
         B     UNX50                                                            
*                                                                               
UNX70    DS    0H                                                               
         L     R5,AREC             CHECK CLIENT FILTER                          
         CLI   RECCLDSP,0          CLIENT IN KEY?                               
         BE    UNX80                                                            
*                                                                               
         ZIC   RF,RECCLDSP                                                      
         AR    R5,RF                                                            
*                                                                               
         OC    0(2,R5),0(R5)       IF NO CLIENT, KEEP IT                        
         BZ    UNX80                                                            
         CLC   0(2,R5),CLTFILTL                                                 
         BNE   UNXPURGE                                                         
*                                                                               
UNX80    DS    0H                                                               
         L     R5,AREC             CHECK ESTIMATE FILTER                        
         CLI   RECESDSP,0          ESTIMATE IN KEY?                             
         BE    UNX90                                                            
*                                                                               
         ZIC   RF,RECESDSP                                                      
         AR    R5,RF                                                            
*                                                                               
         CLI   0(R5),0             IF NO ESTIMATE, KEEP IT                      
         BE    UNX90                                                            
*                                                                               
         LA    R6,ESTFLT           ESTIMATE FILTER TABLE                        
*                                                                               
UNX85    DS    0H                                                               
         CLC   0(2,R6),=X'FFFF'    END OF TABLE?                                
         BE    UNXPURGE                                                         
*                                                                               
         CLC   0(1,R5),0(R6)                                                    
         BE    UNX90                                                            
         LA    R6,1(R6)                                                         
         B     UNX85                                                            
*                                                                               
UNX90    DS    0H                                                               
         L     R5,AREC             CONVERT AGY VALUE IN KEY                     
         ZIC   RF,RECAGDSP                                                      
         AR    R5,RF                                                            
*                                                                               
         CLI   RECAGTYP,AGYL                                                    
         BNE   UNX100                                                           
*                                                                               
*!!!!    GOTO1 VPRNTBL,DMCB,=C'OLD REC',AREC,C'DUMP',20,=C'1D'                  
*                                                                               
         NI    0(R5),X'FF'-OLDAGY  TURN OFF OLD AGENCY BIT                      
         OI    0(R5),NEWAGY        TURN ON NEW AGANCY BIT                       
*                                                                               
UNX100   DS    0H                                                               
         L     R5,AREC                                                          
         CLI   0(R5),X'04'         UNIT RECORD?                                 
         BNE   UNX150                                                           
         USING NURECD,R5                                                        
*                                                                               
         CLC   NUALPHA,=C'DU'                                                   
         BNE   UNX150                                                           
         MVC   NUALPHA,=C'DR'                                                   
         DROP  R5                                                               
*                                                                               
UNX150   DS    0H                                                               
         GOTO1 VPRNTBL,DMCB,=C'NEW REC',AREC,C'DUMP',20,=C'1D'                  
*                                                                               
UNX200   DS    0H                                                               
         L     RF,COUNT                                                         
         LA    RF,1(RF)                                                         
         ST    RF,COUNT                                                         
*                                                                               
         B     UNXKEEP                                                          
*                                                                               
UNXEOF   DS    0H                                                               
         MVC   P(18),=C'# OF RECS CHANGED:'                                     
         EDIT  COUNT,(10,P+20)                                                  
         GOTO1 VPRINTER                                                         
         B     UNXIT                                                            
*                                                                               
NETDEFN  DS    0X                                                               
         DC    X'00000000',AL1(00,00),AL1(01),X'40',AL3(0)                      
         DC    AL1(0,0),AL1(0,0),AL1(0,0)                                       
*                                                                               
         DC    X'020002FF',AL1(00,00),AL1(02),X'00',AL3(0)                      
         DC    AL1(11,AGYL),AL1(12,CLTL),AL1(18,0)                              
*                                                                               
         DC    X'040004FF',AL1(00,00),AL1(03),X'00',AL3(0)                      
         DC    AL1(01,AGYL),AL1(2,CLTL),AL1(17,0)                               
*                                                                               
         DC    X'0A000AFF',AL1(00,00),AL1(04),X'00',AL3(0)                      
         DC    AL1(06,AGYL),AL1(0,0),AL1(0,0)                                   
*                                                                               
         DC    X'0C000CFF',AL1(00,00),AL1(05),X'00',AL3(0)                      
         DC    AL1(01,AGYL),AL1(6,CLTL),AL1(0,0)                                
*                                                                               
         DC    X'0D010D01',AL1(00,00),AL1(09),X'00',AL3(0)                      
         DC    AL1(02,AGYL),AL1(0,0),AL1(0,0)                                   
*                                                                               
         DC    X'0D070D07',AL1(00,00),AL1(10),X'00',AL3(0)                      
         DC    AL1(02,AGYL),AL1(0,0),AL1(0,0)                                   
*                                                                               
         DC    X'200023FF',AL1(00,00),AL1(06),X'00',AL3(0)                      
         DC    AL1(01,AGYL),AL1(2,CLTL),AL1(0,0)                                
*                                                                               
         DC    X'240032FF',AL1(00,00),AL1(06),X'00',AL3(0)                      
         DC    AL1(01,AGYL),AL1(0,0),AL1(0,0)                                   
*                                                                               
         DC    X'400040FF',AL1(00,00),AL1(07),X'00',AL3(0)                      
         DC    AL1(01,AGYL),AL1(2,CLTL),AL1(16,0)                               
*                                                                               
         DC    X'FFFFFFFF',AL1(00,00),AL1(08),X'20',AL3(0)                      
         DC    AL1(0,0),AL1(0,0),AL1(0,0)                                       
*                                                                               
         DC    X'0000FFFF',AL1(00,00),AL1(11),X'98',AL3(0)                      
         DC    AL1(0,0),AL1(0,0),AL1(0,0)                                       
         EJECT                                                                  
*                                                                               
OLDAGY   EQU   X'10'                                                            
NEWAGY   EQU   X'20'                                                            
*                                                                               
AGYL     EQU   1                                                                
AGYR     EQU   2                                                                
AGYA     EQU   3                                                                
AGYC     EQU   4                                                                
*                                                                               
CLTL     EQU   1                                                                
CLTA     EQU   2                                                                
CLTFILTL DC    X'BCDA'                                                          
CLTFILTA DC    C'PG1'                                                           
*                                                                               
ESTFLT   DC    X'05'               5                                            
         DC    X'06'               6                                            
         DC    X'11'               17                                           
         DC    X'17'               23                                           
         DC    X'18'               24                                           
         DC    X'1A'               26                                           
         DC    X'22'               34                                           
         DC    X'25'               37                                           
         DC    X'5E'               94                                           
         DC    X'66'               102                                          
         DC    X'FFFF'                                                          
*                                                                               
         GETEL R3,27,ELCODE                                                     
         LTORG                                                                  
*                                                                               
COUNT    DS    F                                                                
SVREC    DS    XL20                                                             
*                                                                               
         DS    0F                                                               
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
VPRNTBL  DS    A                                                                
MFULL    DS    F                                                                
HALF     DS    H                                                                
BYTE     DS    CL1                                                              
BYTE2    DS    CL2                                                              
ELCODE   DS    CL1                                                              
NEW      DS    CL20                                                             
WORK     DS    CL64                                                             
WORKX    EQU   *                                                                
         EJECT                                                                  
       ++INCLUDE DDDPRINT                                                       
         EJECT                                                                  
       ++INCLUDE NEGENUNIT                                                      
       ++INCLUDE NEGENPACK                                                      
*                                                                               
RECDEFD  DSECT                                                                  
RECKEYL  DS    XL2                 LOW KEY VALUE                                
RECKEYH  DS    XL2                 HIGH KEY VALUE                               
RECOCVAL DS    0AL2                                                             
RECOCDSP DS    AL1                 OC DISP IN KEY                               
RECOCLEN DS    AL1                 OC LENGTH                                    
RECTYPE  DS    AL1                 RECORD TYPE                                  
RECINDS  DS    X                   INDICATORS                                   
*                                  X'80' = EOT                                  
*                                  X'40' = HEADER REC                           
*                                  X'20' = TRAILER REC                          
*                                  X'10' = DELETION REQ'D                       
*                                  X'08' = PRINT REQ'D                          
*                                  X'04' = ? (ASK BEAKY)                        
*                                  X'01' = WRITE TO ALL FILES                   
RECAROUT DS    AL3                 A(RECORD UPDATE ROUTINE)                     
RECAGDSP DS    AL1                 DISP OF AGENCY IN KEY                        
RECAGTYP DS    AL1                 AGENCY FORMAT IN KEY                         
RECCLDSP DS    AL1                 DISP OF CLIENT IN KEY                        
RECCLTYP DS    AL1                 CLIENT FORMAT IN KEY                         
RECESDSP DS    AL1                 DISP OF ESTIMATE IN KEY                      
         DS    AL1                 SPARE                                        
RECLEN   EQU   *-RECDEFD                                                        
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'140UNLDEXTC  03/19/03'                                      
         END                                                                    
