*          DATA SET ACBAT16    AT LEVEL 029 AS OF 08/31/06                      
*PHASE T61B16A                                                                  
         TITLE 'OVERLAY TO HANDLE ORDER RECORDS FOR EXPENSES'                   
T61B16   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 PROGDX-PROGD,**ORDS**,R8,CLEAR=YES                               
         USING TWAD,RA                                                          
         L     R9,4(R1)                                                         
         USING GWS,R9                                                           
         USING PROGD,RC                                                         
         MVI   ERRNUM,X'FF'                                                     
         CLI   MODE,0                                                           
         BE    DORDR                                                            
         CLI   MODE,1                                                           
         BE    ORD100                                                           
         B     EXIT                                                             
         EJECT                                                                  
*---------------------------------------------------------                      
*        READ AND DISPLAY ORDER RECORD                                          
*---------------------------------------------------------                      
*                                                                               
DORDR    DS    0H                                                               
         LA    RF,COMPEL                                                        
         USING ACCOMPD,RF                                                       
         LA    R2,PICORDH                                                       
         MVI   ERRNUM,2                                                         
         CLI   5(R2),0                                                          
         BE    DORD02                                                           
*                                                                               
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   8(0,R2),=6C'0'      CONTROL RECORD IS BANNED                     
         BE    ERROR                                                            
*                                                                               
DORD02   MVI   ERRNUM,X'FF'                                                     
         CLC   PICORD(5),=C'DUMMY' DUMMY MEANS NO ORDER INPUT                   
         BE    DORD04                                                           
         CLI   5(R2),0                                                          
         BNE   DORD08                                                           
         B     DORD06                                                           
*                                                                               
DORD04   MVC   ORDNO(5),=C'DUMMY'                                               
         MVI   ORDNO+5,C' '                                                     
         ZAP   ORDAMT,=P'0'                                                     
*                                                                               
DORD06   MVI   MODE,0              NO ORDER NO - EXIT                           
         B     EXIT                                                             
*                                                                               
DORD08   MVI   ERRNUM,2                                                         
         XC    WORK,WORK                                                        
         GOTO1 SCANNER,DMCB,(R2),(2,WORK)                                       
         CLI   4(R1),0                                                          
         BE    ERROR               INVALID DATA                                 
*                                                                               
         USING ORDRECD,R6                                                       
         LA    R6,IOKEY                                                         
         LA    RF,WORK                                                          
         XC    ORDKEY,ORDKEY                                                    
         MVI   ORDKTYP,ORDKTYPQ                                                 
         MVC   ORDKCPY,COMPANY                                                  
         MVC   ORDKORD,=6C'0'     ZERO FILL ORDER NUMBER IN KEY                 
         LA    RE,ORDKORD+6                                                     
         ZIC   R1,0(RF)                                                         
         SR    RE,R1                                                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),12(RF)      AND RIGHT ALIGN IT                           
         LA    RF,32(RF)                                                        
         CLI   0(RF),0                                                          
         BE    DORD10                                                           
         MVI   ERRNUM,2                                                         
         IC    R1,0(RF)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   12(0,RF),=C'PART'                                                
         BNE   ERROR                                                            
*                                                                               
DORD10   MVC   KEYSAVE(40),SPACES  BUILD BLOCK FOR UNSCAN                       
         MVC   KEYSAVE(6),IOKEY+4                                               
         MVC   KEYSAVE+20(4),WORK+32+12                                         
         LA    R3,1                                                             
         OC    KEYSAVE+20(4),KEYSAVE+20                                         
         BZ    *+8                                                              
         LA    R3,2                                                             
         GOTO1 UNSCAN,DMCB,((R3),KEYSAVE),(R2)                                  
         OI    6(R2),X'80'                                                      
*                                                                               
         GOTO1 AIO,IOHIGH+IOACCMST+IO1                                          
         CLC   IOKEY(L'ORDKEY),IOKEYSAV                                         
         MVI   ERRNUM,ORDMISS                                                   
         CLC   IOKEY(15),KEYSAVE                                                
         BNE   ERROR                                                            
*                                                                               
         MVC   FVMSGNO,=AL2(AE$OFMCH)                                           
         L     R6,AIO1             TEST ORDER FULLY MATCHED                     
         TM    ORDRSTAT,ORDSFMCH                                                
         BNZ   ERROR                                                            
         MVC   FVMSGNO,=AL2(AE$ORDEL) OR DELETED                                
         TM    ORDRSTAT,ORDSDEL+ORDSLDEL                                        
         BNZ   ERROR                                                            
*                                                                               
         MVC   FVMSGNO,=AL2(AE$ORDNF)                                           
         CLI   ORDKSEQ,ORDKEXTN    IS THIS AN EXTENSION?                        
         BE    ERROR               YES, ERROR                                   
         TM    ORDRSTA2,ORDSEXEX   NO, DOES AN EXTENSION EXIST?                 
         BZ    DORD12              NO                                           
         TM    ORDRSTA2,ORDSAPPR   YES, IS IT APPROVED?                         
         BNZ   DORD12              YES                                          
         TM    ORDRSTAT,ORDGDRCV   NO, IS IT GOODS RECEIVED?                    
         BNZ   DORD12              YES                                          
         MVC   FVMSGNO,=AL2(AE$ORTNF)                                           
         B     ERROR               ERROR 'TRANSACTION NOT ON FILE'              
                                                                                
DORD12   MVC   ORDNO,ORDKORD       ORDER NUMBER TO TWA                          
         MVI   ERRNUM,X'FF'                                                     
*                                                                               
         ZAP   INVS,=P'0'                                                       
         ZAP   ORDSOFA,=P'0'                                                    
         XC    PICAMT,PICAMT       CLEAR AMOUNT FIELD                           
*                                                                               
         LA    RF,BOELEM                                                        
         ST    RF,BOADDR2                                                       
         LA    R3,ORDRFST                                                       
*                                                                               
DORD18   CLI   0(R3),0             PICK OUT ORDER DATA FOR DISPLAY              
         BE    DORD44                                                           
         CLI   0(R3),ORDELQ        ORDER ELEMENT = X'67'                        
         BE    DORD22                                                           
         CLI   0(R3),OAMELQ        ORDER AMOUNT ELEMENT = X'68'                 
         BE    DORD38                                                           
         CLI   0(R3),SCMELQ        COMMENT ELEMENT = X'3E'                      
         BE    DORD42                                                           
*                                                                               
DORD20   ZIC   RF,1(R3)                                                         
         AR    R3,RF                                                            
         B     DORD18                                                           
*                                                                               
         USING ACORDRD,R3                                                       
DORD22   DS    0H                  ORDER ELEMENT                                
         LA    R2,PICORDH                                                       
         MVI   ERRNUM,2                                                         
         CLC   ACOREXP+1(2),=C'SJ'                                              
         BE    ERROR               DONT WANT PRODN ORDERS                       
         TM    ACORSTAT,X'10'      TYPE 10 ONLY?                                
         BZ    DORD24              NO                                           
         MVC   FVMSGNO,=Y(AE$TY10O)                                             
         B     ERROR                                                            
*                                                                               
DORD24   OC    PICDBT,SPACES                                                    
         CLC   PICDBT,ACOREXP+3                                                 
         BE    DORD28                                                           
         MVC   PICDBT,SPACES                                                    
         MVC   PICDBT(12),ACOREXP+3     PUT EXP ACCT ON SCREEN                  
         CLC   ACOREXP+1(2),=C'SE' PRECEED BY *UL IF NOT SE                     
         BE    DORD26                                                           
         MVC   PICDBT+1(13),ACOREXP+1                                           
         MVI   PICDBT,C'*'                                                      
*                                                                               
DORD26   XC    PICDBTN,PICDBTN     CLEAR NAME IF DIFFERENT EXP ACCT             
         OI    PICDBTNH+6,X'80'                                                 
         NI    PICDBTH+4,X'DF'                                                  
*                                                                               
DORD28   MVC   WORK(12),PICCRT                                                  
         OC    WORK(12),SPACES                                                  
         CLC   WORK(12),ACORSUPP+3                                              
         BE    DORD20                                                           
         XC    PICCRTN,PICCRTN                                                  
         OI    PICCRTNH+6,X'80'                                                 
         NI    PICCRTH+4,X'DF'                                                  
         MVC   PICCRT(12),ACORSUPP+3                                            
         LA    RF,COMPEL                                                        
         USING ACCOMPD,RF                                                       
         CLI   INPUT,21            SHOW *UL IF NECESSARY                        
         BE    DORD30              BUT FIRST ESTABLISH IF UL IS THE             
         CLI   INPUT,22            DEFAULT FOR THIS INPUT TYPE                  
         BE    DORD32                                                           
         CLC   ACORSUPP+1(2),=C'SA'    SA IS ASSUMED FOR 23                     
         BE    DORD36                                                           
         B     DORD34                                                           
DORD30   MVC   WORK(1),ACMPSUPP        SV OR SX FOR 21                          
         MVC   WORK+1(1),ACMPSUPX                                               
         CLC   ACORSUPP+1(2),WORK                                               
         BE    DORD36                                                           
         B     DORD34                                                           
*                                                                               
DORD32   CLC   ACORSUPP+1(2),ACMPBANK  AND SC FOR 22                            
         BE    DORD36                                                           
*                                                                               
DORD34   MVC   PICCRT+1(13),ACORSUPP+1                                          
         MVI   PICCRT,C'*'                                                      
DORD36   DS    0H                                                               
         B     DORD20                                                           
*                                                                               
         USING ACOAMTD,R3                                                       
DORD38   DS    0H                  ORDER AMOUNT ELEMENT                         
         ZAP   ORDAMT,ACOAMT                                                    
         ZAP   INVS,ACOAINUM                                                    
         AP    ORDSOFA,ACOAMT                                                   
         LA    RE,PICAMT                                                        
         ZAP   LEFT,ACOAMT                                                      
         SP    LEFT,ACOAIVAL       GIVES WHAT IS LEFT OF ORIG. ORDER            
         CLC   TODAYP,ACOPDATE                                                  
         BNE   DORD40                                                           
         SP    LEFT,ACOPAMT                                                     
*                                                                               
DORD40   EDIT  LEFT,(10,0(RE)),2,MINUS=YES,FLOAT=*,ALIGN=LEFT                   
         B     DORD20                                                           
         DROP  R3                                                               
*                                                                               
         USING SCMELD,R3                                                        
DORD42   TM    ORDRSTA2,ORDSEXEX   ONLY EBUYER GETS NARRATIVE                   
         BZ    DORD20                                                           
         CLI   SCMTYPE,SCMTOMOC    IS THIS ORDER MATCHING?                      
         BNE   DORD20              NO, SKIP IT                                  
*                                                                               
         L     RE,BOADDR2                                                       
*                                                                               
         SR    R1,R1                                                            
         IC    R1,SCMLN                                                         
         SHI   R1,SCMLN1Q+1        GET LENGTH OF NARRATIVE                      
*                                                                               
         CHI   R1,98               MAX LENGTH IS 98                             
         BNH   *+8                                                              
         LA    R1,98                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),SCMNARR     MOVE AS MUCH TEXT AS POSSIBLE                
         B     DORD20                                                           
*                                                                               
DORD44   OC    BOELEM,BOELEM                                                    
         BZ    DORD46                                                           
*                                                                               
         LA    RE,PICNARH                                                       
         LA    RE,8(RE)                                                         
         LA    RF,BOELEM                                                        
         LA    R1,49               MAX CHARACTERS PER LINE                      
         SHI   R1,1                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),0(RF)                                                    
*                                                                               
         LA    RF,1(R1,RF)                                                      
         LA    RE,PICNARH                                                       
         SR    R2,R2                                                            
         IC    R2,0(RE)                                                         
         AR    RE,R2                                                            
         LA    RE,8(RE)                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),0(RF)                                                    
*                                                                               
DORD46   DS    0H                                                               
         OI    PICDBTH+6,X'80'     OUTPUT THE FIELDS WE HAVE BUILT              
         OI    PICCRTH+6,X'80'                                                  
         OI    PICOHSTH+6,X'80'                                                 
         OI    PICAMTH+6,X'80'                                                  
         LA    R2,PICNARH                                                       
         OI    6(R2),X'80'                                                      
         SR    RE,RE                                                            
         IC    RE,0(R2)            TRANSMIT 2ND LINE ALSO                       
         AR    R2,RE                                                            
         OI    6(R2),X'80'                                                      
*                                                                               
         LA    RF,PICOHST+10       DISPLAY TOTALS                               
         EDIT  ORDSOFA,(10,0(RF)),2,MINUS=YES,ALIGN=LEFT                        
         LA    RF,PICOHST+38                                                    
         EDIT  INVS,(2,0(RF)),ZERO=NOBLANK                                      
*                                                                               
         MVC   MSG(45),=C'ORDER DETAILS DISPLAYED.ENTER INVOICE DETAILSX        
               '                                                                
         MVI   ERRNUM,X'FE'                                                     
         MVI   MODE,1                                                           
         B     EXIT                                                             
         EJECT                                                                  
*---------------------------------------------------------                      
*        DELETE OR AMEND ORDER RECORDS                                          
*---------------------------------------------------------                      
*                                                                               
ORD100   DS    0H                                                               
         XC    KEY,KEY             BUILD KEY TO READ ORDER RECORD               
         MVC   KEY+1(1),COMPANY                                                 
         MVI   KEY,X'1A'                                                        
         MVC   KEY+4(6),ORDNO                                                   
         BAS   RE,READL                                                         
*                                                                               
ORD110   DS    0H                                                               
         LA    R3,IOAREA                                                        
         LA    R2,PICORDH                                                       
         BAS   RE,UPDT68                                                        
         BAS   RE,WRITE                                                         
*                                                                               
ORD200   LA    R2,PICORDH                                                       
         MVI   MODE,0                                                           
         MVI   ERRNUM,X'FF'                                                     
         B     EXIT                                                             
         EJECT                                                                  
*---------------------------------------------------------                      
*        UPDATE ORDER AMOUNT ELEMENT                                            
*---------------------------------------------------------                      
*                                                                               
UPDT68   NTR1                                                                   
UPD2     CLI   0(R3),0                                                          
         BE    XIT                                                              
         CLI   0(R3),X'68'                                                      
         BE    UPD6                                                             
UPD4     ZIC   RE,1(R3)                                                         
         AR    R3,RE                                                            
         B     UPD2                                                             
*                                                                               
         USING ACOAMTD,R3                                                       
UPD6     DS    0H                                                               
         AP    ACOPAMT,TOTCASH                                                  
         MVC   ACOPDATE,TODAYP                                                  
         B     XIT                                                              
XIT      XIT                                                                    
         EJECT                                                                  
*---------------------------------------------------------                      
*        ACBATCODE                                                              
*---------------------------------------------------------                      
*                                                                               
       ++INCLUDE ACBATCODE                                                      
*---------------------------------------------------------                      
*        LITERAL DECLARATIONS                                                   
*---------------------------------------------------------                      
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*---------------------------------------------------------                      
*        ACBATDSECT                                                             
*---------------------------------------------------------                      
*                                                                               
       ++INCLUDE ACBATDSECT                                                     
         ORG   TWAHOLE                                                          
ORDAMT   DS    PL6                                                              
         ORG   CONTABH                                                          
         EJECT                                                                  
*---------------------------------------------------------                      
*        ACBATE9D                                                               
*---------------------------------------------------------                      
*                                                                               
       ++INCLUDE ACBATE9D                                                       
*        EJECT                                                                  
*---------------------------------------------------------                      
*        PROGD DSECT                                                            
*---------------------------------------------------------                      
*                                                                               
PROGD    DSECT                                                                  
LEFT     DS    PL6                                                              
PARTSW   DS    CL1                                                              
INVS     DS    PL3                                                              
ORDSOFA  DS    PL6                                                              
TRANSKEY DS    CL49                                                             
KEY      DS    CL49                                                             
IOAREA   DS    2000C                                                            
PROGDX   DS    0C                                                               
         EJECT                                                                  
*---------------------------------------------------------                      
*        ++INCLUDES                                                             
*---------------------------------------------------------                      
*                                                                               
* ACGENBOTH                                                                     
* ACGENDAY                                                                      
* DDFLDIND                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
       ++INCLUDE ACGENDAY                                                       
       ++INCLUDE DDFLDIND                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'029ACBAT16   08/31/06'                                      
         END                                                                    
