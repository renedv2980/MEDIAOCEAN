*          DATA SET ACREPXN02A AT LEVEL 054 AS OF 02/16/00                      
*PHASE ACXN02A,+0                                                               
*INCLUDE PRNTBL                                                                 
         TITLE 'ACXN FIX - CHANGE MOA IN 60 ELEMENT'                            
ACXN02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACXN**,R8,R9    BASE REGISTERS 11,9                          
         L     RA,0(R1)                                                         
         USING ACWORKD,RA          RA = A(GLOBAL W/S)                           
         LA    RC,SPACEND                                                       
         USING ACXND,RC            RC = A(SAVE W/S)                             
         L     R8,VBIGPRNT                                                      
         USING BIGPRNTD,R8                                                      
*                                                                               
         CLI   MODE,RUNFRST        RUN FIRST                                    
         BE    RUNF                                                             
         CLI   MODE,PROCTRNS       PROCESS TRANSACTIONS                         
         BE    PTRN                                                             
         CLI   MODE,ACCLAST        ACC LAST                                     
         BE    ACLAST                                                           
         CLI   MODE,LEDGLAST       LEDGER LAST                                  
         BE    LDLAST                                                           
         CLI   MODE,RUNLAST        RUN LAST                                     
         BE    RUNL                                                             
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* RUN FIRST                                                           *         
***********************************************************************         
         SPACE 1                                                                
RUNF     DS    0H                                                               
         USING ACCRECD,RE                                                       
         LA    RE,IO                                                            
         MVC   DISP2,=Y(ACCRFST-ACCKEY)      SET DISP2                          
         DROP  RE                                                               
*                                                                               
         MVC   VTYPES(VTYPLNQ),ADCONS        RELOCATE ADDRESSES                 
         ZAP   PKCNT,=P'0'                                                      
         ZAP   PKACCNT,=P'0'                                                    
         ZAP   PKTOT,=P'0'                                                      
         ZAP   PKACTOT,=P'0'                                                    
         ZAP   PKLDTOT,=P'0'                                                    
*                                                                               
         GOTO1 DATCON,DMCB,(5,0),(0,TODAY)                                      
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* PROCESS TRANSACTION                                                 *         
***********************************************************************         
         SPACE 1                                                                
         USING TRNRECD,R2                                                       
         USING TRNELD,R4                                                        
         USING PLINED,R7                                                        
PTRN     DS    0H                                                               
         L     R4,ADTRANS          R4=A(TRANSACTION ELEMENT)                    
         LR    R2,R4                                                            
         SH    R2,DATADISP         R2=A(TRANSACTION RECORD)                     
         LA    R7,P                                                             
         MVC   P,SPACES                                                         
         MVI   RCSUBPRG,1                                                       
*                                                                               
         CLC   TRNKUNT(2),=C'SJ'                                                
         BE    PTRNX                                                            
*                                                                               
         L     R6,ATRNTAB          TRNS TYPE TABLE                              
PTRN10   CLI   0(R6),EOF           NO ENTRY FOUND                               
         BE    PTRNX                                                            
         CLC   TRNTYPE,0(R6)                                                    
         BE    PTRN20                                                           
         LA    R6,L'TRNTAB(R6)                                                  
         B     PTRN10                                                           
*                                                                               
PTRN20   DS    0H                                                               
         USING TRSELD,R5                                                        
         LR    R5,R4               R5=A(TRANSACTION ELEMENT)                    
         MVI   ELCODE,TRSELQ       X'60' - TRANSACTION STATUS ELEMENT           
         BAS   RE,NEXTEL                                                        
         BNE   PTRN10                                                           
*                                                                               
         CLC   TRSDATE,=X'BD9F'    DEC31/94  OR PRIOR                           
         BH    PTRNX                                                            
*        CLC   TRSDATE,=X'C467'                                                 
*        BH    PTRNX                                                            
*                                                                               
*        USING ACMD,R6                                                          
*        L     R6,AMONACC                                                       
*        CLC   ACMMDTE,=X'9912'    DO WE HAVE THE RIGHT MOA?                    
*        BNE   PTRNX                                                            
         CLC   TRSPMOS,=X'9912'    DO WE HAVE THE RIGHT MOA?                    
         BNE   PTRNX                                                            
*                                                                               
         MVC   MSG,=CL10'TRNS  OLD'                                             
         SR    R3,R3                                                            
         ICM   R3,3,TRNRLEN                                                     
         GOTO1 ADUMP,DMCB,(RC),(R2),(R3)                                        
*                                                                               
         AP    PKTOT,TRNAMNT       ADD UP AMOUNTS FOR RUN TOTAL                 
         AP    PKACTOT,TRNAMNT     ADD UP AMOUNTS FOR ACCOUNT TOTAL             
         AP    PKLDTOT,TRNAMNT                                                  
         AP    PKCNT,=P'1'                                                      
         AP    PKACCNT,=P'1'                                                    
         MVC   PACCT,TRNKULA       MOVE IN ACCOUNT                              
         MVC   PCACCT,TRNKULC      MOVE IN CONTRA ACCOUNT                       
         EDIT  TRNTYPE,PTYPE,ZERO=NOBLANK,FILL=0                                
         EDIT  TRNAMNT,PAMNT,2,MINUS=YES                                        
         MVC   WORK(2),TRSPMOS                                                  
         MVI   WORK+2,X'01'                                                     
         GOTO1 DATCON,DMCB,(1,WORK),(6,PMOA)                                    
         GOTO1 (RF),DMCB,(1,TRNKDATE),(5,PTRNDTE)                               
         GOTO1 (RF),DMCB,(2,TRSDATE),(5,PACTDTE)                                
*                                                                               
         MVC   TRSPMOS,=X'8912'    CHANGE TO DEC/89                             
         MVC   WORK(2),TRSPMOS                                                  
         MVI   WORK+2,X'01'                                                     
         GOTO1 DATCON,DMCB,(1,WORK),(6,PNEWMOA)                                 
*                                                                               
         CLC   TRSUMOS,=X'0000'    DO WE HAVE USED MOA?                         
         BE    PTRN40                                                           
         CLC   TRSUMOS,=X'9912'                                                 
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   TRSUMOS,=X'8912'                                                 
*                                                                               
PTRN40   MVC   MSG,=CL10'TRNS  NEW'                                             
         SR    R3,R3                                                            
         ICM   R3,3,TRNRLEN                                                     
         GOTO1 ADUMP,DMCB,(RC),(R2),(R3)                                        
*                                                                               
         GOTO1 ACREPORT                                                         
         CLI   RCWRITE,C'N'                                                     
         BE    PTRNX                                                            
         MVI   MODE,WRITRANS                                                    
PTRNX    B     EXIT                                                             
         DROP  R2,R4,R5,R7                                                      
         EJECT                                                                  
***********************************************************************         
* ACCOUNT TOTALS                                                      *         
***********************************************************************         
         SPACE 1                                                                
         USING PLINED,R7                                                        
ACLAST   DS    0H                                                               
         LA    R7,P                                                             
*                                                                               
         CP    PKACTOT,=P'0'                                                    
         BE    ACLEXIT                                                          
         CP    PKACCNT,=P'1'                                                    
         BE    ACLEXIT                                                          
*                                                                               
         MVC   PACCT,=CL12'TOTAL ACCT:'                                         
         EDIT  PKACTOT,PAMNT,2,MINUS=YES                                        
         GOTO1 ACREPORT                                                         
         GOTO1 ACREPORT                                                         
ACLEXIT  ZAP   PKACCNT,=P'0'                                                    
         ZAP   PKACTOT,=P'0'                                                    
         B     EXIT                                                             
         DROP  R7                                                               
         EJECT                                                                  
***********************************************************************         
* LEDGER TOTALS                                                      *          
***********************************************************************         
         SPACE 1                                                                
         USING PLINED,R7                                                        
LDLAST   DS    0H                                                               
         LA    R7,P                                                             
*                                                                               
         CP    PKLDTOT,=P'0'                                                    
         BE    EXIT                                                             
*                                                                               
         MVC   PACCT,=CL12'TOTAL LEDG:'                                         
         EDIT  PKLDTOT,PAMNT,2,MINUS=YES                                        
         GOTO1 ACREPORT                                                         
         GOTO1 ACREPORT                                                         
         ZAP   PKLDTOT,=P'0'                                                    
         B     EXIT                                                             
         DROP  R7                                                               
         EJECT                                                                  
***********************************************************************         
* RUN LAST                                                            *         
***********************************************************************         
         SPACE 1                                                                
         USING PLINED,R7                                                        
RUNL     DS    0H                                                               
         LA    R7,P                                                             
*                                                                               
         MVC   PACCT,=CL12'TOTAL RUN :'                                         
         EDIT  PKCNT,PCACCT                                                     
         EDIT  PKTOT,PAMNT,2,MINUS=YES                                          
         GOTO1 ACREPORT                                                         
         B     EXIT                                                             
         DROP  R7                                                               
***********************************************************************         
* GETEL                                                               *         
***********************************************************************         
         SPACE 1                                                                
         GETEL R5,DATADISP,ELCODE                                               
         EJECT                                                                  
***********************************************************************         
* GETEL#2                                                             *         
***********************************************************************         
         SPACE 1                                                                
         GETELN R5,DISP2,ELCODE,2                                               
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* CONSTANTS                                                           *         
***********************************************************************         
         SPACE 1                                                                
ADUMP    DC    A(DUMP)             PRINTABLE ROUTINE                            
         EJECT                                                                  
***********************************************************************         
* RELOCATABLES                                                        *         
***********************************************************************         
         SPACE 1                                                                
ADCONS   DS    0F                                                               
         DC    A(SZTAB)            SZ ACCOUNT TABLE                             
         DC    A(TRNTAB)           SZ TRANSACTION TYPE TABLE                    
*                                                                               
         DC    V(PRNTBL)           PRINT DATA                                   
         EJECT                                                                  
SZTAB    DC    C'YF',CL12'PM',X'00'                                             
         DC    C'YF',CL12'PN',X'00'                                             
         DC    C'YF',CL12'PO',X'00'                                             
         DC    C'YF',CL12'PT',X'00'                                             
         DC    C'YP',CL12'PM',X'00'                                             
         DC    C'YP',CL12'PN',X'00'                                             
         DC    C'YP',CL12'PO',X'00'                                             
         DC    C'YP',CL12'PT',X'00'                                             
         DC    C'YE',CL12'NC',X'00'                                             
         DC    C'YE',CL12'PM',X'00'                                             
         DC    C'YE',CL12'PN',X'00'                                             
         DC    C'YE',CL12'PO',X'00'                                             
         DC    C'YE',CL12'PT',X'00'                                             
         DC    C'YE',CL12'SN',X'00'                                             
         DC    C'YE',CL12'SR',X'00'                                             
         DC    C'YE',CL12'ST',X'00'                                             
         DC    C'WW',CL12'NC',X'00'                                             
         DC    C'WW',CL12'NS',X'00'                                             
         DC    C'WW',CL12'PM',X'00'                                             
         DC    C'WW',CL12'PN',X'00'                                             
         DC    C'WW',CL12'PO',X'00'                                             
         DC    C'WW',CL12'PS',X'00'                                             
         DC    C'WW',CL12'PT',X'00'                                             
         DC    C'WW',CL12'SN',X'00'                                             
         DC    C'WW',CL12'ST',X'00'                                             
         DC    C'YN',CL12'MGAA',X'80'                                           
         DC    C'YN',CL12'MGA8',X'80'                                           
         DC    C'YN',CL12'PM',X'00'                                             
         DC    C'YN',CL12'PN',X'00'                                             
         DC    C'YN',CL12'PO',X'00'                                             
         DC    C'YN',CL12'PT',X'00'                                             
         DC    C'YN',CL12'SN',X'00'                                             
         DC    C'YN',CL12'SR',X'00'                                             
         DC    C'YN',CL12'ST',X'00'                                             
         DC    C'YN',CL12'SX',X'00'                                             
         DC    C'YN',CL12'UA',X'00'                                             
         DC    C'YN',CL12'UN',X'00'                                             
         DC    C'YN',CL12'UG',X'00'                                             
         DC    AL1(EOF)                                                         
*                                                                               
TRNTAB   DS    0XL1                TRANSACTION TYPE TABLE                       
         DC    XL1'2D'             ONLY INCLUDE THESE TRNS 45                   
         DC    AL1(EOF)                                                         
         EJECT                                                                  
***********************************************************************         
* DUMP RECORDS                                                        *         
***********************************************************************         
         SPACE 1                                                                
DUMP     NMOD1 0,**DMP**                                                        
         L     RC,0(R1)                                                         
         CLI   QOPT7,C'Y'                                                       
         BNE   DUMPX                                                            
         LA    R0,L'MSG                                                         
         LA    R2,MSG                                                           
         L     R3,4(R1)                                                         
         L     R4,8(R1)                                                         
*                                                                               
         LA    R5,=C'2D'                                                        
         GOTO1 PRNTBL,DMCB,((R0),(R2)),(R3),C'DUMP',(R4),(R5),         X        
               (C'P',PRINT)                                                     
*                                                                               
DUMPX    XIT1                                                                   
*        MVC   MSG,=CL10'TRNS  REC'                                             
*        GOTO1 ADUMP,DMCB,(RC),(R2),(R6)                                        
*                                                                               
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* WORKING STORAGE                                                     *         
***********************************************************************         
         SPACE 1                                                                
ACXND    DSECT                                                                  
VTYPES   DS    0A                                                               
ASZTAB   DS    A                   SZ ACCOUNT TABLE                             
ATRNTAB  DS    A                   SZ TRANSACTION TYPE TABLE                    
*                                                                               
PRNTBL   DS    V                   PRINT DATA                                   
VTYPLNQ  EQU   *-VTYPES                                                         
*                                                                               
DA       DS    F                   DISK ADDRESS                                 
DISP2    DS    H                   DISPLACEMENT TO ELEMENTS                     
TODAY    DS    CL6                 TODAY'S DATE PACKED                          
*                                                                               
PKTOT    DS    PL16                TOTAL   ACCUMULATOR                          
PKACTOT  DS    PL8                 ACCOUNT ACCUMULATOR                          
PKLDTOT  DS    PL8                 LEDGER ACCUMULATOR                           
PKCNT    DS    PL8                 COUNTER                                      
PKACCNT  DS    PL8                 COUNTER                                      
*                                                                               
MSG      DS    CL10                                                             
DKEY     DS    CL(L'ACCKEY)        DIRECTORY KEY                                
ELCODE   DS    XL1                                                              
*                                                                               
EOF      EQU   X'FF'               END OF FILE MARKER                           
ALL      EQU   X'FF'               EVERYTHING                                   
*                                                                               
IO       DS    0CL2042                                                          
IOKEY    DS    CL42                KEY                                          
IODATA   DS    CL2000              DATA                                         
IOLNQ    EQU   *-IO                LENGTH                                       
         EJECT                                                                  
***********************************************************************         
* SZTAB DSECT                                                         *         
***********************************************************************         
         SPACE 1                                                                
SZTABD   DSECT                                                                  
SZID     DS    CL2                 ALPHAID                                      
SZACCT   DS    CL12                ACCOUNT                                      
SZSTAT   DS    XL1                 STATUS BYTE                                  
SZTRN    EQU   X'80'               INCLUDE ALL TRANSACTIONS                     
SZTABLNQ EQU   *-SZID                                                           
         EJECT                                                                  
***********************************************************************         
* PRINT LINE DSECT                                                    *         
***********************************************************************         
         SPACE 1                                                                
PLINED   DSECT                                                                  
PRTLNE   DS    0C                  PRINT LINE # CLIENT CODES                    
         DS    CL2                                                              
PACCT    DS    CL14                ACCOUNT                                      
         DS    CL1                                                              
PCACCT   DS    CL14                CONTRA-ACCOUNT                               
         DS    CL1                                                              
PTYPE    DS    CL2                 TRANSACTION TYPE                             
         DS    CL3                                                              
PAMNT    DS    CL16                AMOUNT                                       
         DS    CL3                                                              
PACTDTE  DS    CL8                 ACTIVITY DATE                                
         DS    CL3                                                              
PMOA     DS    CL6                 MOA                                          
         DS    CL3                                                              
PNEWMOA  DS    CL6                 NEW MOA                                      
         DS    CL3                                                              
PTRNDTE  DS    CL8                 TRN DATE                                     
PRLNQ    EQU   *-PRTLNE                                                         
         EJECT                                                                  
***********************************************************************         
*              ++INCLUDES                                             *         
***********************************************************************         
         SPACE 1                                                                
* DDCNTRL                                                                       
*                                                                               
       ++INCLUDE DDCNTRL                                                        
*                                                                               
* DMWRKRK                                                                       
*                                                                               
       ++INCLUDE DMWRKRK                                                        
         EJECT                                                                  
* CTGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
* ACBIGPRINTD                                                                   
         PRINT OFF                                                              
       ++INCLUDE ACBIGPRNTD                                                     
       ++INCLUDE ACGENPOST                                                      
       ++INCLUDE ACGENFILE                                                      
       ++INCLUDE ACGENMODES                                                     
       ++INCLUDE ACMASTD                                                        
       ++INCLUDE ACREPWORKD                                                     
       ++INCLUDE DDLOGOD                                                        
       ++INCLUDE DDMASTD                                                        
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE DDBOXEQUS                                                      
       ++INCLUDE DDREPXTRAD                                                     
       ++INCLUDE DDDLCB                                                         
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'054ACREPXN02A02/16/00'                                      
         END                                                                    
