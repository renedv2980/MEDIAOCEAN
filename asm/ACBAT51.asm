*          DATA SET ACBAT51    AT LEVEL 002 AS OF 03/13/12                      
*PHASE T61B51A                                                                  
         TITLE 'RETAINED EARNINGS'                                              
T61B51   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 PROGDX-PROGD,**BAT51**,RR=R5,CLEAR=YES                           
         USING TWAD,RA                                                          
         L     R9,4(R1)                                                         
         USING GWS,R9                                                           
         USING PROGD,RC                                                         
         ST    R5,PRELOC                                                        
*                                                                               
*-------------------------------------------------------------                  
*        RETAINED EARNINGS ENTRY                                                
*-------------------------------------------------------------                  
*                                                                               
         LA    R2,RETDOCH          SAVE DOCUMENT NUMBER                         
         BAS   RE,ANY                                                           
         MVC   SAVEDOC,RETDOC                                                   
         OC    SAVEDOC,SPACES                                                   
*                                                                               
         CLI   PFKEY,PFK11         TEST PF11=ERASE SCREEN                       
         BNE   *+8                                                              
         BAS   RE,CLEARIT                                                       
*                                                                               
         LA    R2,RETDATH          IF NO DATE, USE TODAY                        
         MVC   FVMSGNO,=A(AE$INDAT)                                             
         CLI   5(R2),0                                                          
         BE    RET02                                                            
         GOTO1 DATVAL,DMCB,(0,8(R2)),WORK                                       
         OC    DMCB(4),DMCB                                                     
         BZ    ERROR                                                            
         B     *+8                                                              
*                                                                               
RET02    BAS   RE,GETODAY                                                       
         GOTO1 DATCON,DMCB,(0,WORK),(1,DATE3)                                   
         GOTO1 DATECHK,DMCB,DATE3                                               
         CLI   DMCB,X'FF'                                                       
         BE    ERROR                                                            
*                                                                               
         XC    SAVECAC,SAVECAC     CLEAR SAVE AREAS                             
         XC    SAVECACN,SAVECACN                                                
*                                                                               
         EJECT                                                                  
*-------------------------------------------------------------                  
*        VALIDATE FIELDS, SAVE ACCOUNTS AND AMOUNTS                             
*-------------------------------------------------------------                  
*                                                                               
         LA    R2,RETFRSTH                                                      
         BAS   RE,ANY                                                           
*                                                                               
         USING SCREEND,R5                                                       
         L     R4,=A(TABLE)                                                     
         A     R4,PRELOC                                                        
         USING POSTD,R4                                                         
         ST    R4,ADDTAB                                                        
*                                                                               
RET04    LR    R5,R2                                                            
         LA    R2,SCRDCH                                                        
         BAS   RE,ANY                                                           
         MVI   POSTDC,C'D'                                                      
         CLI   8(R2),C'D'                                                       
         BE    RET06               MUST BE DR OR CR                             
         MVI   POSTDC,C'C'                                                      
         MVC   FVMSGNO,=Y(AE$INVIF)                                             
         CLI   8(R2),C'C'                                                       
         BNE   ERROR                                                            
*                                                                               
RET06    MVC   KEY,SPACES          READ ACCOUNT RECORD                          
         LA    R2,SCRACCH                                                       
         CLI   5(R2),0                                                          
         BNE   RET07                                                            
         MVC   SCRANM,SPACES                                                    
         MVI   SCRANMH+6,X'80'                                                  
         MVC   FVMSGNO,=Y(AE$MISIF)                                             
         B     ERROR                                                            
*                                                                               
RET07    LLC   RF,5(R2)                                                         
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   KEY+1(0),SCRACC                                                  
         MVC   KEY(1),COMPANY                                                   
         SR    R6,R6               NO PROFILES NEEDED                           
         BAS   RE,GETACC                                                        
         BAS   RE,CHECKACC                                                      
         ST    R2,SAVER2                                                        
*                                                                               
         MVC   FVMSGNO,=Y(AE$INACP)                                             
         CLC   KEY+1(2),=C'SB'                                                  
         BE    RET16                                                            
*                                                                               
         CLC   KEY+1(2),=C'SE'                                                  
         BE    RET08                                                            
         CLC   KEY+1(2),=C'SI'                                                  
         BNE   ERROR                                                            
         CLI   POSTDC,C'D'         NO SI DEBITS ALLOWED                         
         BE    ERROR                                                            
*                                                                               
RET08    TM    ACSTAT1,RSTSEADD    SE AND SI CAN'T BE ANALYZED                  
         BO    ERROR                                                            
         TM    ACSTAT1,RSTSGPEI                                                 
         BO    ERROR                                                            
         CLC   KEY+1(2),=C'SE'                                                  
         BNE   RET10                                                            
*                                                                               
         LA    R1,CATBLK                                                        
         USING CATD,R1                                                          
         XC    CATD(CATLNQ),CATD                                                
         MVC   CATDMGR,DATAMGR                                                  
         MVC   CATSEAC,ACCTNUM                                                  
         GOTO1 VCATCALL                                                         
         CLI   CATPST,C'Y'                                                      
         BE    ERROR                                                            
         B     RET16                                                            
*                                                                               
RET10    OC    ACASPA,ACASPA       ANY 2C ELEMENT?                              
         BZ    RET16               NO, NOT ANALYZED                             
                                                                                
         USING SPAELD,R6                                                        
         ICM   R6,15,ACASPA                                                     
*                                                                               
RET12    CLI   SPAEL,0             END OF RECORD?                               
         BE    RET16               YES                                          
         CLI   SPAEL,SPAELQ                                                     
         BE    *+14                                                             
*                                                                               
RET14    IC    R1,SPALN                                                         
         AR    R6,R1                                                            
         B     RET12                                                            
         CLI   SPATYPE,SPATANAL                                                 
         BNE   RET14                                                            
         B     ERROR                                                            
*                                                                               
RET16    MVC   POSTACC,ACCTNUM     SAVE IN POSTING TABLE                        
         MVC   POSTACCN,ACCTNAME                                                
         MVC   FVMSGNO,=A(AE$INACP)                                             
         TM    ACBSTAT,ACBSABAL                                                 
         BZ    ERROR               NO BALANCE ELEMENT                           
         TM    ACBSTAT,ACBSCLSE+ACBSLOCK                                        
         BNZ   ERROR               CLOSED AND/OR LOCKED                         
         TM    ACBSTAT,ACBSPERS+ACBSDEPT                                        
         BNZ   ERROR               DEPT AND/OR STAFF FLAGS SET                  
*                                                                               
         LA    R2,SCRCACH                                                       
         CLI   5(R2),0             IS THERE A CONTRA ACCOUNT?                   
         BNE   RET18               YES                                          
*                                                                               
         OC    SAVECAC,SAVECAC     NO, IS THERE A SAVED ONE?                    
         BNZ   *+8                 YES                                          
         BAS   RE,ANY              NO, GENERATE AN ERROR                        
*                                                                               
         MVC   POSTCAC,SAVECAC     USE IT THEN                                  
*                                                                               
         MVC   FVMSGNO,=Y(AE$MISIF)                                             
         CLC   POSTCAC(3),SPACES                                                
         BE    ERROR                                                            
         MVC   WORK(14),SAVECAC                                                 
         B     RET20                                                            
*                                                                               
RET18    MVC   WORK(14),SCRCAC                                                  
         MVC   POSTCAC,SPACES                                                   
         MVC   POSTCAC+3(11),WORK+3                                             
         OC    POSTCAC+3(11),SPACES                                             
*                                                                               
RET20    MVC   KEY+1(41),SPACES                                                 
         MVC   KEY(1),COMPANY                                                   
         LLC   RF,5(R2)                                                         
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   KEY+1(0),WORK       ASSUME NO COMPANY INPUT                      
*        BAS   RE,HIGH                                                          
*        MVC   POSTCAC,KEYSAVE                                                  
*        CLC   KEY(15),KEYSAVE                                                  
*        BE    RET22                                                            
*                                                                               
*        MVC   POSTCAC,SPACES                                                   
*        MVC   POSTCAC(14),KEYSAVE+1                                            
*        B     RET24                                                            
*                                                                               
RET22    BAS   RE,GETACC                                                        
         BAS   RE,CHECKACC                                                      
         MVC   POSTCAC,ACCTNUM                                                  
         MVC   POSTCACN,ACCTNAME                                                
*                                                                               
RET24    MVC   SAVECAC,POSTCAC                                                  
         MVC   SAVECACN,POSTCACN                                                
*                                                                               
         LA    R2,SCROFFH                                                       
         MVC   POSTOFF,SPACES                                                   
         TM    COMPSTAT,X'20'      OFFICE AGENCY NEEDS ANAL/OFFC INPT           
         BZ    RET28                                                            
         BAS   RE,ANY                                                           
         OC    8(2,R2),SPACES                                                   
         GOTO1 AVALOFFC,DMCB,(X'80',8(R2))                                      
         BNE   ERROR                                                            
*                                                                               
         CLI   5(R2),0                                                          
         BE    RET28                                                            
         CLC   =C'**',8(R2)                                                     
         BE    ERROR                                                            
         IC    RF,5(R2)                                                         
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   POSTOFF(0),8(R2)                                                 
*                                                                               
RET28    LA    R2,SCRAMTH          CASH FIELD                                   
         LLC   R3,5(R2)                                                         
         MVC   FVMSGNO,=Y(AE$INAMT)                                             
         GOTO1 AMTVAL,DMCB,8(R2),(R3)                                           
         CLI   DMCB,X'FF'                                                       
         BE    ERROR                                                            
         L     RF,DMCB+4                                                        
         LA    RF,0(RF)                                                         
         ZAP   POSTAMT,0(8,RF)                                                  
*                                                                               
         LA    R2,SCRANMH          ACCOUNT NAME                                 
         L     R3,SAVER2                                                        
         TM    4(R3),X'20'                                                      
         BO    RET30                                                            
         GOTO1 CHOPPER,DMCB,(36,POSTACCN),(20,8(R2)),1                          
         FOUT  (R2)                                                             
*                                                                               
RET30    AHI   R4,POSTLNQ                                                       
         MVI   0(R4),X'FF'                                                      
         LLC   RF,0(R2)                                                         
         AR    R2,RF                                                            
         CLI   0(R2),10            TAB FIELD                                    
         BL    RET100                                                           
         CLI   5(R2),0             ANY MORE                                     
         BNE   RET04                                                            
         EJECT                                                                  
*-------------------------------------------------------------                  
*        NOW SEE IF DEBITS EQUAL CREDITS                                        
*-------------------------------------------------------------                  
*                                                                               
RET100   L     R4,ADDTAB                                                        
         ZAP   DEBITS,=P'0'                                                     
         ZAP   CREDITS,=P'0'                                                    
*                                                                               
RET102   LA    RF,DEBITS                                                        
         CLI   POSTDC,C'D'                                                      
         BE    *+8                                                              
         LA    RF,CREDITS                                                       
         AP    0(6,RF),POSTAMT                                                  
         AHI   R4,POSTLNQ                                                       
         CLI   0(R4),X'FF'                                                      
         BNE   RET102                                                           
*                                                                               
         CP    CREDITS,DEBITS                                                   
         BE    RET200                                                           
         MVI   ERRNUM,X'FE'                                                     
         LA    R2,RETFCASH                                                      
         MVC   MSG,SPACES                                                       
         MVC   MSG(35),=C'ERROR - DEBITS DO NOT EQUAL CREDITS'                  
         B     EXIT                                                             
         EJECT                                                                  
*-------------------------------------------------------------                  
*              NOW BUILD POSTING RECORD                                         
*-------------------------------------------------------------                  
*                                                                               
RET200   LA    R7,IOAREA+2                                                      
         USING DLDESCD,R7                                                       
         MVI   DLDSEL,X'64'        DESCRIPTION ELEMENT                          
         MVC   DLDSREF,SAVEDOC                                                  
         MVC   DLDSDATE,DATE3                                                   
         MVI   DLDSSBRF,0                                                       
         MVI   DLDSSTAT,0                                                       
         XC    DLDSNARR,DLDSNARR                                                
         MVI   DLDSNARR,C' '                                                    
         LA    RF,1                                                             
         CLI   RETNARH+5,0                                                      
         BE    RET210                                                           
         IC    RF,RETNARH+5                                                     
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   DLDSNARR(0),RETNAR                                               
         LA    RF,1(RF)                                                         
*                                                                               
RET210   LA    R3,DLDSNARR                                                      
         SR    R3,R7                                                            
         AR    R3,RF                                                            
         STH   R3,HALF                                                          
         MVC   DLDSLEN,HALF+1                                                   
         AR    R7,R3               POSTING ELEMENTS                             
         USING DLPOSTD,R7                                                       
         L     R4,ADDTAB                                                        
*                                                                               
         USING POSTD,R4                                                         
RET220   MVI   DLPSLEN,X'71'                                                    
         MVI   DLPSTYPE,0                                                       
         ZAP   DLPSAMNT,POSTAMT                                                 
         MVC   DLPSANAL,POSTOFF                                                 
*                                                                               
         CLI   POSTDC,C'C'                                                      
         BE    RET230                                                           
         MVC   DLPSDBAC(51),POSTACC                                             
         MVC   DLPSCRAC(51),POSTCAC                                             
         MVI   DLPSEL,X'69'        DEBIT                                        
         B     RET240                                                           
*                                                                               
RET230   MVC   DLPSCRAC(51),POSTACC                                             
         MVC   DLPSDBAC(51),POSTCAC                                             
         MVI   DLPSEL,X'6A'        CREDIT                                       
*                                                                               
RET240   LLC   RF,DLPSLEN                                                       
         AR    R7,RF                                                            
         AHI   R4,POSTLNQ                                                       
         CLI   0(R4),X'FF'                                                      
         BNE   RET220              DO ANOTHER                                   
*                                                                               
         MVI   0(R7),0             END OF RECORD                                
         LA    R3,IOAREA-1                                                      
         SR    R7,R3                                                            
         STH   R7,HALF                                                          
         MVC   IOAREA(2),HALF      FINAL LENGTH                                 
         BAS   RE,PUTDAY                                                        
*                                                                               
         XC    WORK(20),WORK                                                    
         MVC   WORK(6),SAVEDOC     REFERENCE                                    
         L     R3,DMCB+8                                                        
         MVC   WORK+10(4),0(R3)    DISK ADDRESS                                 
         ZAP   TRANSAMT,DEBITS                                                  
         BAS   RE,ADTWA1                                                        
         LA    R2,RETDOCH                                                       
         MVI   ERRNUM,X'FF'                                                     
         B     EXIT                                                             
         EJECT                                                                  
CLEARIT  NTR1                                                                   
         MVC   FLD,SPACES                                                       
         CLI   CSACT,ACTCHA        ENTERING FROM ITEM/CHANGE?                   
         BE    CLEARITX                                                         
         LA    R2,RETFRSTH                                                      
         USING SCREEND,R5                                                       
*                                                                               
CLEAR10  LR    R5,R2                                                            
         LA    R2,SCRDCH           START AT DEBIT/CREDIT FIELD                  
         TM    4(R2),X'80'         USE FIELD IF ENTERED                         
         BO    *+12                                                             
         BAS   RE,MOVEFLD          ELSE, MOVE IN SPACES                         
         STC   RF,5(R2)                                                         
*                                                                               
         LA    R2,SCRACCH          ACCOUNT FIELD                                
         TM    4(R2),X'80'                                                      
         BO    *+12                                                             
         BAS   RE,MOVEFLD                                                       
         STC   RF,5(R2)                                                         
*                                                                               
         LA    R2,SCRCACH          CONTRA ACCOUNT FIELD                         
         TM    4(R2),X'80'                                                      
         BO    *+12                                                             
         BAS   RE,MOVEFLD                                                       
         STC   RF,5(R2)                                                         
*                                                                               
         LA    R2,SCROFFH          OFFICE FIELD                                 
         TM    4(R2),X'80'                                                      
         BO    *+12                                                             
         BAS   RE,MOVEFLD                                                       
         STC   RF,5(R2)                                                         
*                                                                               
         LA    R2,SCRAMTH          AMOUNT FIELD                                 
         TM    4(R2),X'80'                                                      
         BO    *+12                                                             
         BAS   RE,MOVEFLD                                                       
         STC   RF,5(R2)                                                         
*                                                                               
         LA    R2,SCRANMH          ACCOUNT NAME FIELD                           
         BAS   RE,MOVEFLD                                                       
         STC   RF,5(R2)                                                         
*                                                                               
         LLC   RF,0(R2)                                                         
         AR    R2,RF                                                            
         CLI   0(R2),10            TAB FIELD                                    
         BL    CLEARITX                                                         
         CLI   5(R2),0             ANY MORE                                     
         BNE   CLEAR10                                                          
                                                                                
CLEARITX B     EXIT                                                             
         EJECT                                                                  
MOVEFLD  NTR1                                                                   
         ZIC   R1,0(R2)                                                         
         LA    RE,8+1                                                           
         TM    1(R2),X'02'         TEST FOR EXTENDED FIELD                      
         BZ    *+8                                                              
         LA    RE,8+8+1                                                         
         SR    R1,RE                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),FLD                                                      
*                                                                               
         LA    R3,8(R2,R1)         GET LAST BYTE OF FIELD                       
         LA    RF,1(R1)            GET LENGTH OF FIELD                          
         CLI   0(R3),C' '          LOOK FOR SIGNIFICANT DATA                    
         BH    *+10                                                             
         BCTR  R3,0                                                             
         BCT   RF,*-10                                                          
         OI    6(R2),X'80'                                                      
*                                                                               
MOVEFLDX B     EXIT                                                             
         EJECT                                                                  
* ACBATCODE                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACBATCODE                                                      
         PRINT ON                                                               
         LTORG                                                                  
* ACBATDSECT                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACBATDSECT                                                     
         PRINT ON                                                               
* ACBATB5D                                                                      
         PRINT OFF                                                              
         ORG   CONTABH                                                          
       ++INCLUDE ACBATB5D                                                       
         PRINT ON                                                               
         EJECT                                                                  
*-------------------------------------------------------------                  
*        LOCAL WORKING STORAGE                                                  
*-------------------------------------------------------------                  
*                                                                               
PROGD    DSECT                                                                  
SAVER2   DS    F                                                                
ADDTAB   DS    F                                                                
PRELOC   DS    F                                                                
DEBITS   DS    PL6                                                              
CREDITS  DS    PL6                                                              
SAVEDOC  DS    CL6                                                              
DATE3    DS    CL3                                                              
SAVECAC  DS    CL15                                                             
SAVECACN DS    CL36                                                             
KEY      DS    CL49                                                             
CATBLK   DS    XL(CATLNQ)                                                       
IOAREA   DS    2000C                                                            
PROGDX   DS    0C                                                               
         EJECT                                                                  
*-------------------------------------------------------------                  
*        DSECT FOR LINE IN POSTING TABLE                                        
*-------------------------------------------------------------                  
*                                                                               
POSTD    DSECT                                                                  
POSTDC   DS    CL1                                                              
POSTACC  DS    CL15                                                             
POSTACCN DS    CL36                                                             
POSTCAC  DS    CL15                                                             
POSTCACN DS    CL36                                                             
POSTOFF  DS    CL2                                                              
POSTAMT  DS    PL8                                                              
POSTLNQ  EQU   *-POSTD                                                          
         EJECT                                                                  
*-------------------------------------------------------------                  
*        DSECT FOR LINE ON SCREEN                                               
*-------------------------------------------------------------                  
*                                                                               
SCREEND  DSECT                                                                  
SCRDCH   DS    CL8                                                              
SCRDC    DS    CL1                                                              
         DS    CL8                                                              
SCRACCH  DS    CL8                                                              
SCRACC   DS    CL14                                                             
         DS    CL8                                                              
SCRCACH  DS    CL8                                                              
SCRCAC   DS    CL14                                                             
         DS    CL8                                                              
SCROFFH  DS    CL8                                                              
SCROFF   DS    CL2                                                              
         DS    CL8                                                              
SCRAMTH  DS    CL8                                                              
SCRAMT   DS    CL15                                                             
         DS    CL8                                                              
SCRANMH  DS    CL8                                                              
SCRANM   DS    CL20                                                             
         DS    CL8                                                              
         EJECT                                                                  
*-------------------------------------------------------------                  
*        OTHER INCLUDES                                                         
*-------------------------------------------------------------                  
* ACGENDAY                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACGENDAY                                                       
         PRINT ON                                                               
* DDFLDIND                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDFLDIND                                                       
         PRINT ON                                                               
* ACCATCALLD                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACCATCALLD                                                     
         PRINT ON                                                               
TABLE    CSECT                                                                  
         DS    CL(10*POSTLNQ)                                                   
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002ACBAT51   03/13/12'                                      
         END                                                                    
