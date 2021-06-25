*          DATA SET ACLFM29    AT LEVEL 015 AS OF 05/01/02                      
*PHASE T60329A,+0                                                               
         TITLE 'SALES/USE TAX RECORDS'                                          
T60329   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 LWSX-LWSD,T60329,CLEAR=YES                                       
         LR    R9,RC                                                            
         USING LWSD,R9                                                          
         L     RA,0(R1)                                                         
         USING T603FFD,RA                                                       
         L     RC,4(R1)                                                         
         USING LOGWORKD,RC                                                      
         EJECT                                                                  
*              BUILD KEY                                                        
         SPACE 2                                                                
         CLI   MODE,BUILDKEY                                                    
         BNE   DSPLR                                                            
         XC    TAXNAM1,TAXNAM1     CLEAR LOCALITY NAME FIELDS                   
         OI    TAXNAM1H+6,X'80'                                                 
         XC    TAXNAM2,TAXNAM2                                                  
         OI    TAXNAM2H+6,X'80'                                                 
         XC    TAXNAM3,TAXNAM3                                                  
         OI    TAXNAM3H+6,X'80'                                                 
         XC    TAXACNM,TAXACNM      AND CREDIT ACCOUNT NAME FIELD               
         OI    TAXACNMH+6,X'80'                                                 
         LA    R2,TAXCODXH                                                      
         GOTO1 ANY                                                              
         MVI   ERROR,2                                                          
         CLI   TAXCODXH+5,8        LENGTH OF TAX CODE                           
         BH    XIT                                                              
         ZIC   R3,TAXCODXH+5                                                    
         MVC   LWKEY,SPACES                                                     
         LA    R4,LWKEY            BUILD TAX KEY                                
         USING ACKEYD,R4                                                        
         MVI   ACUTTYPE,ACUTEQU                                                 
         MVI   ACUTSREC,ACUTSEQU                                                
         MVC   ACUTCMP,COMPANY                                                  
         MVC   ACUTLOC(2),TAXCODX                                               
         OC    ACUTLOC(2),SPACES                                                
         SH    R3,=H'2'                                                         
         BNP   BLDK10              ONE LEVEL OF CODE                            
         BAS   RE,RDHIGH                                                        
         CLC   LWKEY,LWIO                                                       
         BNE   ERR01               MISSING LEVEL ONE                            
         LA    R6,LWIO                                                          
         LA    R7,TAXNAM1                                                       
         BAS   RE,GETNAME                                                       
*                                                                               
         MVC   ACUTLOC(4),TAXCODX                                               
         OC    ACUTLOC(4),SPACES                                                
         SH    R3,=H'2'            NOW GET LEVEL 2 ACCOUNT                      
         BNP   BLDK10              END OF KEY FIELD                             
         BAS   RE,RDHIGH                                                        
         CLC   LWKEY,LWIO                                                       
         BNE   ERR02               MISSING LEVEL TWO                            
         LA    R6,LWIO                                                          
         LA    R7,TAXNAM2                                                       
         BAS   RE,GETNAME                                                       
*                                                                               
         MVC   ACUTLOC(6),TAXCODX                                               
         OC    ACUTLOC(6),SPACES                                                
         SH    R3,=H'2'            NOW GET LEVEL 2 ACCOUNT                      
         BNP   BLDK10              END OF KEY FIELD                             
         BAS   RE,RDHIGH                                                        
         CLC   LWKEY,LWIO                                                       
         BNE   ERR03               MISSING LEVEL THREE                          
         LA    R6,LWIO                                                          
         LA    R7,TAXNAM3                                                       
         BAS   RE,GETNAME                                                       
*                                                                               
         MVC   ACUTLOC(8),TAXCODX   SET FOR FULL KEY                            
         OC    ACUTLOC(8),SPACES                                                
*                                                                               
BLDK10   MVI   ERROR,X'FF'                                                      
         MVC   KEY,LWKEY                                                        
         TM    TAXCODXH+4,X'20'                                                 
         BNO   *+12                PREVIOUSLY VERIFIED- NO KEY CHANGE           
         LA    R2,TAXNAMXH                                                      
         B     XIT                                                              
*                                                                               
         MVI   ANYKEY,C'Y'         KEY CHANGE                                   
         OI    TAXCODXH+4,X'20'                                                 
         B     XIT                                                              
         EJECT                                                                  
*              DISPLAY ELEMENTS                                                 
         SPACE 2                                                                
DSPLR    CLI   MODE,DSPLYREC                                                    
         BNE   BLDR00                                                           
         LA    R6,IO               DISPLAY RECORD IN IO1                        
         BAS   RE,DSPL                                                          
         B     XIT                                                              
*                                                                               
*              DISPLAY ROUTINE                                                  
*                                                                               
DSPL     NTR1                                                                   
         TWAXC TAXNAMXH,TAXTABH                                                 
         LA    R7,TAXNAMX                                                       
         BAS   RE,GETNAME          DISPLAY THIS ACCOUNT NAME                    
*                                                                               
         LA    R2,TAXEFFH          EFFECTIVE DATES AND RATES                    
         MVI   ELCODE,X'5F'                                                     
         BAS   RE,GETEL                                                         
         BNE   XIT                                                              
*                                                                               
         USING ACTAXEL,R6                                                       
DSPL10   GOTO1 DATCON,DMCB,(1,ACTAXEFF),(8,8(R2))                               
         ZIC   R1,0(R2)                                                         
         AR    R2,R1                                                            
         EDIT  ACTAXRTE,(7,8(R2)),4,DROP=3,ALIGN=LEFT                           
         ZIC   R1,0(R2)                                                         
         AR    R2,R1                                                            
         CLI   ACTAXLEN,ACTAXLQ2   IF THIS ELEMENT HAS CREDIT ACCOUNT           
         BL    DSPL20                                                           
*                                                                               
         MVC   TAXACC,ACTAXACC                                                  
         LA    R4,LWKEY            BUILD KEY FOR CREDIT ACCOUNT                 
         USING ACKEYD,R4                                                        
         MVC   ACKEYACC(42),SPACES                                              
         MVC   ACKEYACC(1),COMPANY                                              
         MVC   ACKEYACC+1(14),ACTAXACC                                          
         BAS   RE,RDHIGH           READ CREDIT ACCOUNT                          
         CLC   LWKEY,LWIO                                                       
         BNE   DSPL20                                                           
         ST    R6,SAVR6            SAVE ADDRESS OF CURRENT ELEMENT              
         LA    R6,LWIO                                                          
         LA    R7,TAXACNM                                                       
         BAS   RE,GETNAME          AND GET THE NAME                             
         L     R6,SAVR6                                                         
*                                                                               
DSPL20   MVI   ELCODE,X'5F'        GET THE NEXT RATE                            
         BAS   RE,NEXTEL                                                        
         BE    DSPL10                                                           
         LA    R2,TAXCODXH                                                      
         MVI   ERROR,X'FF'                                                      
         B     XIT                                                              
*                                                                               
         EJECT                                                                  
*              BUILD ELEMENTS                                                   
         SPACE 2                                                                
BLDR00   DS    0H                                                               
         CLI   MODE,BUILDREC                                                    
         BNE   XIT                                                              
         GOTO1 REMANEL,DMCB,(X'5F',0)                                           
         LA    R2,TAXNAMXH                                                      
         CLI   5(R2),0             IF NO NAME USE NAME FROM                     
         BE    BLDR08              CREDIT ACCOUNT                               
         CLI   5(R2),6                                                          
         BNE   BLDR07                                                           
         CLC   8(6,R2),=C'DELETE'                                               
         BNE   BLDR07                                                           
         MVI   ERROR,2             ACTION MUST BE AMEND FOR DELETE              
         CLI   LOGACT,C'A'                                                      
         BNE   XIT                                                              
         MVC   SVKEY,KEY                                                        
         GOTO1 SEQ                 GET NEXT RECORD                              
         MVI   ERROR,67            NOT VALID FOR DELETION                       
         LA    R1,14                                                            
         LA    R3,SVKEY+14                                                      
BLDR04   CLI   0(R3),C' '          FIND LAST CHARACTER IN SAVED KEY             
         BH    BLDR05                                                           
         BCTR  R3,0                                                             
         BCT   R1,BLDR04                                                        
         DC    H'0'                BAD KEY                                      
BLDR05   EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   KEY(0),SVKEY        IF NEXT RECORD IS PART OF THIS               
         BE    XIT                 FAMILY, IT CAN'T BE DELETED                  
         LA    R4,IO2                                                           
         USING ACKEYD,R4                                                        
         OI    ACSTATUS,X'80'                                                   
         MVI   ERROR,X'FF'                                                      
         B     XIT                DELETE IT                                     
*                                                                               
BLDR07   GOTO1 NAMIN               GET LEVEL NAME                               
*                                                                               
BLDR08   LA    R2,TAXACCH          CREDIT ACCOUNT                               
         GOTO1 ANY                                                              
         LA    R1,CREDLST                                                       
BLDR09   CLC   0(2,R1),TAXACC      VALID UNIT AND LEDGER                        
         BE    BLDR10                                                           
         LA    R1,2(R1)                                                         
         CLI   0(R1),X'FF'                                                      
         BNE   BLDR09                                                           
         B     ERR06                                                            
*                                                                               
BLDR10   LA    R6,ELEMENT                                                       
         USING ACTAXD,R6                                                        
         XC    ELEMENT,ELEMENT                                                  
         MVI   ACTAXEL,X'5F'                                                    
         MVI   ACTAXLEN,ACTAXLQ2                                                
         MVC   ACTAXACC,SPACES                                                  
         ZIC   R3,5(R2)                                                         
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   ACTAXACC(0),8(R2)   SAVE CREDIT ACCOUNT CODE                     
*                                                                               
         LA    R4,LWKEY            BUILD KEY FOR CREDIT ACCOUNT                 
         USING ACKEYD,R4                                                        
         MVC   ACKEYACC(42),SPACES                                              
         MVC   ACKEYACC(1),COMPANY                                              
         MVC   ACKEYACC+1(14),ACTAXACC                                          
         BAS   RE,RDHIGH           READ CREDIT ACCOUNT                          
         CLC   LWKEY,LWIO                                                       
         BNE   ERR06               INVALID ACCOUNT                              
         LA    R6,LWIO                                                          
         LA    R7,TAXACNM                                                       
         BAS   RE,GETNAME          AND GET THE NAME                             
*                                                                               
BLDR15   MVI   ELCODE,X'32'                                                     
         LA    R6,LWIO                                                          
         BAS   RE,GETEL                                                         
         BNE   ERR07               INVALID FOR POSTING                          
*                                                                               
         EJECT                                                                  
**             VALIDATE EFFECTIVE DATE AND REAT                                 
**                                                                              
         LA    R2,TAXEFFH                                                       
         B     BLDR32             CHECK IF ANY INPUT                            
*                                                                               
         USING ACTAXD,R6                                                        
BLDR20   LA    R6,ELEMENT                                                       
         GOTO1 DATCON,DMCB,(5,0),(1,ACTAXEFF) DEFAULT IS TODAY                  
         CLI   5(R2),0                                                          
         BE    BLDR25                NO DATE INPUT                              
         GOTO1 DATVAL,DMCB,(0,8(R2)),WORK                                       
         OC    DMCB(4),DMCB                                                     
         BNZ   *+12                                                             
         MVI   ERROR,DATERR        INVALID DATE                                 
         B     XIT                                                              
         GOTO1 DATCON,DMCB,(0,WORK),(1,ACTAXEFF)                                
*                                                                               
BLDR25   ZIC   R1,0(R2)            VALIDATE THE RATE                            
         AR    R2,R1                                                            
         GOTO1 ANY                                                              
         ZIC   R3,5(R2)                                                         
         GOTO1 CASHVAL,DMCB,(4,8(R2)),(R3)                                      
         CLI   DMCB,X'FF'                                                       
         BE    ERR05                                                            
         L     R3,DMCB+4                                                        
         CVD   R3,DUB                                                           
         ZAP   ACTAXRTE,DUB                                                     
         CP    ACTAXRTE,=P'1'                                                   
         BNH   ERR05                                                            
         CP    ACTAXRTE,=P'999999'                                              
         BH    ERR05                                                            
         GOTO1 ADDANEL                                                          
*                                                                               
         LA    R6,ELEMENT          SET FOR NEXT ELEMENT                         
         USING ACTAXD,R6                                                        
         XC    ELEMENT,ELEMENT                                                  
         MVI   ACTAXEL,X'5F'                                                    
         MVI   ACTAXLEN,ACTAXLQ1                                                
*                                                                               
BLDR30   ZIC   R1,0(R2)            NEXT DATE FIELD                              
         AR    R2,R1                                                            
         LA    R3,TAXTABH                                                       
         CR    R2,R3                                                            
         BNL   BLDR40              END OF SCREEN                                
BLDR32   CLI   5(R2),0                                                          
         BNE   BLDR20                                                           
         ZIC   R1,0(R2)                                                         
         LA    R3,0(R1,R2)         R3 TO NEXT RATE FIELD                        
         CLI   5(R3),0             IF EITHER NOT ZERO - VALIDATE                
         BNE   BLDR20                                                           
         LR    R2,R3                                                            
         B     BLDR30              BOTH ZERO SKIP TO NEXT LINE                  
*                                                                               
BLDR40   CLI   TAXNAMXH+5,0                                                     
         BNE   BLDR45              ALREADY HAVE NAME                            
*                                                                               
         LA    R6,LWIO                                                          
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL            ELSE GET NAME FROM CREDIT ACCOUNT            
         BE    *+6                                                              
         DC    H'0'                                                             
         USING ACNAMED,R6                                                       
         ZIC   R3,ACNMLEN                                                       
         SH    R3,=H'2'                                                         
         STC   R3,TAXNAMXH+5                                                    
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   TAXNAMX(0),ACNMNAME                                              
         LA    R2,TAXNAMXH                                                      
         GOTO1 NAMIN               ADD NAME ELEMENT TO TAX RECORD               
*                                                                               
BLDR45   LA    R2,TAXEFFH          EFFECTIVE DATES AND RATES                    
         MVI   ELCODE,X'5F'                                                     
         LA    R6,IO2              DISPLAY NEW RECORD                           
         BAS   RE,GETEL                                                         
         BE    BLDR46                                                           
         GOTO1 ANY                  MUST HAVE AT LEAST ONE RATE                 
*                                                                               
BLDR46   LA    R2,TAXCODXH                                                      
         LA    R6,IO2              DISPLAY NEW RECORD                           
         BAS   RE,DSPL                                                          
XIT      XIT1  REGS=(R2)                                                        
         EJECT                                                                  
*              SUBROUTINES                                                      
*                                                                               
RDHIGH   ST    RE,SAVRE                                                         
         GOTO1 DATAMGR,DMCB,(0,=C'DMRDHI'),=C'ACCOUNT',LWKEY,LWIO               
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         L     RE,SAVRE                                                         
         BR    RE                                                               
*                                                                               
*               MOVE NAME TO SCREEN FIELD                                       
*                                                                               
GETNAME  NTR1                                                                   
         MVC   0(36,R7),SPACES                                                  
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING ACNAMED,R6                                                       
         ZIC   R3,ACNMLEN                                                       
         SH    R3,=H'3'                                                         
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R7),ACNMNAME                                                 
         B     XIT                                                              
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
         EJECT                                                                  
ERR01    MVI   ERROR,X'FE'                                                      
         MVC   LOGHEAD(40),=CL40' ** ERROR **  INVALID LEVEL 1 RECORD'          
         B     XIT                                                              
*                                                                               
ERR02    MVI   ERROR,X'FE'                                                      
         MVC   LOGHEAD(40),=CL40' ** ERROR **  INVALID LEVEL 2 RECORD'          
         B     XIT                                                              
*                                                                               
ERR03    MVI   ERROR,X'FE'                                                      
         MVC   LOGHEAD(40),=CL40' ** ERROR **  INVALID LEVEL 3 RECORD'          
         B     XIT                                                              
*                                                                               
ERR05    MVI   ERROR,CASHERR       INVALID AMOUNT                               
         B     XIT                                                              
ERR06    MVI   ERROR,17            INVALID ACCOUNT                              
         B     XIT                                                              
ERR07    MVI   ERROR,18            INVALID ACCOUNT FOR POSTING                  
         B     XIT                                                              
*                                                                               
*              VALID CREDIT U/L                                                 
CREDLST  DC    C'SBSCSVSXSE',X'FF'                                              
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*              DSECT FOR LOCAL WORKING STORAGE                                  
LWSD     DSECT                                                                  
ELCODE   DS    CL1                                                              
SAVRE    DS    F                                                                
SAVR6    DS    F                                                                
SVKEY    DS    CL32                                                             
LWKEY    DS    CL42                                                             
LWIO     DS    CL2000                                                           
LWSX     DS    0C                                                               
         EJECT                                                                  
       ++INCLUDE ACLFMFFD                                                       
         ORG   LOGTABH                                                          
       ++INCLUDE ACLFMDAD                                                       
*        ACLFMWORK                                                              
*        ACGENBOTH                                                              
*        ACGENFILE                                                              
*        ACLFMEQU                                                               
*        DDLFDIND                                                               
         PRINT OFF                                                              
       ++INCLUDE ACLFMWORK                                                      
       ++INCLUDE ACGENBOTH                                                      
       ++INCLUDE ACGENFILE                                                      
       ++INCLUDE ACLFMEQU                                                       
       ++INCLUDE DDFLDIND                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'015ACLFM29   05/01/02'                                      
         END                                                                    
