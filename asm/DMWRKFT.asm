*          DATA SET DMWRKFT    AT LEVEL 001 AS OF 11/17/09                      
*PHASE WRKFTA                                                                   
*INCLUDE DMDMGRL                                                                
*INCLUDE CARDS                                                                  
*INCLUDE DATCON                                                                 
*INCLUDE GETRET                                                                 
*INCLUDE HEXIN                                                                  
*INCLUDE HEXOUT                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE PRINTERL                                                               
*INCLUDE QSORT                                                                  
         TITLE 'WKTEST - TEST REMOTE PRINTING'                                  
         PRINT NOGEN                                                            
WKTEST   CSECT                                                                  
         ENTRY SSB                                                              
         ENTRY UTL                                                              
         ENTRY MASTC                                                            
         NBASE 0,WKTEST,R9,WORK=A(WKWORK)                                       
                                                                                
         L     R2,=V(CPRINT)                                                    
         USING DPRINT,R2                                                        
         MVI   COLSMAX,132                                                      
         MVC   TITLE(30),=CL30'PROGRAM TO TEST DMWRKF ROUTINE'                  
                                                                                
         MVC   WRKFID,=CL8'WRKFU'  SET DEFAULT WRKF AND USER                    
         MVC   USERID,UO           US=TCH1 / UK=DDS2                            
         XC    CARD,CARD                                                        
         EJECT                                                                  
         USING CARDD,CARD                                                       
NEXT     GOTO1 =V(CARDS),DMCB,CARD,=C'RE00'                                     
         CLI   CARD,C'*'                                                        
         BE    NEXT                IGNORE COMMENT CARDS                         
         CLC   CARD(2),=C'/*'                                                   
         BE    EXIT                                                             
                                                                                
         MVC   P,SPACES            PRINT CARD                                   
         GOTO1 =V(PRINTER)                                                      
         MVI   P,C'='                                                           
         MVC   P+1(131),P                                                       
         GOTO1 =V(PRINTER)                                                      
         MVC   P(80),CARD                                                       
         GOTO1 =V(PRINTER)                                                      
         GOTO1 =V(PRINTER)                                                      
                                                                                
         CLC   CARD(6),=C'DDSIO='  DDSIO=XXXXXX (MUST BE 1ST)                   
         BNE   NEXT020                                                          
         L     RF,=V(DDSIO)                                                     
         MVC   0(8,RF),CARD+6                                                   
         B     NEXT                                                             
                                                                                
NEXT020  CLC   CARD(7),=C'DSPACE=' DSPACE=                                      
         BNE   NEXT030                                                          
         L     RF,=A(SSB)                                                       
         MVC   SSODSPAC-SSOOFF(1,RF),CARD+7                                     
         B     NEXT                                                             
*                                                                               
NEXT030  CLC   CARD(6),=C'PRINT='  PRINT=Y/N/E - YES/NO/ERRORS                  
         BNE   NEXT040                                                          
         MVC   PFLAG,CARD+6                                                     
         B     NEXT                                                             
                                                                                
NEXT040  CLC   CARD(8),=C'CLEANUP='  CLEANUP=WRKFX NN WRITE=NO                  
         BNE   NEXT050                                                          
I        USING UKRECD,XX                                                        
         XC    I.UKINDEX,I.UKINDEX                                              
         OC    CARD+14(2),=C'0000'   NUMBER OF HOURS TO GO BACK                 
         PACK  DUB,CARD+14(2)                                                   
         CVB   R0,DUB                                                           
         STH   R0,I.UKINFO                                                      
         MVC   I.UKUSRINF(8),CARD+17 PASS WRITE=NO IS SET                       
*                                                                               
         GOTO1 =V(DATAMGR),DMCB,(0,=C'CLEANUP'),CARD+8,XX,AIO1,ABUFF            
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   P(25),=CL25'PART#1=00000 PART#2=00000'                           
         SR    R0,R0                                                            
         ICM   R0,3,I.UKUSRINF+0   GET PART#1 PURGED COUNT                      
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+7(5),DUB                                                       
         ICM   R0,3,I.UKUSRINF+2   GET PART#2 PURGED COUNT                      
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+20(5),DUB                                                      
         GOTO1 =V(PRINTER)         PRINT PURGED COUNTS                          
         B     NEXT                                                             
                                                                                
NEXT050  LA    RE,CARD             FI=WRKFN/WRKFN                               
         CLC   CARD(4),=C'WRKF'                                                 
         BE    NEXT054                                                          
         CLC   CARD(3),=C'FI='                                                  
         BNE   NEXT100                                                          
         LA    RE,CARD+3                                                        
NEXT054  MVC   WRKFID(5),0(RE)                                                  
         CLI   WRKFID+4,C'A'                                                    
         BL    NEXT070                                                          
         CLI   WRKFID+4,C'G'                                                    
         BNH   NEXT100                                                          
         CLI   WRKFID+4,C'1'                                                    
         BL    NEXT070                                                          
         CLI   WRKFID+4,C'9'                                                    
         BNH   NEXT100                                                          
NEXT070  MVC   P(30),=CL30'INVALID WRKF FILE ID'                                
         GOTO1 =V(PRINTER)                                                      
         B     EXIT                                                             
                                                                                
NEXT100  CLC   CARD(3),=C'DIE'     FIRST THREE BYTES ARE ACTION                 
         BNE   *+6                                                              
         DC    H'0'                                                             
                                                                                
         CLC   CARD(3),=C'INDEX'                                                
         BE    NDX                                                              
         CLC   CARD(3),=C'SEQ'                                                  
         BE    SEQ                                                              
                                                                                
         CLC   CARD(3),=C'ACTV'                                                 
         BE    STA                                                              
         CLC   CARD(3),=C'SENT'                                                 
         BE    STA                                                              
         CLC   CARD(3),=C'HOLD'                                                 
         BE    STA                                                              
         CLC   CARD(3),=C'RETAIN'                                               
         BE    STA                                                              
         CLC   CARD(3),=C'KEEP'                                                 
         BE    STA                                                              
         CLC   CARD(3),=C'UNKEEP'                                               
         BE    STA                                                              
                                                                                
         CLC   CARD(3),=C'READ'                                                 
         BE    WKR                                                              
         CLC   CARD(3),=C'RANDOM'                                               
         BE    RDM                                                              
         CLC   CARD(3),=C'SEQ'                                                  
         BE    SEQ                                                              
         CLC   CARD(3),=C'WRITE'                                                
         BE    WRT                                                              
                                                                                
         CLC   CARD(4),=C'SCAN'                                                 
         BE    SCN                                                              
                                                                                
         CLC   CARD(3),=C'CHK'                                                  
         BNE   *+14                                                             
         L     RF,=A(CHK)                                                       
         BASR  RE,RF                                                            
         B     NEXT                                                             
                                                                                
         CLC   CARD(2),=C'UO'                                                   
         BNE   *+14                                                             
         MVC   USERID,UO                                                        
         B     NEXT                                                             
         CLC   CARD(2),=C'UE'                                                   
         BNE   *+14                                                             
         MVC   USERID,UE                                                        
         B     NEXT                                                             
         CLC   CARD(2),=C'U='                                                   
         BNE   NEXT                                                             
*        *********************     READ USER ID RECORD AND GET NUMBER           
         B     NEXT                                                             
                                                                                
EXIT     XBASE                                                                  
                                                                                
*----------------------------------------------------------------------         
* INDEX CARD FORMAT - INDEX  SPPBDDC NNNN                                       
*  S=(SYSTEM) PP=(PROGRAM) B=(SUBPROGRAM) DD=(DAY) C=(CLASS) N=(F R #)          
*  NNNN=(FILE REFERENCE NUMBER)                                                 
*----------------------------------------------------------------------         
NDX      CLC   CDSYS(10),SPACES                                                 
         BE    NDX020                                                           
                                                                                
         XC    XX,XX               CLEAR USER INDEX                             
I        USING UKRECD,XX                                                        
         MVC   I.UKUSRID,USERID    SET DDS USER ID                              
                                                                                
         CLC   CDSYS(3),SPACES                                                  
         BE    NDX002                                                           
         MVC   I.UKSYSPRG,CDSYS    SET REPORT ID                                
         MVC   I.UKSUBPRG,CDSUB    SET SUB PROGRAM NUMBER                       
                                                                                
NDX002   CLC   CDDAY,SPACES                                                     
         BE    NDX010                                                           
         MVC   DUB(2),CDDAY                                                     
         OC    DUB(2),=C'0000'                                                  
         PACK  DUB1,DUB(2)                                                      
         SR    R0,R0                                                            
         ICM   R0,3,DUB1+6                                                      
         SRL   R0,4                                                             
         STC   R0,I.UKDAY                                                       
                                                                                
NDX010   CLI   CDCLS,C' '                                                       
         BE    *+10                                                             
         MVC   I.UKCLASS,CDCLS     SET REPORT CLASS                             
                                                                                
         CLC   CDREF,SPACES                                                     
         BE    NDX020                                                           
         MVC   DUB(4),CDREF                                                     
         OC    DUB(4),=C'0000'                                                  
         PACK  DUB1,DUB(4)                                                      
         CVB   R0,DUB1                                                          
         LTR   R0,R0                                                            
         BZ    NDXPRT                                                           
         STCM  R0,3,I.UKFILENO     SET FILE REFERENCE NUMBER                    
                                                                                
NDX020   GOTO1 =V(HEXOUT),PARM,XX,P,L'XX,=C'TOG'                                
         GOTO1 =V(PRINTER)                                                      
                                                                                
         GOTO1 =V(DATAMGR),DMCB,(X'08',=C'IND'),WRKFID,XX,AIO1,ABUFF            
         CLI   DMCB+8,0                                                         
         BE    NDXPRT                                                           
                                                                                
         TM    DMCB+8,X'40'                                                     
         BZ    *+6                                                              
         DC    H'0'                DIE IF DISK ERROR                            
         MVC   P,SPACES                                                         
         TM    DMCB+8,X'80'        END OF INDEX                                 
         BZ    *+14                                                             
         MVC   P(30),=CL30'*** END-OF-PART#1 INDEX ***'                         
         B     NDXPRTX                                                          
         TM    DMCB+8,X'10'        REC NOT FOUND                                
         BZ    *+14                                                             
         MVC   P(30),=CL30'*** INDEX REC NOT FOUND ***'                         
         B     NDXPRTX                                                          
         DC    H'0'                                                             
                                                                                
NDXPRT   GOTO1 =V(HEXOUT),PARM,XX,P,L'XX,=C'TOG'                                
                                                                                
NDXPRTX  GOTO1 =V(PRINTER)                                                      
         B     NEXT                                                             
         DROP  I                                                                
         EJECT                                                                  
*----------------------------------------------------------------------         
* READ CARD FORMAT - READ   SPPBDDC NNNN                                        
*  S=(SYSTEM) PP=(PROGRAM) B=(SUBPROGRAM) DD=(DAY) C=(CLASS) N=(F R #)          
*  NNNN=(FILE REFERENCE NUMBER)                                                 
*----------------------------------------------------------------------         
WKR      CLC   CDSYS(10),SPACES                                                 
         BE    WKR020                                                           
                                                                                
         XC    XX,XX               CLEAR USER INDEX                             
I        USING UKRECD,XX                                                        
         MVC   I.UKUSRID,USERID    SET DDS USER ID                              
                                                                                
         CLC   CDSYS(3),SPACES                                                  
         BE    WKR002                                                           
         MVC   I.UKSYSPRG,CDSYS    SET REPORT ID                                
         MVC   I.UKSUBPRG,CDSUB    SET SUB PROGRAM NUMBER                       
                                                                                
WKR002   CLC   CDDAY,SPACES                                                     
         BE    WKR010                                                           
         MVC   DUB(2),CDDAY                                                     
         OC    DUB(2),=C'0000'                                                  
         PACK  DUB1,DUB(2)                                                      
         SR    R0,R0                                                            
         ICM   R0,3,DUB1+6                                                      
         SRL   R0,4                                                             
         STC   R0,I.UKDAY                                                       
                                                                                
WKR010   CLI   CDCLS,C' '                                                       
         BE    *+10                                                             
         MVC   I.UKCLASS,CDCLS     SET REPORT CLASS                             
                                                                                
         CLC   CDREF,SPACES                                                     
         BE    WKR020                                                           
         MVC   DUB(4),CDREF                                                     
         OC    DUB(4),=C'0000'                                                  
         PACK  DUB1,DUB(4)                                                      
         CVB   R0,DUB1                                                          
         LTR   R0,R0                                                            
         BZ    WKRPRT                                                           
         STCM  R0,3,I.UKFILENO     SET FILE REFERENCE NUMBER                    
                                                                                
WKR020   GOTO1 =V(HEXOUT),PARM,XX,P,L'XX,=C'TOG'                                
         GOTO1 =V(PRINTER)                                                      
                                                                                
         GOTO1 =V(DATAMGR),DMCB,(0,=C'READ'),WRKFID,XX,AIO1,ABUFF               
         CLI   DMCB+8,0                                                         
         BE    WKRPRT                                                           
                                                                                
         TM    DMCB+8,X'40'                                                     
         BZ    *+6                                                              
         DC    H'0'                DIE IF DISK ERROR                            
         MVC   P,SPACES                                                         
         TM    DMCB+8,X'80'        END OF INDEX                                 
         BZ    *+14                                                             
         MVC   P(30),=CL30'*** END-OF-FILE ***'                                 
         B     WKRPRTX                                                          
         TM    DMCB+8,X'10'        REC NOT FOUND                                
         BZ    *+14                                                             
         MVC   P(30),=CL30'*** REC NOT FOUND ***'                               
         B     WKRPRTX                                                          
         DC    H'0'                                                             
                                                                                
WKRPRT   L     R5,AIO1                                                          
         GOTO1 =V(HEXOUT),PARM,(R5),P,40,=C'TOG'                                
         GOTO1 =V(PRINTER)                                                      
         AHI   R5,40                                                            
         GOTO1 =V(HEXOUT),PARM,(R5),P,40,=C'TOG'                                
         GOTO1 =V(PRINTER)                                                      
         AHI   R5,40                                                            
         GOTO1 =V(HEXOUT),PARM,(R5),P,40,=C'TOG'                                
                                                                                
WKRPRTX  GOTO1 =V(PRINTER)                                                      
         B     NEXT                                                             
         DROP  I                                                                
         EJECT                                                                  
*----------------------------------------------------------------------         
* RANDOM CARD FORMAT - RANDOM SPPBDDC NNNN                                      
*  S=(SYSTEM) PP=(PROGRAM) B=(SUBPROGRAM) DD=(DAY) C=(CLASS) N=(F R #)          
*  NNNN=(FILE REFERENCE NUMBER)                                                 
*----------------------------------------------------------------------         
RDM      CLC   CDSYS(10),SPACES                                                 
         BE    RDM020                                                           
                                                                                
         XC    XX,XX               CLEAR USER INDEX                             
I        USING UKRECD,XX                                                        
         MVC   I.UKUSRID,USERID    SET DDS USER ID                              
                                                                                
         CLC   CDSYS(3),SPACES                                                  
         BE    RDM002                                                           
         MVC   I.UKSYSPRG,CDSYS    SET REPORT ID                                
         MVC   I.UKSUBPRG,CDSUB    SET SUB PROGRAM NUMBER                       
                                                                                
RDM002   CLC   CDDAY,SPACES                                                     
         BE    RDM010                                                           
         MVC   DUB(2),CDDAY                                                     
         OC    DUB(2),=C'0000'                                                  
         PACK  DUB1,DUB(2)                                                      
         SR    R0,R0                                                            
         ICM   R0,3,DUB1+6                                                      
         SRL   R0,4                                                             
         STC   R0,I.UKDAY                                                       
                                                                                
RDM010   CLI   CDCLS,C' '                                                       
         BE    *+10                                                             
         MVC   I.UKCLASS,CDCLS     SET REPORT CLASS                             
                                                                                
         CLC   CDREF,SPACES                                                     
         BE    RDM020                                                           
         MVC   DUB(4),CDREF                                                     
         OC    DUB(4),=C'0000'                                                  
         PACK  DUB1,DUB(4)                                                      
         CVB   R0,DUB1                                                          
         LTR   R0,R0                                                            
         BZ    RDMPRT                                                           
         STCM  R0,3,I.UKFILENO     SET FILE REFERENCE NUMBER                    
                                                                                
RDM020   GOTO1 =V(HEXOUT),PARM,XX,P,L'XX,=C'TOG'                                
         GOTO1 =V(PRINTER)                                                      
                                                                                
         GOTO1 =V(DATAMGR),DMCB,(0,=C'RANDOM'),WRKFID,XX,AIO1,ABUFF             
         CLI   DMCB+8,0                                                         
         BE    RDMPRT                                                           
                                                                                
         TM    DMCB+8,X'40'                                                     
         BZ    *+6                                                              
         DC    H'0'                DIE IF DISK ERROR                            
         MVC   P,SPACES                                                         
         TM    DMCB+8,X'80'        END OF INDEX                                 
         BZ    *+14                                                             
         MVC   P(30),=CL30'*** END-OF-FILE ***'                                 
         B     RDMPRTX                                                          
         TM    DMCB+8,X'10'        REC NOT FOUND                                
         BZ    *+14                                                             
         MVC   P(30),=CL30'*** REC NOT FOUND ***'                               
         B     RDMPRTX                                                          
         DC    H'0'                                                             
                                                                                
RDMPRT   L     R5,AIO1                                                          
         GOTO1 =V(HEXOUT),PARM,(R5),P,40,=C'TOG'                                
         GOTO1 =V(PRINTER)                                                      
         AHI   R5,40                                                            
         GOTO1 =V(HEXOUT),PARM,(R5),P,40,=C'TOG'                                
         GOTO1 =V(PRINTER)                                                      
         AHI   R5,40                                                            
         GOTO1 =V(HEXOUT),PARM,(R5),P,40,=C'TOG'                                
                                                                                
RDMPRTX  GOTO1 =V(PRINTER)                                                      
         B     NEXT                                                             
         DROP  I                                                                
         EJECT                                                                  
*----------------------------------------------------------------------         
* SEQ CARD FORMAT - SEQ    SPPBDDC NNNN                                         
*  S=(SYSTEM) PP=(PROGRAM) B=(SUBPROGRAM) DD=(DAY) C=(CLASS) N=(F R #)          
*  NNNN=(FILE REFERENCE NUMBER)                                                 
*----------------------------------------------------------------------         
SEQ      CLC   CDSYS(10),SPACES                                                 
         BE    SEQ020                                                           
                                                                                
         XC    XX,XX               CLEAR USER INDEX                             
I        USING UKRECD,XX                                                        
         MVC   I.UKUSRID,USERID    SET DDS USER ID                              
                                                                                
         CLC   CDSYS(3),SPACES                                                  
         BE    SEQ002                                                           
         MVC   I.UKSYSPRG,CDSYS    SET REPORT ID                                
         MVC   I.UKSUBPRG,CDSUB    SET SUB PROGRAM NUMBER                       
                                                                                
SEQ002   CLC   CDDAY,SPACES                                                     
         BE    SEQ010                                                           
         MVC   DUB(2),CDDAY                                                     
         OC    DUB(2),=C'0000'                                                  
         PACK  DUB1,DUB(2)                                                      
         SR    R0,R0                                                            
         ICM   R0,3,DUB1+6                                                      
         SRL   R0,4                                                             
         STC   R0,I.UKDAY                                                       
                                                                                
SEQ010   CLI   CDCLS,C' '                                                       
         BE    *+10                                                             
         MVC   I.UKCLASS,CDCLS     SET REPORT CLASS                             
                                                                                
         CLC   CDREF,SPACES                                                     
         BE    SEQ020                                                           
         MVC   DUB(4),CDREF                                                     
         OC    DUB(4),=C'0000'                                                  
         PACK  DUB1,DUB(4)                                                      
         CVB   R0,DUB1                                                          
         LTR   R0,R0                                                            
         BZ    SEQPRT                                                           
         STCM  R0,3,I.UKFILENO     SET FILE REFERENCE NUMBER                    
                                                                                
SEQ020   GOTO1 =V(HEXOUT),PARM,XX,P,L'XX,=C'TOG'                                
         GOTO1 =V(PRINTER)                                                      
                                                                                
         GOTO1 =V(DATAMGR),DMCB,(0,=C'SEQ   '),WRKFID,XX,AIO1,ABUFF             
         CLI   DMCB+8,0                                                         
         BE    SEQPRT                                                           
                                                                                
         TM    DMCB+8,X'40'                                                     
         BZ    *+6                                                              
         DC    H'0'                DIE IF DISK ERROR                            
         MVC   P,SPACES                                                         
         TM    DMCB+8,X'80'        END OF INDEX                                 
         BZ    *+14                                                             
         MVC   P(30),=CL30'*** END-OF-FILE ***'                                 
         B     SEQPRTX                                                          
         TM    DMCB+8,X'10'        REC NOT FOUND                                
         BZ    *+14                                                             
         MVC   P(30),=CL30'*** REC NOT FOUND ***'                               
         B     SEQPRTX                                                          
         DC    H'0'                                                             
                                                                                
SEQPRT   L     R5,AIO1                                                          
         GOTO1 =V(HEXOUT),PARM,(R5),P,40,=C'TOG'                                
         GOTO1 =V(PRINTER)                                                      
         AHI   R5,40                                                            
         GOTO1 =V(HEXOUT),PARM,(R5),P,40,=C'TOG'                                
         GOTO1 =V(PRINTER)                                                      
         AHI   R5,40                                                            
         GOTO1 =V(HEXOUT),PARM,(R5),P,40,=C'TOG'                                
                                                                                
SEQPRTX  GOTO1 =V(PRINTER)                                                      
         B     NEXT                                                             
         DROP  I                                                                
         EJECT                                                                  
*----------------------------------------------------------------------         
* STA CARD FORMAT STA SPPBNNNNC XXX                                             
*  S=(SYSTEM) PP=(PROGRAM) B=(SUBPROGRAM) NNNN=(FILE REF #) C=(CLASS)           
*----------------------------------------------------------------------         
STA      CLC   CDSYS(10),SPACES                                                 
         BE    STA020                                                           
                                                                                
         XC    XX,XX               CLEAR USER INDEX                             
I        USING UKRECD,XX                                                        
         MVC   I.UKUSRID,USERID    SET DDS USER ID                              
                                                                                
         CLC   CDSYS(3),SPACES                                                  
         BE    STA002                                                           
         MVC   I.UKSYSPRG,CDSYS    SET REPORT ID                                
         MVC   I.UKSUBPRG,CDSUB    SET SUB PROGRAM NUMBER                       
                                                                                
STA002   CLC   CDDAY,SPACES                                                     
         BE    STA010                                                           
         MVC   DUB(2),CDDAY                                                     
         OC    DUB(2),=C'0000'                                                  
         PACK  DUB1,DUB(2)                                                      
         SR    R0,R0                                                            
         ICM   R0,3,DUB1+6                                                      
         SRL   R0,4                                                             
         STC   R0,I.UKDAY                                                       
                                                                                
STA010   CLI   CDCLS,C' '                                                       
         BE    *+10                                                             
         MVC   I.UKCLASS,CDCLS     SET REPORT CLASS                             
                                                                                
         CLC   CDREF,SPACES                                                     
         BE    STA020                                                           
         MVC   DUB(4),CDREF                                                     
         OC    DUB(4),=C'0000'                                                  
         PACK  DUB1,DUB(4)                                                      
         CVB   R0,DUB1                                                          
         LTR   R0,R0                                                            
         BZ    STAPRT                                                           
         STCM  R0,3,I.UKFILENO     SET FILE REFERENCE NUMBER                    
                                                                                
STA020   GOTO1 =V(HEXOUT),PARM,XX,P,L'XX,=C'TOG'                                
         GOTO1 =V(PRINTER)                                                      
                                                                                
         GOTO1 =V(DATAMGR),DMCB,(X'08',=C'IND'),WRKFID,XX,AIO1,ABUFF            
         CLI   DMCB+8,0                                                         
         BE    STA050                                                           
                                                                                
STA030   ST    R1,FUL              SET EOF/ERR CONDITION                        
         TM    DMCB+8,X'40'                                                     
         BZ    *+6                                                              
         DC    H'0'                DIE IF DISK ERROR                            
         MVC   P,SPACES                                                         
         TM    DMCB+8,X'80'                                                     
         BZ    *+14                                                             
         MVC   P(30),=CL30'*** END-OF-INDEX ***'                                
         B     STAPRT                                                           
         TM    DMCB+8,X'10'                                                     
         BZ    *+14                                                             
         MVC   P(30),=CL30'*** INDEX REC NOT FOUND ***'                         
         B     STAPRT                                                           
         DC    H'0'                                                             
                                                                                
STA050   XC    FUL,FUL                                                          
         GOTO1 =V(HEXOUT),PARM,XX,P,32,=C'TOG'                                  
         GOTO1 =V(DATAMGR),DMCB,(X'00',CARD),WRKFID,XX,AIO1,ABUFF               
         CLI   DMCB+8,0                                                         
         BNE   STA030                                                           
         MVC   P+70(30),=CL30'STATUS CHANGED TO XXXXXX'                         
         MVC   P+88(6),CARD                                                     
                                                                                
STAPRT   GOTO1 =V(PRINTER)                                                      
         B     NEXT                                                             
                                                                                
*----------------------------------------------------------------------         
* WRT CARD FORMAT WRT SPPBNNNNC XXX                                             
*  S=(SYSTEM) PP=(PROGRAM) B=(SUBPROGRAM) NNNN=(FILE REF #) C=(CLASS)           
*----------------------------------------------------------------------         
*RT      CLC   CDSYS(10),SPACES                                                 
*        BE    WRT020                                                           
*                                                                               
         USING WLHDRD,R3                                                        
WRT      L     R3,AIO1                                                          
         XC    0(255,R3),0(R3)           BUILD HEADER                           
         XC    XX,XX                                                            
                                                                                
         MVC   WLSOFLAB,=C'*SOFSOF*'                                            
         MVC   WLUSRID,USERID                                                   
         MVC   WLDESC,=CL16'WRKF TESTS   '                                      
         MVC   WLSYSPRG,=C'WRK'          SET REPORT ID                          
         MVC   WLSUBPRG,=C'F'            SET SUB PROGRAM NUMBER                 
         MVI   WLDAY,17                                                         
         MVC   WLCLASS,=C'T'             SET REPORT CLASS                       
                                                                                
         MVC   WLSYSPRG,CDSYS            SET REPORT ID                          
         MVC   WLSUBPRG,CDSUB            SET SUB PROGRAM NUMBER                 
         MVC   WLCLASS,CDCLS             SET REPORT CLASS                       
                                                                                
         CLC   CDDAY,SPACES                                                     
         BE    WRT010                                                           
         MVC   DUB(2),CDDAY                                                     
         OC    DUB(2),=C'0000'                                                  
         PACK  DUB1,DUB(2)                                                      
         SR    R0,R0                                                            
         ICM   R0,3,DUB1+6                                                      
         SRL   R0,4                                                             
         STC   R0,WLDAY                                                         
WRT010   MVI   WLTYPE,0                                                         
         MVC   XX,0(R3)                                                         
                                                                                
         GOTO1 =V(DATAMGR),DMCB,=C'DMPRINT ',WRKFID,XX,(R3),ABUFF               
         CLI   8(R1),0                                                          
         BNE   WRT030                                                           
                                                                                
         XC    0(255,R3),0(R3)           BUILD HEADER                           
         MVC   4(50,R3),=CL50'THIS IS A TEST USING DMWRKFT OFFLINE'             
         MVC   0(2,R3),=AL2(50)                                                 
                                                                                
         GOTO1 =V(DATAMGR),DMCB,=C'DMPRINT ',WRKFID,XX,(R3),ABUFF               
         CLI   8(R1),0                                                          
         BNE   WRT030                                                           
                                                                                
         XC    0(255,R3),0(R3)           BUILD HEADER                           
         MVC   WLSOFLAB,=C'*EOFEOF*'                                            
                                                                                
         GOTO1 =V(DATAMGR),DMCB,=C'DMPRINT ',WRKFID,XX,(R3),ABUFF               
         CLI   8(R1),0                                                          
         BNE   WRT030                                                           
                                                                                
WRT020   GOTO1 =V(HEXOUT),PARM,XX,P,L'XX,=C'TOG'                                
         GOTO1 =V(PRINTER)                                                      
         B     WRT050                                                           
                                                                                
WRT030   ST    R1,FUL              SET EOF/ERR CONDITION                        
         TM    DMCB+8,X'40'                                                     
         BZ    *+6                                                              
         DC    H'0'                DIE IF DISK ERROR                            
         MVC   P,SPACES                                                         
         TM    DMCB+8,X'80'                                                     
         BZ    *+14                                                             
         MVC   P(30),=CL30'*** END-OF-FILE ***'                                 
         B     WRTPRT                                                           
         TM    DMCB+8,X'10'                                                     
         BZ    *+14                                                             
         MVC   P(30),=CL30'*** REC NOT FOUND ***'                               
         B     WRTPRT                                                           
         DC    H'0'                                                             
                                                                                
WRT050   XC    FUL,FUL                                                          
         GOTO1 =V(HEXOUT),PARM,XX,P,L'XX,=C'TOG'                                
         MVC   P+70(30),=CL30'WORKER FILE ADDED'                                
                                                                                
WRTPRT   GOTO1 =V(PRINTER)                                                      
         B     NEXT                                                             
                                                                                
*----------------------------------------------------------------------         
* SCAN INDEX FOR FORMAT INTEGRITY                                               
*----------------------------------------------------------------------         
SCN      CLC   CDSYS(10),SPACES                                                 
         BE    SCN020                                                           
                                                                                
         XC    XX,XX               CLEAR USER INDEX                             
I        USING UKRECD,XX                                                        
         MVC   I.UKUSRID,USERID    SET DDS USER ID                              
                                                                                
         CLC   CDSYS(3),SPACES                                                  
         BE    SCN002                                                           
         MVC   I.UKSYSPRG,CDSYS    SET REPORT ID                                
         MVC   I.UKSUBPRG,CDSUB    SET SUB PROGRAM NUMBER                       
                                                                                
SCN002   CLC   CDDAY,SPACES                                                     
         BE    SCN010                                                           
         MVC   DUB(2),CDDAY                                                     
         OC    DUB(2),=C'0000'                                                  
         PACK  DUB1,DUB(2)                                                      
         SR    R0,R0                                                            
         ICM   R0,3,DUB1+6                                                      
         SRL   R0,4                                                             
         STC   R0,I.UKDAY                                                       
                                                                                
SCN010   CLI   CDCLS,C' '                                                       
         BE    *+10                                                             
         MVC   I.UKCLASS,CDCLS     SET REPORT CLASS                             
                                                                                
         CLC   CDREF,SPACES                                                     
         BE    SCN020                                                           
         MVC   DUB(4),CDREF                                                     
         OC    DUB(4),=C'0000'                                                  
         PACK  DUB1,DUB(4)                                                      
         CVB   R0,DUB1                                                          
         LTR   R0,R0                                                            
         BZ    SCNPRT                                                           
         STCM  R0,3,I.UKFILENO     SET FILE REFERENCE NUMBER                    
                                                                                
SCN020   GOTO1 =V(HEXOUT),PARM,XX,P,L'XX,=C'TOG'                                
         GOTO1 =V(PRINTER)                                                      
                                                                                
         GOTO1 =V(DATAMGR),DMCB,(X'08',=C'IND'),WRKFID,XX,AIO1,ABUFF            
         CLI   DMCB+8,0                                                         
         BE    SCNPRT                                                           
                                                                                
         TM    DMCB+8,X'40'                                                     
         BZ    *+6                                                              
         DC    H'0'                DIE IF DISK ERROR                            
         MVC   P,SPACES                                                         
         TM    DMCB+8,X'80'        END OF INDEX                                 
         BZ    *+14                                                             
         MVC   P(30),=CL30'*** END-OF-INDEX ***'                                
         B     SCNPRTX                                                          
         TM    DMCB+8,X'10'        REC NOT FOUND                                
         BZ    *+14                                                             
         MVC   P(30),=CL30'*** REC NOT FOUND ***'                               
         B     SCNPRTX                                                          
         DC    H'0'                                                             
                                                                                
SCNPRT   GOTO1 =V(HEXOUT),PARM,XX,P,L'XX,=C'TOG'                                
                                                                                
SCNPRTX  GOTO1 =V(PRINTER)                                                      
         B     NEXT                                                             
         DROP  I                                                                
         EJECT                                                                  
*----------------------------------------------------------------------         
* ROUTINE TO TEST DATAMGR RETURN CODE AND OUTPUT MESSAGE                        
*----------------------------------------------------------------------         
CHKERR   ST    RE,CHKERRRE                                                      
         CLI   8(R1),0                                                          
         BE    CHKERRX                                                          
         L     RF,=A(WFERRS)                                                    
CHKERR2  CLI   0(RF),0             TEST END OF TABLE - UNKNOWN ERROR            
         BE    CHKERR3                                                          
         CLC   0(1,RF),8(R1)                                                    
         BE    CHKERR3                                                          
         LA    RF,L'WFERRS(RF)                                                  
         B     CHKERR2                                                          
CHKERR3  MVC   CHKERRM1(3),0(RF)   DMCB+8/ABEND CODE                            
         MVC   CHKERRM4(25),3(RF)  TEXT                                         
CHKERR4  L     RF,8(RD)            LOOK FOR DMWRKF'S WORKING STORAGE            
         CLC   0(4,RF),=C'WRKF'                                                 
         BE    CHKERR4A                                                         
         L     RF,8(RF)                                                         
         CLC   0(4,RF),=C'WRKF'                                                 
         BNE   CHKERR5                                                          
CHKERR4A LA    RF,72(RF)                                                        
         CLC   0(8,RF),=C'WFWSINFO'                                             
         BNE   CHKERR5                                                          
         CLC   12(4,RF),=C'WRKF'   TEST IF USERWRKF NAME FILLED IN              
         BNE   CHKERR5                                                          
         MVC   CHKERRM4+4(1),16(RF)                                             
         LA    RF,24(RF)           POINT TO REPTINFO                            
         OC    0(2,RF),0(RF)                                                    
         BZ    CHKERR5                                                          
         MVC   CHKERRM5(3),2(RF)   REPORT ID                                    
         MVI   CHKERRM5+3,C','                                                  
         SR    R0,R0                                                            
         ICM   R0,3,5(RF)                                                       
         CVD   R0,CHKERRDW                                                      
         OI    CHKERRDW+7,X'0F'                                                 
         UNPK  CHKERRM5+4(5),CHKERRDW                                           
CHKERR5  LA    R3,CHKERRM3         POINT TO MESSAGE LENGTH                      
         WTO   TEXT=(R3)                                                        
         LA    R1,DMCB                                                          
         SR    R0,R0                                                            
         IC    R0,8(R1)            R0=DMCB+8                                    
         STC   R0,CHKERRM1                                                      
CHKERR6  LA    R1,DMCB                                                          
         SR    R0,R0                                                            
         IC    R0,CHKERRM1         R0=DMCB+8                                    
         LH    R3,CHKERRM2         R3=ABEND CODE                                
         ABEND (3)                                                              
CHKERRX  L     RE,CHKERRRE                                                      
         BR    RE                                                               
                                                                                
CHKERRDW DC    D'0'                                                             
CHKERRRE DC    F'0'                                                             
CHKERRMR DC    H'0'                                                             
         DC    X'00'               ALIGN                                        
CHKERRM1 DC    X'00'               DMCB+8                                       
CHKERRM2 DC    H'0'                ABEND CODE                                   
CHKERRM3 DC    H'36'               WTO MSG LENGTH                               
CHKERRM4 DC    CL26' '             WTO MSG TEXT                                 
CHKERRM5 DC    CL10' '             WTO MSG TEXT                                 
CHKWRK   DC    CL16' '                                                          
         DS    0F                                                               
WFERRS   DS    0XL28                                                            
         DC    X'80',AL2(130),CL25'WRKF--EOF'                                   
         DC    X'81',AL2(131),CL25'WRKF--EOF NO PART#1 CIS'                     
         DC    X'82',AL2(132),CL25'WRKF--EOF NO PART#2 CIS'                     
         DC    X'84',AL2(133),CL25'WRKF--REPORT TOO BIG'                        
         DC    X'88',AL2(134),CL25'WRKF--USERID MAX REPORTS'                    
         DC    X'40',AL2(135),CL25'WRKF--DISK ERROR'                            
         DC    X'41',AL2(136),CL25'WRKF--FORMAT ERR ON OPEN'                    
         DC    X'42',AL2(137),CL25'WRKF--FORMAT ERR ON CLOSE'                   
WFERRSX  DC    X'00',AL2(138),CL25'WRKF--UNKNOWN ERROR'                         
         EJECT                                                                  
*----------------------------------------------------------------------         
* STANDARD DMWRKFR ROUTINES - MUST START AFTER 4K TO USE R9 BASE                
*----------------------------------------------------------------------         
       ++INCLUDE DMWRKFR                                                        
         EJECT                                                                  
*----------------------------------------------------------------------         
                                                                                
DUB      DS    D                                                                
DUB1     DS    D                                                                
DUB2     DS    D                                                                
FUL      DS    F                                                                
DSKADR   DS    F                                                                
P1       DS    6F                                                               
DMCB     DS    6F                                                               
DMCB1    DS    6F                                                               
PARM     DS    6F                                                               
WRKFID   DS    CL8                                                              
LASTREPT DC    XL8'00'                                                          
INREPT   DC    XL8'00'                                                          
                                                                                
ACXREC   DC    A(CXREC)                                                         
ABUFF    DC    A(BUFF)                                                          
ABUFF1   DC    A(BUFF1)                                                         
AIO1     DC    A(IO1)                                                           
AIO2     DC    A(IO2)                                                           
                                                                                
TTREPS   DS    F                                                                
TTSGLS   DS    F                                                                
TTDBLS   DS    F                                                                
TTBADS   DS    F                                                                
TTMIXS   DS    F                                                                
TTTRKS   DS    F                                                                
TOTRKSW  DS    F                                                                
TOPART#2 DS    F                                                                
TNTRKSW  DS    F                                                                
TNPART#2 DS    F                                                                
                                                                                
TCIL#1   DS    F                                                                
TCIL#2   DS    F                                                                
TCID#1   DS    F                                                                
TCID#2   DS    F                                                                
CIATABN  DS    F                                                                
CIATABP  DS    F                                                                
CIATABL  DS    F                                                                
CIATABT  DS    F                                                                
CIATABA  DS    A                                                                
                                                                                
LINES    DS    H                                                                
PAGES    DS    H                                                                
LINNO    DS    PL4                                                              
PAGNO    DS    PL4                                                              
                                                                                
PFLAG    DC    C'Y'                SET BY PRINT=Y/N/E CARD                      
         DC    C' '                                                             
TYPEWF   DC    C'O'                NEW=N 20-BIT OLD=O 16-BIT                    
THISWF   DC    C'O'                SET BY DMWRKF BUFF CALL                      
                                                                                
CARD     DS    CL80                CURRENT CARD                                 
                                                                                
         DS    0D                                                               
         DC    C'*NDXNDX*'                                                      
XX       DC    XL56'00'            INDEX                                        
                                                                                
SOFLAB   DC    X'00',C'SOF',X'00'                                               
EOFLAB   DC    X'FF',C'EOF',X'FF'                                               
                                                                                
USERID   DS    H                                                                
*&&UK                                                                           
UE       DC    H'38'               DDS1...EVEN                                  
UO       DC    H'33'               DDS2...ODD                                   
*&&                                                                             
*&&US                                                                           
UO       DC    H'43'               TCH1...ODD                                   
UE       DC    H'236'              TCH2...EVEN                                  
*&&                                                                             
SAVEI    DC    CL198' '                                                         
SAVEP    DC    CL198' '                                                         
         LTORG                                                                  
         DS    0D                                                               
         DC    C'*KFWKFW*'                                                      
*DMWRKFW                                                                        
       ++INCLUDE DMWRKFW                                                        
         DS    2F                                                               
                                                                                
         DS    0D                                                               
         DC    C'*CXRCXR*'                                                      
CXREC    DC    14336X'00'                                                       
                                                                                
         DS    0D                                                               
         DC    C'*SOBSOB*'                                                      
BUFF     DC    14336X'00'                                                       
         DC    C'*EOBEOB*'                                                      
                                                                                
         DS    0D                                                               
         DC    C'*IO1IO1*'                                                      
IO1      DS    2048X                                                            
         DS    0D                                                               
         DC    C'*IO2IO2*'                                                      
IO2      DS    2048X                                                            
                                                                                
BUFF1    DC    14336X'00'                                                       
                                                                                
         DS    0D                                                               
         DC    C'*CIATAB*'                                                      
CIATAB   DS    64000F                                                           
CIATABX  DC    8X'FF'                                                           
                                                                                
         DC    C'*WRKWRK*'                                                      
WKWORK   DS    10000D                                                           
                                                                                
                                                                                
         DS    0D                                                               
         DC    CL8'SSB*SSB*'                                                    
SSB      DC    X'0000FF',X'40',4X'00',CL8' ',32X'00',A(MASTC),204X'00'          
UTL      DC    F'0',X'01',XL3'00',XL248'00'                                     
                                                                                
         DC    CL8'*MSC*MSC'                                                    
MASTC    DC    1932X'00'                                                        
MCARC    DC    CL72' '                                                          
         DC    44X'00'                                                          
         EJECT                                                                  
*----------------------------------------------------------------------         
* CHK CARD FORMAT - CHK    SPPBDDC NNNN                                         
*  S=(SYSTEM) PP=(PROGRAM) B=(SUBPROGRAM) DD=(DAY) C=(CLASS) N=(F R #)          
*  NNNN=(FILE REFERENCE NUMBER)                                                 
*----------------------------------------------------------------------         
CHK      NTR1  BASE=*                                                           
         L     R4,ABUFF            INIT BUFFER - MUST KNOW WRKFID               
         USING W_RECD,R4                                                        
         GOTO1 =V(DATAMGR),DMCB,=C'BUFF',WRKFID,XX,AIO1,ABUFF                   
         LA    RE,12(R4)           RE=A(CIDATA)                                 
         MVC   CIDATA(64),0(RE)    MOVE WRKF DATA                               
         MVC   CINDXLN,=H'24'                                                   
         MVI   THISWF,C'N'         NEW STYLE HAS 20-BIT DISK ADDRESS            
         TM    CIDATA+37,X'40'                                                  
         BO    *+8                                                              
         MVI   THISWF,C'O'         OLD STYLE HAS 16-BIT DISK ADDRESS            
         MVC   TYPEWF,THISWF                                                    
                                                                                
CHK001   XC    XX,XX               CLEAR USER INDEX                             
I        USING UKRECD,XX                                                        
         CLC   CDSYS(10),SPACES    TEST IF ANY FILTERS SPECIFIED                
         BE    CHK020                                                           
         MVC   I.UKUSRID,USERID    SET DDS USER ID                              
*                                                                               
CHK002   CLC   CDSYS(3),SPACES     TEST IF SPP SPECIFIED                        
         BE    CHK004                                                           
         MVC   I.UKSYSPRG,CDSYS    SET REPORT ID                                
         MVC   I.UKSUBPRG,CDSUB    SET SUB PROGRAM NUMBER                       
*                                                                               
CHK004   CLC   CDDAY,SPACES        TEST IF DAY SPECIFIED                        
         BE    CHK006                                                           
         MVC   DUB(2),CDDAY                                                     
         OC    DUB(2),=C'0000'                                                  
         PACK  DUB1,DUB(2)                                                      
         SR    R0,R0                                                            
         ICM   R0,3,DUB1+6                                                      
         SRL   R0,4                                                             
         STC   R0,I.UKDAY                                                       
*                                                                               
CHK006   CLI   CDCLS,C' '          TEST IF CLASS SPECIFIED                      
         BE    CHK008                                                           
         MVC   I.UKCLASS,CDCLS                                                  
*                                                                               
CHK008   CLC   CDREF,SPACES        TEST IF FILE NUMBER SPECIFIED                
         BE    CHK020                                                           
         MVC   DUB(4),CDREF                                                     
         OC    DUB(4),=C'0000'                                                  
         PACK  DUB1,DUB(4)                                                      
         CVB   R0,DUB1                                                          
         LTR   R0,R0                                                            
         BZ    *+4                                                              
         STCM  R0,3,I.UKFILENO     SET FILE REFERENCE NUMBER                    
                                                                                
CHK020   XC    TCIL#1,TCIL#1       CLEAR TOTAL COUNTERS                         
         XC    TCIL#2,TCIL#2                                                    
         XC    TCID#1,TCID#1                                                    
         XC    TCID#2,TCID#2                                                    
         XC    TTMIXS,TTMIXS                                                    
         XC    TTBADS,TTBADS                                                    
         L     R0,=A(CIATAB)       CLEAR CI ADDR TABLE                          
         ST    R0,CIATABA                                                       
         XC    CIATABN,CIATABN                                                  
         XC    CIATABP,CIATABP                                                  
         XC    CIATABL,CIATABL                                                  
         XC    CIATABT,CIATABT                                                  
         L     R1,=A(CIATABX)                                                   
         SR    R1,R0                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
CHKLOOP  XC    FUL,FUL                                                          
         GOTO1 =V(DATAMGR),DMCB,(X'08',=C'IND'),WRKFID,XX,AIO1,ABUFF            
         CLI   DMCB+8,0                                                         
         BE    CHKRCI                                                           
         ST    R1,FUL              SET EOF/ERR CONDITION                        
         TM    DMCB+8,X'40'                                                     
         BZ    *+6                                                              
         DC    H'0'                DIE IF DISK ERROR                            
         MVC   P,SPACES                                                         
         TM    DMCB+8,X'80'        END OF INDEX                                 
         BO    CHKPRTA                                                          
         TM    DMCB+8,X'10'        REC NOT FOUND                                
         BZ    *+14                                                             
         MVC   P(30),=CL30'*** INDEX REC NOT FOUND ***'                         
         B     CHKPRTA                                                          
         DC    H'0'                                                             
                                                                                
CHKRCI   ICM   R0,7,I.UKCIADDR-1   R0=..0TTTTT                                  
         SLL   R0,12                                                            
         ST    R0,DSKADR                                                        
         MVI   DSKADR+3,1          DSKADR=TTTTT001                              
         CLI   THISWF,C'N'                                                      
         BE    CHKRCI1                                                          
         MVC   DSKADR(2),I.UKCIADDR                                             
         MVC   DSKADR+2(2),=X'0100'                                             
         DROP  R4                                                               
CHKRCI1  L     R5,ABUFF1           R5=A(FIRST CI REC IN BUFF1)                  
         USING W_RECD,R5                                                        
         GOTO1 =V(DATAMGR),DMCB1,=C'DMREAD',WRKFID,DSKADR,(R5)                  
         CLI   8(R1),0                                                          
         BNE   CHKBAD                                                           
CHKRCI2  SR    RE,RE               RE=NUM OF PART#1 CIS                         
         SR    RF,RF               RF=NUM OF PART#2 CIS                         
         TM    W_TYPE,0            W_TYNEW                                      
         BO    CHKRCI3                                                          
         CLI   THISWF,C'O'         TEST REPORT TYPE WITH WF TYPE                
         BE    CHKRCI2A                                                         
         L     R0,TTMIXS           BUMP MIS MATCH TYPE COUNT                    
         AHI   R0,1                                                             
         ST    R0,TTMIXS                                                        
         MVI   P+48,C'>'                                                        
         MVI   P+52,C'1'           REPORT TYPE NOT EQUAL TO FILE TYPE           
         B     CHKRCI3                                                          
CHKRCI2A IC    RE,W_NCI            OLD WF VALUES                                
         CHI   RE,1                                                             
         BE    CHKRCI4                                                          
         IC    RF,W_NCIX                                                        
         AR    RF,RE                                                            
         AHI   RF,-1                                                            
         LA    RE,1                                                             
         B     CHKRCI4                                                          
CHKRCI3  CLI   THISWF,C'N'         TEST REPORT TYPE WITH WF TYPE                
         BE    CHKRCI3A                                                         
         L     R0,TTMIXS           BUMP MIS MATCH TYPE COUNT                    
         AHI   R0,1                                                             
         ST    R0,TTMIXS                                                        
         MVI   P+48,C'>'                                                        
         MVI   P+52,C'1'           REPORT TYPE NOT EQUAL TO FILE TYPE           
         B     CHKRCI2A                                                         
CHKRCI3A IC    RE,W_NCI            NEW WF VALUES                                
         CHI   RE,1                                                             
         BE    CHKRCI4                                                          
         IC    RF,W_NCIX                                                        
         AR    RF,RE                                                            
         AHI   RF,-1                                                            
         LA    RE,1                                                             
CHKRCI4  TM    I.UKSTAT,X'C0'      TEST LIVE INDEX ENTRY                        
         BZ    CHKRCI5                                                          
         A     RE,TCIL#1           BUMP LIVE PART#1 CI COUNT                    
         ST    RE,TCIL#1                                                        
         A     RF,TCIL#2           BUMP LIVE PART#2 CI COUNT                    
         ST    RF,TCIL#2                                                        
         B     CHKRCI6                                                          
CHKRCI5  A     RE,TCID#1           BUMP DEAD PART#1 CI COUNT                    
         ST    RE,TCID#1                                                        
         A     RF,TCID#2           BUMP DEAD PART#2 CI COUNT                    
         ST    RF,TCID#2                                                        
CHKRCI6  B     CHKPRT                                                           
                                                                                
CHKBAD   L     RF,TTBADS           BUMP BAD REPORT COUNTS                       
         AHI   RF,1                                                             
         ST    RF,TTBADS                                                        
         GOTO1 =V(HEXOUT),PARM,XX,P,24,=C'TOG'                                  
         MVC   P+48(5),=C'>NDX3'   WRKF FILE READ ERROR                         
         GOTO1 =V(HEXOUT),PARM,I.UKCIADDR,P+53,2,=C'TOG'                        
         MVC   P+60(7),=C'DSKADR='                                              
         GOTO1 =V(HEXOUT),PARM,DSKADR,P+67,4,=C'TOG'                            
         CLI   PFLAG,C'N'                                                       
         BE    CHKLOOP                                                          
         GOTO1 =V(PRINTER)                                                      
         B     CHKLOOP             BACK FOR NEXT INDEX ENTRY                    
                                                                                
CHKPRT   GOTO1 =V(HEXOUT),PARM,XX,P,24,=C'TOG'                                  
         MVC   P+49(3),=C'NDX'                                                  
         GOTO1 =V(HEXOUT),PARM,I.UKCIADDR,P+53,2,=C'TOG'                        
         MVC   SAVEI,P                                                          
         CLI   PFLAG,C'Y'          PRINT INDEX ENTRY IF PRINT=Y                 
         BNE   *+10                                                             
         GOTO1 =V(PRINTER)                                                      
         MVC   P,SPACES                                                         
         B     CHKPRT2                                                          
*                                                                               
CHKPRT1  GOTO1 =V(DATAMGR),DMCB1,=C'DMREAD',WRKFID,DSKADR,(R5)                  
         CLI   8(R1),0                                                          
         BNE   CHKBAD                                                           
*                                                                               
CHKPRT2  GOTO1 =V(HEXOUT),PARM,(R5),P,24,=C'TOG'                                
         SR    R0,R0                                                            
         IC    R0,W_SEQ                                                         
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+49(3),DUB+6(2)                                                 
         GOTO1 =V(HEXOUT),PARM,W_BATTR,P+53,16,=C'TOG'                          
         CLI   W_SEQ,1             GET FIRST PART OF CI DATA                    
         BH    CHKPRT2A                                                         
         LA    RF,W_DATA                                                        
         CLI   THISWF,C'N'                                                      
         BE    *+8                                                              
         LA    RF,W_DATA                                                        
         GOTO1 =V(HEXOUT),PARM,(RF),P+86,18,=C'TOG'                             
*                                                                               
CHKPRT2A MVC   SAVEP,P             SAVE THIS PRINT LINE                         
         CLC   XX(7),0(R5)                                                      
         BE    CHKPRT2B                                                         
         MVI   P+48,C'>'                                                        
         MVI   P+52,C'2'           INDEX KEY NEQ CI KEY                         
         MVC   SAVEP,P                                                          
CHKPRT2B CLI   PFLAG,C'N'                                                       
         BE    CHKPRT2N            PRINT=N                                      
         BH    CHKPRT2D            PRINT=Y                                      
         CLI   P+48,C'>'           PRINT=E PRINT IF ERROR                       
         BNE   CHKPRT2N                                                         
         CLI   SAVEI,C' '          TEST IF PRINTED SAVED INDEX ENTRY            
         BE    CHKPRT2C                                                         
         MVC   P,SAVEI                                                          
         GOTO1 =V(PRINTER)                                                      
         MVC   SAVEI,SPACES                                                     
CHKPRT2C MVC   P,SAVEP                                                          
CHKPRT2D GOTO1 =V(PRINTER)         PRINT CI DATA                                
CHKPRT2N MVC   P,SPACES                                                         
*                                                                               
CHKPRT3  CLI   THISWF,C'N'         GET NEXT CI DISK ADDR                        
         BE    CHKPRT3A                                                         
         OC    W_CINEXT,W_CINEXT                                                
         BZ    CHKPRT4                                                          
         MVC   DSKADR(2),W_CINEXT                                               
         MVC   DSKADR+2(2),=X'0100'                                             
         B     CHKPRT3B                                                         
*                                                                               
CHKPRT3A OC    W_CINEXT,W_CINEXT                                                
         BZ    CHKPRT4                                                          
         MVC   DSKADR,W_CINEXT                                                  
CHKPRT3B L     RE,CIATABA                                                       
         CLC   0(4,RE),=4X'FF'     TEST IF SPACE IN TABLE                       
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   0(4,RE),DSKADR      MOVE PART2 CI ADDR TO TABLE                  
         LA    RE,4(RE)                                                         
         ST    RE,CIATABA                                                       
         L     RE,CIATABN          BUMP NUMBER OF TABLE ENTRIES                 
         LA    RE,1(RE)                                                         
         ST    RE,CIATABN                                                       
         B     CHKPRT1                                                          
CHKPRT4  OC    FUL,FUL                                                          
         BZ    CHKLOOP                                                          
                                                                                
CHKPRTA  MVC   P(30),=CL30'*** END-OF-PART#1 INDEX ***'                         
         GOTO1 =V(PRINTER)                                                      
         OC    FUL,FUL                                                          
         BZ    CHKLOOP                                                          
         CLC   CDSYS(10),SPACES    TEST IF ANY FILTERS SPECIFIED                
         BNE   CHKT                                                             
                                                                                
CHKPRTB  XC    DMCB(24),DMCB       SORT LIST OF PART2 CI ADDRS                  
         L     R6,=A(CIATAB)                                                    
         ST    R6,DMCB             R6=A(FIRST ENTRY)                            
         ICM   R0,15,CIATABN                                                    
         BZ    CHKT                                                             
         ST    R0,DMCB+4           R0=NUM OF ENTRIES                            
         MVC   DMCB+8(4),=F'4'     LEN OF RECORD                                
         MVC   DMCB+12(4),=F'4'    LEN OF KEY                                   
         GOTO1 =V(QSORT),DMCB                                                   
                                                                                
CHK2     CLC   THISWF,TYPEWF       CAN ONLY CHK PART2 INDEX IF MATCH            
         BNE   CHKT                                                             
         BAS   RE,CXLOOPJ          POINT TO START OF PART2 INDEX                
         USING W_RECD,R5           R5=A(WRKF INDEX ENTRY)                       
         L     R6,=A(CIATAB)                                                    
*                                                                               
CHK2A    BAS   RE,GETXAD                                                        
         GOTO1 =V(DATAMGR),DMCB,(0,=C'DMREAD'),WRKFID,CXADDR,CXREC              
         CLI   8(R1),0                                                          
         BE    CHK2B                                                            
         DC    H'0'                                                             
*                                                                               
CHK2B    BAS   RE,GETCAD           SET CIADDR TO CI DISK ADDRESS                
         CLC   CIADDR,0(R6)        IS CIADDR NEXT IN TABLE                      
         BNE   CHK2D               NO                                           
         CLI   W_STAT,W_STPU       YES CANT BE PURGED                           
         BE    CHK2C                                                            
         LA    R6,4(R6)            THIS IS OK SO BUMP TO NEXT                   
         B     CHK2N                                                            
*                                                                               
CHK2C    MVC   P+48(5),=C'>NDX4'   ERROR PURGED PART2 IN TABLE                  
         LA    RF,24                                                            
         GOTO1 =V(HEXOUT),PARM,(R5),P,(RF),=C'TOG'                              
         GOTO1 =V(HEXOUT),PARM,CIADDR,P+53,4,=C'TOG'                            
         CLI   PFLAG,C'N'                                                       
         BE    *+10                                                             
         GOTO1 =V(PRINTER)                                                      
         LA    R6,4(R6)                                                         
         B     CHK2N                                                            
*                                                                               
CHK2D    CLI   W_STAT,W_STPU       CIADDR NOT IN TABLE                          
         BNE   CHK2E               THIS IS OK IF ITS PURGED                     
         L     R1,CIATABP                                                       
         LA    R1,1(R1)                                                         
         ST    R1,CIATABP          BUMP PURGED PART2 CIS                        
         B     CHK2N                                                            
*                                                                               
CHK2E    MVC   P+48(5),=C'>NDX5'   ERROR NON PURGED PART2 NOT IN TAB            
         TM    W_STAT,W_STCRE                                                   
         BZ    CHK2F                                                            
         CLI   W_AGERT,X'FF'                                                    
         BNE   CHK2F                                                            
         MVC   P+48(5),=C'>NDX6'   ERROR TEMPORARY PART2 NOT IN TAB             
CHK2F    LA    RF,24                                                            
         GOTO1 =V(HEXOUT),PARM,(R5),P,(RF),=C'TOG'                              
         GOTO1 =V(HEXOUT),PARM,CIADDR,P+53,4,=C'TOG'                            
         CLI   PFLAG,C'N'                                                       
         BE    *+10                                                             
         GOTO1 =V(PRINTER)                                                      
         TM    W_STAT,W_STCRE                                                   
         BZ    CHK2G                                                            
         CLI   W_AGERT,X'FF'                                                    
         BNE   CHK2G                                                            
         L     R1,CIATABT                                                       
         LA    R1,1(R1)                                                         
         ST    R1,CIATABT          BUMP TEMP PART2 CIS                          
         B     CHK2N                                                            
CHK2G    L     R1,CIATABL                                                       
         LA    R1,1(R1)                                                         
         ST    R1,CIATABL          BUMP LOST PART2 CIS                          
         B     CHK2N                                                            
*                                                                               
CHK2N    BAS   RE,CXLOOPX          BUMP TO NEXT INDEX ENTRY                     
         B     CHK2B                                                            
         B     CHK2A               END OF PAGE                                  
*                                                                               
         MVC   P(30),=CL30'*** END-OF-PART#2 INDEX ***'                         
         GOTO1 =V(PRINTER)                                                      
                                                                                
CHKT     GOTO1 =V(PRINTER)                                                      
         MVC   P(15),=CL15'WRKFX CI TOTALS'                                     
         MVC   P(5),WRKFID                                                      
         GOTO1 =V(PRINTER)                                                      
         GOTO1 =V(PRINTER)                                                      
         LA    R7,P                                                             
*                                                                               
         MVC   0(7,R7),=C'TOTL#1='                                              
         SR    R0,R0                                                            
         ICM   R0,3,CICITOT                                                     
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  7(5,R7),DUB                                                      
         LA    R7,13(R7)                                                        
*                                                                               
         MVC   0(7,R7),=C'LIVE#1='                                              
         L     R0,TCIL#1                                                        
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  7(5,R7),DUB                                                      
         LA    R7,13(R7)                                                        
*                                                                               
         MVC   0(7,R7),=C'DEAD#1='                                              
         L     R0,TCID#1                                                        
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  7(5,R7),DUB                                                      
         LA    R7,13(R7)                                                        
*                                                                               
         ST    R7,FUL              SAVE WHERE PART#2 DATA STARTS                
*                                                                               
         MVC   0(7,R7),=C'TOTL#2='                                              
         SR    R0,R0                                                            
         ICM   R0,3,CJCITOT                                                     
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  7(5,R7),DUB                                                      
         LA    R7,13(R7)                                                        
*                                                                               
         MVC   0(7,R7),=C'LIVE#2='                                              
         L     R0,TCIL#2                                                        
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  7(5,R7),DUB                                                      
         LA    R7,13(R7)                                                        
*                                                                               
         MVC   0(7,R7),=C'DEAD#2='                                              
         L     R0,TCID#2                                                        
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  7(5,R7),DUB                                                      
         LA    R7,13(R7)                                                        
*                                                                               
         GOTO1 =V(PRINTER)                                                      
*                                                                               
CHKT2    CLC   THISWF,TYPEWF       NO PART#2 COUNTERS IF MIS MATCH              
         BNE   CHKX                                                             
         L     R7,FUL              POSITION TO PART#2 DATA                      
*                                                                               
         MVC   0(7,R7),=C'PRGD#2='                                              
         L     R0,CIATABP                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  7(5,R7),DUB                                                      
         LA    R7,13(R7)                                                        
*                                                                               
         MVC   0(7,R7),=C'LOST#2='                                              
         L     R0,CIATABL                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  7(5,R7),DUB                                                      
         LA    R7,13(R7)                                                        
*                                                                               
         MVC   0(7,R7),=C'TEMP#2='                                              
         L     R0,CIATABT                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  7(5,R7),DUB                                                      
         LA    R7,13(R7)                                                        
*                                                                               
CHKTX    GOTO1 =V(PRINTER)                                                      
                                                                                
CHKX     XIT1                                                                   
         DROP  I                                                                
         DROP  R5                                                               
         USING W_RECD,R4                                                        
         LTORG                                                                  
*----------------------------------------------------------------------         
* CARD DSECT                                                                    
*----------------------------------------------------------------------         
CARDD    DSECT                                                                  
CDCMD    DS    CL7             COMMAND                                          
         DS    CL1                                                              
CDSYS    DS    CL1             SYSTEM                                           
CDPRG    DS    CL2             PROGRAM                                          
CDSUB    DS    CL1             SUB PROGRAM                                      
CDDAY    DS    CL2             DAY                                              
CDCLS    DS    CL1             CLASS                                            
         DS    CL1                                                              
CDREF    DS    CL4             FILE REFERENCE #                                 
*----------------------------------------------------------------------         
                                                                                
*DMWRKFX                                                                        
       ++INCLUDE DMWRKFX                                                        
                                                                                
*DMWRKFD                                                                        
       ++INCLUDE DMWRKFD                                                        
         EJECT                                                                  
*DMWRKFK                                                                        
       ++INCLUDE DMWRKFK                                                        
         EJECT                                                                  
*DMWRKFL                                                                        
       ++INCLUDE DMWRKFL                                                        
         EJECT                                                                  
*DMWRKFS                                                                        
       ++INCLUDE DMWRKFS                                                        
         EJECT                                                                  
*DDDPRINTL                                                                      
       ++INCLUDE DDDPRINTL                                                      
         EJECT                                                                  
SSOD     DSECT                                                                  
*FASSBOFF                                                                       
       ++INCLUDE FASSBOFF                                                       
                                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'001DMWRKFT   11/17/09'                                      
         END                                                                    
