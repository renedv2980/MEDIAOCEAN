*          DATA SET DMWRKZT    AT LEVEL 001 AS OF 03/22/11                      
*PHASE WRKZTA                                                                   
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
                                                                                
         L     R8,=V(CPRINT)                                                    
         USING DPRINT,R8                                                        
         MVI   COLSMAX,132                                                      
         MVC   TITLE(30),=CL30'PROGRAM TO TEST DMWRKZ ROUTINE'                  
                                                                                
         MVC   WRKZID,=CL8'WRKZIL'  SET DEFAULT WRKZ AND USER                   
         MVC   USERID,UO           US=TCH1 / UK=DDS2                            
         XC    CARD,CARD                                                        
*                                                                               
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
                                                                                
NEXT030  CLC   CARD(6),=C'PRINT='  PRINT=Y/N/E - YES/NO/ERRORS                  
         BNE   NEXT035                                                          
         MVC   PFLAG,CARD+6                                                     
         B     NEXT                                                             
                                                                                
NEXT035  CLC   CARD(6),=C'WRITE='  WRITE=Y/N                                    
         BNE   NEXT040                                                          
         MVC   WFLAG,CARD+6                                                     
         CLI   WFLAG,C'Y'                                                       
         BE    NEXT                                                             
         CLI   WFLAG,C'N'                                                       
         BE    NEXT                                                             
         MVC   P(15),=CL15'*** INVALID ***'                                     
         GOTO1 =V(PRINTER)                                                      
         B     EXIT                                                             
                                                                                
NEXT040  CLC   CARD(8),=C'CLEANUP='  CLEANUP=WRKZX NN WRITE=NO                  
         BNE   NEXT050                                                          
I        USING UKRECD,XX                                                        
         XC    I.UKINDEX,I.UKINDEX                                              
         OC    CARD+14(2),ZEROS    NUMBER OF HOURS TO GO BACK                   
         PACK  DUB,CARD+14(2)                                                   
         CVB   R0,DUB                                                           
         STH   R0,I.UKINFO                                                      
                                                                                
         MVC   UPDATE,WFLAG        SET TO VALUE IN WRITE=Y/N CARD               
         CLC   CARD+17(6),=C'WRITE='                                            
         BNE   *+10                                                             
         MVC   UPDATE,CARD+23      CAN BE MODIFIED ON THIS CARD                 
         CLI   UPDATE,C' '                                                      
         BNE   *+8                                                              
         MVI   UPDATE,C'Y'         THIS DEFAULTS TO UPDATIVE                    
         CLI   UPDATE,C'N'                                                      
         BNE   NEXT045                                                          
         MVC   I.UKUSRINF(8),=C'WRITE=NO'                                       
                                                                                
NEXT045  GOTO1 =V(DATAMGR),DMCB,(0,=C'CLEANUP'),CARD+8,XX,AIO1,ABUFF            
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
                                                                                
NEXT050  LA    RE,CARD             FI=WRKZN/WRKZN                               
         CLC   CARD(4),=C'WRKZ'                                                 
         BE    NEXT054                                                          
         CLC   CARD(3),=C'FI='                                                  
         BNE   NEXT100                                                          
         LA    RE,CARD+3                                                        
NEXT054  MVC   WRKZID(5),0(RE)                                                  
         CLI   WRKZID+4,C'A'                                                    
         BL    NEXT070                                                          
         CLI   WRKZID+4,C'F'                                                    
         BNH   NEXT100                                                          
         CLI   WRKZID+4,C'1'                                                    
         BL    NEXT070                                                          
         CLI   WRKZID+4,C'9'                                                    
         BNH   NEXT100                                                          
NEXT070  MVC   P(30),=CL30'INVALID WRKZ FILE ID'                                
         GOTO1 =V(PRINTER)                                                      
         B     EXIT                                                             
                                                                                
NEXT100  CLC   CARD(3),=C'DIE'     FIRST THREE BYTES ARE ACTION                 
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   CARD(3),=C'INDEX'                                                
         BE    NDX                                                              
*                                                                               
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
*                                                                               
         CLC   CARD(3),=C'READ'                                                 
         BE    WKR                                                              
         CLC   CARD(3),=C'RANDOM'                                               
         BE    RDM                                                              
         CLC   CARD(3),=C'SEQ'                                                  
         BE    SEQ                                                              
         CLC   CARD(3),=C'WRITE'                                                
         BE    WRT                                                              
*                                                                               
         CLC   CARD(4),=C'SCAN'                                                 
         BE    SCN                                                              
*                                                                               
         CLC   CARD(3),=C'CHK'                                                  
         BNE   NEXT110                                                          
         L     RF,=A(CHK)                                                       
         BASR  RE,RF                                                            
         B     NEXT                                                             
                                                                                
NEXT110  CLC   CARD(2),=C'ANA'                                                  
         BNE   NEXT120                                                          
         L     RF,=A(ANA)                                                       
         BASR  RE,RF                                                            
         B     NEXT                                                             
                                                                                
NEXT120  CLC   CARD(2),=C'UO'                                                   
         BNE   NEXT130                                                          
         MVC   USERID,UO                                                        
         B     NEXT                                                             
NEXT130  CLC   CARD(2),=C'UE'                                                   
         BNE   NEXT140                                                          
         MVC   USERID,UE                                                        
         B     NEXT                                                             
NEXT140  CLC   CARD(2),=C'U='                                                   
         BNE   NEXT                                                             
         LHI   R1,6                                                             
         LA    R2,CARD+7                                                        
NEXT142  CLI   0(R2),C' '                                                       
         BH    NEXT144                                                          
         BCTR  R2,0                                                             
         BCT   R1,NEXT142                                                       
         DC    H'0'                                                             
NEXT144  BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,CARD+2(0)                                                    
         CVB   R1,DUB                                                           
         STCM  R1,3,USERID                                                      
         B     NEXT                                                             
                                                                                
EXIT     XBASE                                                                  
XIT      XIT1                                                                   
                                                                                
***********************************************************************         
* INDEX CARD FORMAT - INDEX  SPPBDDC NNNN                                       
*  S=(SYSTEM) PP=(PROGRAM) B=(SUBPROGRAM) DD=(DAY) C=(CLASS) N=(F R #)          
*  NNNN=(FILE REFERENCE NUMBER)                                                 
***********************************************************************         
NDX      CLC   CDSYS(10),SPACES                                                 
         BE    NDX020                                                           
*                                                                               
         XC    XX,XX               CLEAR USER INDEX                             
I        USING UKRECD,XX                                                        
         MVC   I.UKUSRID,USERID    SET DDS USER ID                              
*                                                                               
         CLC   CDSYS(3),SPACES                                                  
         BE    NDX002                                                           
         MVC   I.UKSYSPRG,CDSYS    SET FILE ID                                  
         MVC   I.UKSUBPRG,CDSUB    SET SUB PROGRAM NUMBER                       
*                                                                               
NDX002   CLC   CDDAY,SPACES                                                     
         BE    NDX010                                                           
         MVC   DUB(2),CDDAY                                                     
         OC    DUB(2),ZEROS                                                     
         PACK  DUB1,DUB(2)                                                      
         SR    R0,R0                                                            
         ICM   R0,3,DUB1+6                                                      
         SRL   R0,4                                                             
         STC   R0,I.UKDAY                                                       
*                                                                               
NDX010   CLI   CDCLS,C' '                                                       
         BE    *+10                                                             
         MVC   I.UKCLASS,CDCLS     SET FILE CLASS                               
*                                                                               
         CLC   CDREF,SPACES                                                     
         BE    NDX020                                                           
         MVC   DUB(4),CDREF                                                     
         OC    DUB(4),ZEROS                                                     
         PACK  DUB1,DUB(4)                                                      
         CVB   R0,DUB1                                                          
         LTR   R0,R0                                                            
         BZ    NDXPRT                                                           
         STCM  R0,15,I.UKFILENO    SET FILE REFERENCE NUMBER                    
*                                                                               
NDX020   GOTO1 =V(HEXOUT),PARM,XX,P,L'XX,=C'TOG'                                
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         GOTO1 =V(DATAMGR),DMCB,(X'08',=C'IND'),WRKZID,XX,AIO1,ABUFF            
         CLI   DMCB+8,0                                                         
         BE    NDXPRT                                                           
*                                                                               
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
*                                                                               
NDXPRT   GOTO1 =V(HEXOUT),PARM,XX,P,L'XX,=C'TOG'                                
*                                                                               
NDXPRTX  GOTO1 =V(PRINTER)                                                      
         B     NEXT                                                             
         DROP  I                                                                
         EJECT                                                                  
                                                                                
***********************************************************************         
* READ CARD FORMAT - READ   SPPBDDC NNNN                                        
*  S=(SYSTEM) PP=(PROGRAM) B=(SUBPROGRAM) DD=(DAY) C=(CLASS) N=(F R #)          
*  NNNN=(FILE REFERENCE NUMBER)                                                 
***********************************************************************         
WKR      XC    RECNUM,RECNUM                                                    
*                                                                               
         CLC   CDSYS(10),SPACES                                                 
         BE    WKR020                                                           
*                                                                               
         XC    XX,XX               CLEAR USER INDEX                             
I        USING UKRECD,XX                                                        
         MVC   I.UKUSRID,USERID    SET DDS USER ID                              
*                                                                               
         CLC   CDSYS(3),SPACES                                                  
         BE    WKR002                                                           
         MVC   I.UKSYSPRG,CDSYS    SET FILE ID                                  
         MVC   I.UKSUBPRG,CDSUB    SET SUB PROGRAM NUMBER                       
*                                                                               
WKR002   CLC   CDDAY,SPACES                                                     
         BE    WKR010                                                           
         MVC   DUB(2),CDDAY                                                     
         OC    DUB(2),ZEROS                                                     
         PACK  DUB1,DUB(2)                                                      
         SR    R0,R0                                                            
         ICM   R0,3,DUB1+6                                                      
         SRL   R0,4                                                             
         STC   R0,I.UKDAY                                                       
*                                                                               
WKR010   CLI   CDCLS,C' '                                                       
         BE    *+10                                                             
         MVC   I.UKCLASS,CDCLS     SET FILE CLASS                               
*                                                                               
         CLC   CDREF,SPACES                                                     
         BE    WKR020                                                           
         MVC   DUB(4),CDREF                                                     
         OC    DUB(4),ZEROS                                                     
         PACK  DUB1,DUB(4)                                                      
         CVB   R0,DUB1                                                          
         LTR   R0,R0                                                            
         BZ    WKRPRT                                                           
         STCM  R0,15,I.UKFILENO    SET FILE REFERENCE NUMBER                    
         MVI   I.UKFLAG,X'80'                                                   
*                                                                               
WKR020   GOTO1 =V(HEXOUT),PARM,XX,P,L'XX,=C'TOG'                                
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         XC    FUL,FUL                                                          
         GOTO1 =V(DATAMGR),DMCB,(X'08',=C'IND'),WRKZID,XX,AIO1,ABUFF            
         CLI   DMCB+8,0                                                         
         BE    WKR023                                                           
*                                                                               
         ST    R1,FUL              SET EOF/ERR CONDITION                        
         TM    DMCB+8,X'40'                                                     
         BZ    *+6                                                              
         DC    H'0'                DIE IF DISK ERROR                            
         MVC   P,SPACES                                                         
         TM    DMCB+8,X'80'        END OF INDEX                                 
         BO    WKRPRTX                                                          
         TM    DMCB+8,X'10'        REC NOT FOUND                                
         BZ    *+14                                                             
         MVC   P(30),=CL30'*** INDEX REC NOT FOUND ***'                         
         B     WKRPRTX                                                          
         DC    H'0'                                                             
*                                                                               
*                                                                               
WKR023   GOTO1 =V(HEXOUT),PARM,XX,P,L'XX,=C'TOG'                                
         GOTO1 =V(PRINTER)                                                      
*                                                                               
WKR024   ICM   RF,15,RECNUM        BUMP CURRENT RECORD NUMBER                   
         AHI   RF,1                                                             
         STCM  RF,15,RECNUM                                                     
*                                                                               
WKR025   GOTO1 =V(DATAMGR),DMCB,(X'00',=C'READ'),WRKZID,XX,AIO1,ABUFF           
         CLI   8(R1),0             ANYTHING ELSE                                
         BE    WKR030                                                           
         CLI   8(R1),X'80'         TEST EOF                                     
         BE    WKRPRTX                                                          
         CLI   8(R1),X'90'         TEST EOF                                     
         BE    WKRPRTX                                                          
         CLI   8(R1),X'41'         FORMAT ERROR                                 
         BE    *+6                                                              
         DC    H'0'                ALL OTHER ERRORS ARE DEATH                   
         DC    H'0'                                                             
*                                                                               
WKR030   L     R5,AIO1                                                          
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         LHI   R6,2                                                             
WKR033   DS    0H                                                               
         GOTO1 =V(HEXOUT),PARM,(R5),P,20,=C'TOG'                                
         GOTO1 =V(PRINTER)                                                      
         AHI   R5,20                                                            
         BCT   R6,WKR033                                                        
*                                                                               
         ICM   R1,15,RECNUM                                                     
         B     WKR024                                                           
*                                                                               
WKRPRT   LA    R5,XX                                                            
         GOTO1 =V(HEXOUT),PARM,(R5),P,40,=C'TOG'                                
         GOTO1 =V(PRINTER)                                                      
         AHI   R5,40                                                            
         GOTO1 =V(HEXOUT),PARM,(R5),P,40,=C'TOG'                                
         GOTO1 =V(PRINTER)                                                      
         AHI   R5,40                                                            
         GOTO1 =V(HEXOUT),PARM,(R5),P,40,=C'TOG'                                
         MVC   P(30),=CL30'*** FORMAT ERROR **********'                         
*                                                                               
WKRPRTX  GOTO1 =V(PRINTER)                                                      
         B     NEXT                                                             
         DROP  I                                                                
         EJECT                                                                  
                                                                                
***********************************************************************         
* RANDOM CARD FORMAT - RANDOM SPPBDDC NNNN                                      
*  S=(SYSTEM) PP=(PROGRAM) B=(SUBPROGRAM) DD=(DAY) C=(CLASS) N=(F R #)          
*  NNNN=(FILE REFERENCE NUMBER)                                                 
***********************************************************************         
RDM      CLC   CDSYS(10),SPACES                                                 
         BE    RDM020                                                           
*                                                                               
         XC    XX,XX               CLEAR USER INDEX                             
I        USING UKRECD,XX                                                        
         MVC   I.UKUSRID,USERID    SET DDS USER ID                              
*                                                                               
         CLC   CDSYS(3),SPACES                                                  
         BE    RDM002                                                           
         MVC   I.UKSYSPRG,CDSYS    SET FILE ID                                  
         MVC   I.UKSUBPRG,CDSUB    SET SUB PROGRAM NUMBER                       
*                                                                               
RDM002   CLC   CDDAY,SPACES                                                     
         BE    RDM010                                                           
         MVC   DUB(2),CDDAY                                                     
         OC    DUB(2),ZEROS                                                     
         PACK  DUB1,DUB(2)                                                      
         SR    R0,R0                                                            
         ICM   R0,3,DUB1+6                                                      
         SRL   R0,4                                                             
         STC   R0,I.UKDAY                                                       
*                                                                               
RDM010   CLI   CDCLS,C' '                                                       
         BE    *+10                                                             
         MVC   I.UKCLASS,CDCLS     SET FILE CLASS                               
*                                                                               
         CLC   CDREF,SPACES                                                     
         BE    RDM020                                                           
         MVC   DUB(4),CDREF                                                     
         OC    DUB(4),ZEROS                                                     
         PACK  DUB1,DUB(4)                                                      
         CVB   R0,DUB1                                                          
         LTR   R0,R0                                                            
         BZ    RDMPRT                                                           
         STCM  R0,15,I.UKFILENO    SET FILE REFERENCE NUMBER                    
*                                                                               
RDM020   GOTO1 =V(HEXOUT),PARM,XX,P,L'XX,=C'TOG'                                
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         GOTO1 =V(DATAMGR),DMCB,(X'08',=C'IND'),WRKZID,XX,AIO1,ABUFF            
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         ICM   R0,15,I.UKCIADDR                                                 
         ST    R0,DSKADR                                                        
         MVI   DSKADR+3,1                                                       
*                                                                               
         GOTO1 =V(DATAMGR),DMCB1,=C'RANDOM',WRKZID,XX,AIO1,ABUFF                
         CLI   8(R1),0                                                          
         BE    RDMPRT                                                           
*                                                                               
RDM030   TM    DMCB+8,X'40'                                                     
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
*                                                                               
RDMPRT   L     R5,AIO1                                                          
         GOTO1 =V(HEXOUT),PARM,(R5),P,40,=C'TOG'                                
         GOTO1 =V(PRINTER)                                                      
         AHI   R5,40                                                            
         GOTO1 =V(HEXOUT),PARM,(R5),P,40,=C'TOG'                                
         GOTO1 =V(PRINTER)                                                      
         AHI   R5,40                                                            
         GOTO1 =V(HEXOUT),PARM,(R5),P,40,=C'TOG'                                
*                                                                               
RDMPRTX  GOTO1 =V(PRINTER)                                                      
         B     NEXT                                                             
         DROP  I                                                                
         EJECT                                                                  
                                                                                
***********************************************************************         
* SEQ CARD FORMAT - SEQ    SPPBDDC NNNN                                         
*  S=(SYSTEM) PP=(PROGRAM) B=(SUBPROGRAM) DD=(DAY) C=(CLASS) N=(F R #)          
*  NNNN=(FILE REFERENCE NUMBER)                                                 
***********************************************************************         
SEQ      CLC   CDSYS(10),SPACES                                                 
         BE    SEQ020                                                           
*                                                                               
         XC    XX,XX               CLEAR USER INDEX                             
I        USING UKRECD,XX                                                        
         MVC   I.UKUSRID,USERID    SET DDS USER ID                              
*                                                                               
         CLC   CDSYS(3),SPACES                                                  
         BE    SEQ002                                                           
         MVC   I.UKSYSPRG,CDSYS    SET FILE ID                                  
         MVC   I.UKSUBPRG,CDSUB    SET SUB PROGRAM NUMBER                       
*                                                                               
SEQ002   CLC   CDDAY,SPACES                                                     
         BE    SEQ010                                                           
         MVC   DUB(2),CDDAY                                                     
         OC    DUB(2),ZEROS                                                     
         PACK  DUB1,DUB(2)                                                      
         SR    R0,R0                                                            
         ICM   R0,3,DUB1+6                                                      
         SRL   R0,4                                                             
         STC   R0,I.UKDAY                                                       
*                                                                               
SEQ010   CLI   CDCLS,C' '                                                       
         BE    *+10                                                             
         MVC   I.UKCLASS,CDCLS     SET FILE CLASS                               
*                                                                               
         CLC   CDREF,SPACES                                                     
         BE    SEQ020                                                           
         MVC   DUB(4),CDREF                                                     
         OC    DUB(4),ZEROS                                                     
         PACK  DUB1,DUB(4)                                                      
         CVB   R0,DUB1                                                          
         LTR   R0,R0                                                            
         BZ    SEQPRT                                                           
         STCM  R0,15,I.UKFILENO    SET FILE REFERENCE NUMBER                    
*                                                                               
SEQ020   GOTO1 =V(HEXOUT),PARM,XX,P,L'XX,=C'TOG'                                
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         L     R4,ABUFF1           R5=A(FIRST CI REC IN BUFF1)                  
         USING W_RECD,R4                                                        
         GOTO1 =V(DATAMGR),DMCB,=C'SEQ   ',WRKZID,XX,AIO1,ABUFF                 
         CLI   8(R1),0                                                          
         BE    SEQPRT                                                           
         DROP  R4                                                               
*                                                                               
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
*                                                                               
SEQPRT   L     R5,ABUFF1                                                        
         GOTO1 =V(HEXOUT),PARM,(R5),P,40,=C'TOG'                                
         GOTO1 =V(PRINTER)                                                      
         AHI   R5,40                                                            
         GOTO1 =V(HEXOUT),PARM,(R5),P,40,=C'TOG'                                
         GOTO1 =V(PRINTER)                                                      
         AHI   R5,40                                                            
         GOTO1 =V(HEXOUT),PARM,(R5),P,40,=C'TOG'                                
*                                                                               
SEQPRTX  GOTO1 =V(PRINTER)                                                      
         B     NEXT                                                             
         DROP  I                                                                
         EJECT                                                                  
                                                                                
***********************************************************************         
* STA CARD FORMAT STA SPPBNNNNC XXX                                             
*  S=(SYSTEM) PP=(PROGRAM) B=(SUBPROGRAM) NNNN=(FILE REF #) C=(CLASS)           
***********************************************************************         
STA      CLC   CDSYS(10),SPACES                                                 
         BE    STA020                                                           
*                                                                               
         XC    XX,XX               CLEAR USER INDEX                             
I        USING UKRECD,XX                                                        
         MVC   I.UKUSRID,USERID    SET DDS USER ID                              
*                                                                               
         CLC   CDSYS(3),SPACES                                                  
         BE    STA002                                                           
         MVC   I.UKSYSPRG,CDSYS    SET FILE ID                                  
         MVC   I.UKSUBPRG,CDSUB    SET SUB PROGRAM NUMBER                       
*                                                                               
STA002   CLC   CDDAY,SPACES                                                     
         BE    STA010                                                           
         MVC   DUB(2),CDDAY                                                     
         OC    DUB(2),ZEROS                                                     
         PACK  DUB1,DUB(2)                                                      
         SR    R0,R0                                                            
         ICM   R0,3,DUB1+6                                                      
         SRL   R0,4                                                             
         STC   R0,I.UKDAY                                                       
*                                                                               
STA010   CLI   CDCLS,C' '                                                       
         BE    *+10                                                             
         MVC   I.UKCLASS,CDCLS     SET FILE CLASS                               
*                                                                               
         CLC   CDREF,SPACES                                                     
         BE    STA020                                                           
         MVC   DUB(4),CDREF                                                     
         OC    DUB(4),ZEROS                                                     
         PACK  DUB1,DUB(4)                                                      
         CVB   R0,DUB1                                                          
         LTR   R0,R0                                                            
         BZ    STAPRT                                                           
         STCM  R0,15,I.UKFILENO     SET FILE REFERENCE NUMBER                   
*                                                                               
STA020   GOTO1 =V(HEXOUT),PARM,XX,P,L'XX,=C'TOG'                                
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         GOTO1 =V(DATAMGR),DMCB,(X'08',=C'IND'),WRKZID,XX,AIO1,ABUFF            
         CLI   DMCB+8,0                                                         
         BE    STA050                                                           
*                                                                               
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
*                                                                               
STA050   XC    FUL,FUL                                                          
         GOTO1 =V(HEXOUT),PARM,XX,P,32,=C'TOG'                                  
         GOTO1 =V(DATAMGR),DMCB,(X'00',CARD),WRKZID,XX,AIO1,ABUFF               
         CLI   DMCB+8,0                                                         
         BNE   STA030                                                           
         MVC   P+70(30),=CL30'STATUS CHANGED TO XXXXXX'                         
         MVC   P+88(6),CARD                                                     
*                                                                               
STAPRT   GOTO1 =V(PRINTER)                                                      
         B     NEXT                                                             
                                                                                
***********************************************************************         
* WRT CARD FORMAT WRT SPPBNNNNC XXX                                             
*  S=(SYSTEM) PP=(PROGRAM) B=(SUBPROGRAM) NNNN=(FILE REF #) C=(CLASS)           
***********************************************************************         
         USING WLHDRD,R3                                                        
WRT      L     R3,AIO1                                                          
         XC    0(255,R3),0(R3)           BUILD HEADER                           
         XC    XX,XX                                                            
*                                                                               
         MVC   WLSOFLAB,=C'*SOFSOF*'                                            
         MVC   WLUSRID,USERID                                                   
         MVC   WLDESC,=CL16'WRKZ TESTS   '                                      
         MVC   WLSYSPRG,=C'WRK'          SET FILE ID                            
         MVC   WLSUBPRG,=C'F'            SET SUB PROGRAM NUMBER                 
         MVI   WLDAY,17                                                         
         MVC   WLCLASS,=C'T'             SET FILE CLASS                         
*                                                                               
         MVC   WLSYSPRG,CDSYS            SET FILE ID                            
         MVC   WLSUBPRG,CDSUB            SET SUB PROGRAM NUMBER                 
         MVC   WLCLASS,CDCLS             SET FILE CLASS                         
*                                                                               
         CLC   CDDAY,SPACES                                                     
         BE    WRT010                                                           
         MVC   DUB(2),CDDAY                                                     
         OC    DUB(2),ZEROS                                                     
         PACK  DUB1,DUB(2)                                                      
         SR    R0,R0                                                            
         ICM   R0,3,DUB1+6                                                      
         SRL   R0,4                                                             
         STC   R0,WLDAY                                                         
WRT010   MVI   WLTYPE,0                                                         
         MVC   XX,0(R3)                                                         
*                                                                               
         MVC   HALF,=H'1'                THIS # IS # OF RECS ON WRITE           
         CLC   CDREF,SPACES                                                     
         BE    WRT012                                                           
         MVC   DUB(4),CDREF                                                     
         OC    DUB(4),ZEROS                                                     
         PACK  DUB1,DUB(4)                                                      
         CVB   R0,DUB1                                                          
         LTR   R0,R0                                                            
         BZ    WRT012                                                           
         STCM  R0,3,HALF                 THIS # IS # OF RECS ON WRITE           
*                                                                               
WRT012   GOTO1 =V(DATAMGR),DMCB,=C'DMPRINT ',WRKZID,XX,(R3),ABUFF               
         CLI   8(R1),0                                                          
         BNE   WRT030                                                           
*                                                                               
WRT014   XC    0(255,R3),0(R3)           BUILD HEADER                           
         MVC   4(50,R3),=CL50'THIS IS A TEST USING DMWRKZT OFFLINE'             
         MVC   0(2,R3),=AL2(50)                                                 
*                                                                               
         GOTO1 =V(DATAMGR),DMCB,=C'DMPRINT ',WRKZID,XX,(R3),ABUFF               
         CLI   8(R1),0                                                          
         BNE   WRT030                                                           
*                                                                               
         XR    R1,R1                                                            
         ICM   R1,3,HALF                 THIS # IS # OF RECS ON WRITE           
         AHI   R1,-1                                                            
         STCM  R1,3,HALF                                                        
         LTR   R1,R1                                                            
         BP    WRT014                                                           
*                                                                               
         XC    0(255,R3),0(R3)           BUILD HEADER                           
         MVC   WLSOFLAB,=C'*EOFEOF*'                                            
*                                                                               
         GOTO1 =V(DATAMGR),DMCB,=C'DMPRINT ',WRKZID,XX,(R3),ABUFF               
         CLI   8(R1),0                                                          
         BNE   WRT030                                                           
*                                                                               
WRT020   GOTO1 =V(HEXOUT),PARM,XX,P,L'XX,=C'TOG'                                
         GOTO1 =V(PRINTER)                                                      
         B     WRT050                                                           
*                                                                               
WRT030   ST    R1,FUL                    SET EOF/ERR CONDITION                  
         TM    DMCB+8,X'40'                                                     
         BZ    *+6                                                              
         DC    H'0'                      DIE IF DISK ERROR                      
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
*                                                                               
WRT050   XC    FUL,FUL                                                          
         GOTO1 =V(HEXOUT),PARM,XX,P,L'XX,=C'TOG'                                
         MVC   P+70(30),=CL30'WORKER FILE ADDED'                                
*                                                                               
WRTPRT   GOTO1 =V(PRINTER)                                                      
         B     NEXT                                                             
                                                                                
***********************************************************************         
* SCAN INDEX FOR FORMAT INTEGRITY                                               
***********************************************************************         
SCN      CLC   CDSYS(10),SPACES                                                 
         BE    SCN020                                                           
                                                                                
         XC    XX,XX               CLEAR USER INDEX                             
I        USING UKRECD,XX                                                        
         MVC   I.UKUSRID,USERID    SET DDS USER ID                              
                                                                                
         CLC   CDSYS(3),SPACES                                                  
         BE    SCN002                                                           
         MVC   I.UKSYSPRG,CDSYS    SET FILE ID                                  
         MVC   I.UKSUBPRG,CDSUB    SET SUB PROGRAM NUMBER                       
                                                                                
SCN002   CLC   CDDAY,SPACES                                                     
         BE    SCN010                                                           
         MVC   DUB(2),CDDAY                                                     
         OC    DUB(2),ZEROS                                                     
         PACK  DUB1,DUB(2)                                                      
         SR    R0,R0                                                            
         ICM   R0,3,DUB1+6                                                      
         SRL   R0,4                                                             
         STC   R0,I.UKDAY                                                       
                                                                                
SCN010   CLI   CDCLS,C' '                                                       
         BE    *+10                                                             
         MVC   I.UKCLASS,CDCLS     SET FILE CLASS                               
                                                                                
         CLC   CDREF,SPACES                                                     
         BE    SCN020                                                           
         MVC   DUB(4),CDREF                                                     
         OC    DUB(4),ZEROS                                                     
         PACK  DUB1,DUB(4)                                                      
         CVB   R0,DUB1                                                          
         LTR   R0,R0                                                            
         BZ    SCNPRT                                                           
         STCM  R0,15,I.UKFILENO    SET FILE REFERENCE NUMBER                    
                                                                                
SCN020   GOTO1 =V(HEXOUT),PARM,XX,P,L'XX,=C'TOG'                                
         GOTO1 =V(PRINTER)                                                      
                                                                                
         GOTO1 =V(DATAMGR),DMCB,(X'08',=C'IND'),WRKZID,XX,AIO1,ABUFF            
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
                                                                                
***********************************************************************         
* ROUTINE TO TEST DATAMGR RETURN CODE AND OUTPUT MESSAGE                        
***********************************************************************         
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
CHKERR4  L     RF,8(RD)            LOOK FOR DMWRKZ'S WORKING STORAGE            
         CLC   0(4,RF),=C'WRKZ'                                                 
         BE    CHKERR4A                                                         
         L     RF,8(RF)                                                         
         CLC   0(4,RF),=C'WRKZ'                                                 
         BNE   CHKERR5                                                          
CHKERR4A LA    RF,72(RF)                                                        
         CLC   0(8,RF),=C'WFWSINFO'                                             
         BNE   CHKERR5                                                          
         CLC   12(4,RF),=C'WRKZ'   TEST IF USER WRKZ NAME FILLED IN             
         BNE   CHKERR5                                                          
         MVC   CHKERRM4+4(1),16(RF)                                             
         LA    RF,24(RF)           POINT TO REPTINFO                            
         OC    0(2,RF),0(RF)                                                    
         BZ    CHKERR5                                                          
         MVC   CHKERRM5(3),2(RF)   FILE ID                                      
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
         DC    X'80',AL2(130),CL25'WRKZ--EOF'                                   
         DC    X'81',AL2(131),CL25'WRKZ--EOF NO PART#1 CIS'                     
         DC    X'82',AL2(132),CL25'WRKZ--EOF NO PART#2 CIS'                     
         DC    X'84',AL2(133),CL25'WRKZ--FILE TOO BIG'                          
         DC    X'88',AL2(134),CL25'WRKZ--USERID MAX FILES'                      
         DC    X'40',AL2(135),CL25'WRKZ--DISK ERROR'                            
         DC    X'41',AL2(136),CL25'WRKZ--FORMAT ERR ON OPEN'                    
         DC    X'42',AL2(137),CL25'WRKZ--FORMAT ERR ON CLOSE'                   
WFERRSX  DC    X'00',AL2(138),CL25'WRKZ--UNKNOWN ERROR'                         
         EJECT                                                                  
                                                                                
***********************************************************************         
* STANDARD DMWRKZR ROUTINES - MUST START AFTER 4K TO USE R9 BASE                
***********************************************************************         
       ++INCLUDE DMWRKZR                                                        
         EJECT                                                                  
                                                                                
***********************************************************************         
* STORAGE                                                                       
***********************************************************************         
DUB      DS    D                                                                
DUB1     DS    D                                                                
DUB2     DS    D                                                                
FUL      DS    F                                                                
DSKADR   DS    F                                                                
HALF     DS    H                                                                
HALFWORD DS    H                                                                
P1       DS    6F                                                               
DMCB     DS    6F                                                               
DMCB1    DS    6F                                                               
PARM     DS    6F                                                               
WRKZID   DS    CL8                                                              
LASTREPT DC    XL8'00'                                                          
INREPT   DC    XL8'00'                                                          
ZEROS    DC    CL8'00000000'                                                    
                                                                                
ACXREC   DC    A(CXREC)                                                         
ABUFF    DC    A(BUFF)                                                          
ABUFF1   DC    A(BUFF1)                                                         
AIO1     DC    A(IO1)                                                           
AIO2     DC    A(IO2)                                                           
                                                                                
RECNUM   DS    F                                                                
                                                                                
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
PURTABN  DS    F                                                                
PURTABA  DS    A                                                                
                                                                                
OCISZ#1  DS    H                                                                
OCISZ#2  DS    H                                                                
NCISZ#1  DS    H                                                                
NCISZ#2  DS    H                                                                
                                                                                
FRSTCI   DS    F                                                                
LASTCI   DS    F                                                                
LASTTK   DS    F                                                                
NUMTRKS  DS    H                                                                
                                                                                
OPART#2  DS    H                                                                
NPART#2  DS    H                                                                
OTRKSW   DS    H                                                                
NTRKSW   DS    H                                                                
                                                                                
LINES    DS    H                                                                
PAGES    DS    H                                                                
LINNO    DS    PL4                                                              
PAGNO    DS    PL4                                                              
                                                                                
PFLAG    DC    C'Y'                SET BY PRINT=Y/N/E CARD                      
WFLAG    DC    C' '                SET BY WRITE=Y/N CARD                        
UPDATE   DC    C' '                VALUE USED TO VARY ACTION                    
                                                                                
TYPEWF   DC    C'N'                NEW=N 20-BIT OLD=O 16-BIT                    
THISWF   DC    C'O'                SET BY DMWRKZ BUFF CALL                      
WORK     DS    CL19                                                             
                                                                                
CARD     DS    CL80                CURRENT CARD                                 
                                                                                
         DS    0D                                                               
         DC    C'*NDXNDX*'                                                      
XX       DC    XL80'00'            INDEX                                        
                                                                                
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
*                                                                               
         DS    0D                                                               
         DC    C'*KZWKZW*'                                                      
       ++INCLUDE DMWRKZW                                                        
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
                                                                                
         DS    0D                                                               
         DC    C'*PURTAB*'                                                      
PURTAB   DS    1000XL12'00'                                                     
PURTABX  DC    8X'FF'                                                           
                                                                                
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
                                                                                
***********************************************************************         
* CHK CARD FORMAT - CHK     SPPBDDC NNNNN UPDATE=Y                              
*  S=(SYSTEM) PP=(PROGRAM) B=(SUBPROGRAM) DD=(DAY) C=(CLASS) N=(F R #)          
*  NNNNN=(FILE REFERENCE NUMBER)                                                
***********************************************************************         
CHK      NTR1  BASE=*,LABEL=*                                                   
         L     R4,ABUFF            INIT BUFFER - MUST KNOW WRKZID               
         USING W_RECD,R4                                                        
         GOTO1 =V(DATAMGR),DMCB,=C'BUFF',WRKZID,XX,AIO1,ABUFF                   
         LA    RE,12(R4)           RE=A(CIDATA)                                 
         MVC   CIDATA(64),0(RE)    MOVE WRKZ DATA                               
         MVC   CINDXLN,=AL2(L'W_INDEX)                                          
         L     RF,=V(DMENQDEQ)     NEEDED TO CALL LOCKING ROUTINE               
         ST    RF,CIENQDEQ                                                      
         MVI   THISWF,C'N'         NEW STYLE HAS 20-BIT DISK ADDRESS            
         MVC   TYPEWF,THISWF                                                    
                                                                                
CHK001   XC    XX,XX               CLEAR USER INDEX                             
I        USING UKRECD,XX                                                        
         CLC   CDSYS(10),SPACES    TEST IF ANY FILTERS SPECIFIED                
         BE    CHK010                                                           
         MVC   I.UKUSRID,USERID    SET DDS USER ID                              
*                                                                               
CHK002   CLC   CDSYS(3),SPACES     TEST IF SPP SPECIFIED                        
         BE    CHK004                                                           
         MVC   I.UKSYSPRG,CDSYS    SET FILE ID                                  
         MVC   I.UKSUBPRG,CDSUB    SET SUB PROGRAM NUMBER                       
*                                                                               
CHK004   CLC   CDDAY,SPACES        TEST IF DAY SPECIFIED                        
         BE    CHK006                                                           
         MVC   DUB(2),CDDAY                                                     
         OC    DUB(2),ZEROS                                                     
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
CHK008   CLC   CARD+16(5),SPACES   TEST IF FILE NUMBER SPECIFIED                
         BE    CHK010                                                           
         MVC   DUB(5),CARD+16                                                   
         OC    DUB(5),ZEROS                                                     
         PACK  DUB1,DUB(5)                                                      
         CVB   R0,DUB1                                                          
         LTR   R0,R0                                                            
         BZ    *+8                                                              
         STCM  R0,15,I.UKFILENO    SET FILE REFERENCE NUMBER                    
*                                                                               
CHK010   MVC   UPDATE,WFLAG        SET TO VALUE IN WRITE=Y/N CARD               
         CLC   CARD+22(6),=C'WRITE='                                            
         BNE   *+10                                                             
         MVC   UPDATE,CARD+28      CAN BE MODIFIED ON THIS CARD                 
         CLI   UPDATE,C' '                                                      
         BNE   *+8                                                              
         MVI   UPDATE,C'N'         THIS DEFAULTS TO NON UPDATIVE                
                                                                                
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
         L     R0,=A(PURTAB)       CLEAR PURGE CI TABLE                         
         ST    R0,PURTABA                                                       
         XC    PURTABN,PURTABN                                                  
         L     R1,=A(PURTABX)                                                   
         SR    R1,R0                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         MVC   P(30),=CL30'*** STR-OF-PART#1 INDEX ***'                         
         GOTO1 =V(PRINTER)                                                      
                                                                                
CHKLOOP  XC    FUL,FUL                                                          
         GOTO1 =V(DATAMGR),DMCB,(X'08',=C'IND'),WRKZID,XX,AIO1,ABUFF            
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
         DROP  R4                                                               
*                                                                               
         L     R5,ABUFF1           R5=A(FIRST CI REC IN BUFF1)                  
         USING W_RECD,R5                                                        
         GOTO1 =V(DATAMGR),DMCB1,=C'DMREAD',WRKZID,DSKADR,(R5)                  
         CLI   8(R1),0                                                          
         BNE   CHKBAD                                                           
CHKRCI2  SR    RE,RE               RE=NUM OF PART#1 CIS                         
         SR    RF,RF               RF=NUM OF PART#2 CIS                         
         CLI   THISWF,C'N'                                                      
         BO    CHKRCI3                                                          
CHKRCI2A IC    RE,W_NCI            OLD WF VALUES                                
         CHI   RE,1                                                             
         BE    CHKRCI4                                                          
         IC    RF,W_NCIX                                                        
         AR    RF,RE                                                            
         AHI   RF,-1                                                            
         LA    RE,1                                                             
         B     CHKRCI4                                                          
CHKRCI3  IC    RE,W_NCI            NEW WF VALUES                                
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
                                                                                
CHKBAD   L     RF,TTBADS           BUMP BAD FILE COUNTS                         
         AHI   RF,1                                                             
         ST    RF,TTBADS                                                        
         GOTO1 =V(HEXOUT),PARM,XX,P,24,=C'TOG'                                  
         MVC   P+48(5),=C'>NDX3'   WRKZ FILE READ ERROR                         
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
CHKPRT1  GOTO1 =V(DATAMGR),DMCB1,=C'DMREAD',WRKZID,DSKADR,(R5)                  
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
         GOTO1 =V(PRINTER)                                                      
         MVC   P(30),=CL30'*** STR-OF-PART#2 INDEX ***'                         
         GOTO1 =V(PRINTER)                                                      
         BAS   RE,CXLOOPJ          POINT TO START OF PART2 INDEX                
         USING W_RECD,R5           R5=A(WRKZ INDEX ENTRY)                       
         L     R6,=A(CIATAB)                                                    
*                                                                               
CHK2A    BAS   RE,GETXAD                                                        
         GOTO1 =V(DATAMGR),DMCB,(0,=C'DMREAD'),WRKZID,CXADDR,CXREC              
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
         BZ    CHK2E1                                                           
         CLI   W_AGERT,X'FF'                                                    
         BNE   CHK2E1                                                           
         MVC   P+48(5),=C'>NDX6'   ERROR TEMPORARY PART2 NOT IN TAB             
         B     CHK2F                                                            
*                                                                               
CHK2E1   L     RE,PURTABA          TEST IF SPACE IN PURGE CI TABLE              
         CLC   0(4,RE),=4X'FF'                                                  
         BE    CHK2F                                                            
         MVC   0(4,RE),CXADDR      MOVE INDEX DSKADR TO TABLE                   
         MVC   4(2,RE),CXPAGE      MOVE INDEX PAGE/ENTRY TO TABLE               
         MVC   6(2,RE),CXENTRY                                                  
         MVC   8(4,RE),CIADDR      MOVE CI DSKADR TO TABLE                      
         LA    RE,12(RE)                                                        
         ST    RE,PURTABA                                                       
         L     RE,PURTABN          BUMP NUMBER OF PURGE TABLE ENTRYS            
         LA    RE,1(RE)                                                         
         ST    RE,PURTABN                                                       
*                                                                               
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
*                                                                               
CHKPUR   ICM   R7,15,PURTABN       TEST IF ANY ENTRIES IN PURGE TABLE           
         BZ    CHKT                                                             
         CLI   UPDATE,C'Y'         TEST IF WRITE=Y SPECIFIED                    
         BNE   CHKT                                                             
         GOTO1 =V(PRINTER)                                                      
         MVC   P(30),=CL30'*** LOST PART#2 PURGED ***'                          
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         CLI   UPDATE,C'Y'                                                      
         BNE   *+8                                                              
         BAS   RE,W_LOCK           LOCK WRKZ FILE                               
*                                                                               
         BAS   RE,CXLOOPJ          POINT TO START OF PART2 INDEX                
         USING W_RECD,R5           R5=A(WRKZ INDEX ENTRY)                       
         L     R6,=A(PURTAB)       R6=A(PURGE TABLE)                            
*                                                                               
CHKPURA  BAS   RE,GETXAD           GET DSKADR OF NEXT INDEX PAGE                
         CLC   CXADDR,0(R6)                                                     
         BNE   CHKPURN                                                          
         GOTO1 =V(DATAMGR),DMCB,(0,=C'DMREAD'),WRKZID,CXADDR,CXREC              
         CLI   8(R1),0                                                          
         BE    CHKPURB                                                          
         CLI   UPDATE,C'Y'                                                      
         BNE   *+8                                                              
         BAS   RE,W_UNLK           UNLOCK WRKZ FILE                             
         DC    H'0'                                                             
*                                                                               
CHKPURB  CLC   CXADDR,0(R6)        TEST IF INDEX PAGE IN PURGE TABLE            
         BNE   CHKPURN                                                          
         CLC   CXPAGE,4(R6)        TEST IF INDEX ENTRY IN PURGE TABLE           
         BNE   CHKPURN                                                          
         CLC   CXENTRY,6(R6)                                                    
         BNE   CHKPURN                                                          
         MVC   XX,W_INDEX          SAVE INDEX ENTRY                             
         XC    W_INDEX,W_INDEX     PURGE INDEX ENTRY                            
         LA    RF,24                                                            
         GOTO1 =V(HEXOUT),PARM,XX,P,(RF),=C'TOG'                                
         GOTO1 =V(HEXOUT),PARM,8(R6),P+53,4,=C'TOG'                             
         GOTO1 =V(HEXOUT),PARM,0(R6),P+62,4,=C'TOG'                             
         GOTO1 =V(HEXOUT),PARM,4(R6),P+71,4,=C'TOG'                             
         MVI   P+48,C'>'                                                        
         MVC   P+49(3),=C'PUR'                                                  
         LA    R6,12(R6)           BUMP TO NEXT PURGE TABLE ENTRY               
         BCTR  R7,0                                                             
         CLC   CXADDR,0(R6)        TEST IF STILL IN THIS PAGE                   
         BNE   CHKPURC                                                          
         GOTO1 =V(PRINTER)                                                      
         B     CHKPURN                                                          
*                                                                               
CHKPURC  MVI   P+52,C'>'           SET INDEX PAGE WRITTEN FLAG                  
         GOTO1 =V(PRINTER)                                                      
         CLI   UPDATE,C'Y'                                                      
         BNE   CHKPURN                                                          
         GOTO1 =V(DATAMGR),DMCB,(0,=C'DMWRT'),WRKZID,CXADDR,CXREC               
         CLI   8(R1),0                                                          
         BE    CHKPURN                                                          
         BAS   RE,W_UNLK           UNLOCK WRKZ FILE                             
         DC    H'0'                                                             
*                                                                               
CHKPURN  LTR   R7,R7               TEST END OF PURGE TABLE                      
         BZ    CHKPURX                                                          
         BAS   RE,CXLOOPX          BUMP TO NEXT INDEX ENTRY                     
         B     CHKPURB                                                          
         B     CHKPURA             END OF PAGE                                  
*                                                                               
CHKPURX  CLI   UPDATE,C'Y'                                                      
         BNE   CHKT                                                             
         BAS   RE,W_UNLK           UNLOCK WRKZ FILE                             
*                                                                               
CHKT     GOTO1 =V(PRINTER)                                                      
         MVC   P(15),=CL15'WRKZX CI TOTALS'                                     
         MVC   P(5),WRKZID                                                      
         GOTO1 =V(PRINTER)                                                      
         GOTO1 =V(PRINTER)                                                      
         LA    R7,P                                                             
*                                                                               
         MVC   0(7,R7),=C'TOTL#1='                                              
         ICM   R0,15,CICITOT                                                    
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
         EJECT                                                                  
                                                                                
***********************************************************************         
* CARD FORMAT  ANA II JJ - II IS NEW PART#1 SIZE AND JJ IS PART#2 SIZE          
***********************************************************************         
ANA      NTR1  BASE=*,LABEL=*                                                   
         L     R4,ABUFF            INIT BUFFER - MUST KNOW WRKZID               
         USING W_RECD,R4                                                        
         GOTO1 =V(DATAMGR),DMCB,=C'BUFF',WRKZID,XX,AIO1,(R4)                    
         LA    RE,12(R4)           RE=A(CIDATA)                                 
         MVC   CIDATA(64),0(RE)    MOVE WRKZ DATA                               
         MVC   CINDXLN,=H'24'                                                   
         MVI   THISWF,C'N'         NEW STYLE HAS 20-BIT DISK ADDRESS            
         TM    CIDATA+37,X'40'                                                  
         BO    *+8                                                              
         MVI   THISWF,C'O'         OLD STYLE HAS 16-BIT DISK ADDRESS            
         MVC   TYPEWF,THISWF                                                    
*                                                                               
ANA0     MVC   OCISZ#1,CITRKS      SET OLD/NEW PART#1/PART#2 SIZES              
         MVC   NCISZ#1,CITRKS                                                   
         MVC   OCISZ#2,CJTRKS                                                   
         MVC   NCISZ#2,CJTRKS                                                   
         CLC   THISWF,TYPEWF       CAN ONLY ANALIZE MATCHING WF                 
         BNE   ANAERR                                                           
         CLC   CARD+4(2),SPACES    TEST IF CI SIZES INPUT                       
         BE    ANA1                                                             
         MVC   DUB(2),CARD+4                                                    
         OC    DUB(2),ZEROS                                                     
         PACK  DUB1,DUB(2)                                                      
         CVB   R0,DUB1                                                          
         CHI   R0,1                                                             
         BL    ANAERR                                                           
         CHI   R0,5                                                             
         BH    ANAERR                                                           
         STH   R0,NCISZ#1          PART#1 CI SIZE (CITRKS)                      
         MVC   DUB(2),CARD+7                                                    
         OC    DUB(2),ZEROS                                                     
         PACK  DUB1,DUB(2)                                                      
         CVB   R0,DUB1                                                          
         CHI   R0,1                                                             
         BL    ANAERR                                                           
         CHI   R0,20                                                            
         BH    ANAERR                                                           
         STH   R0,NCISZ#2          PART#2 CI SIZE (CJTRKS)                      
*                                                                               
ANA1     CLI   PFLAG,C'Y'          INIT FOR PRINTING                            
         BNE   ANA2                                                             
         MVC   TITLE+31(5),WRKZID                                               
         MVC   MID1(84),ANAMID1                                                 
         MVC   MID2(84),ANAMID2                                                 
         ZAP   LINE,=P'99'                                                      
         GOTO1 =V(PRINTER)                                                      
*                                                                               
ANA2     XC    TTREPS,TTREPS       CLEAR TOTAL COUNTERS                         
         XC    TTSGLS,TTSGLS                                                    
         XC    TTDBLS,TTDBLS                                                    
         XC    TTBADS,TTBADS                                                    
         XC    TTMIXS,TTMIXS                                                    
         XC    TTTRKS,TTTRKS                                                    
         XC    TOPART#2,TOPART#2                                                
         XC    TOTRKSW,TOTRKSW                                                  
         XC    TNPART#2,TNPART#2                                                
         XC    TNTRKSW,TNTRKSW                                                  
         XC    TCIL#1,TCIL#1                                                    
         XC    TCIL#2,TCIL#2                                                    
         XC    TCID#1,TCID#1                                                    
         XC    TCID#2,TCID#2                                                    
         XC    XX,XX               CLEAR USER INDEX                             
*                                                                               
ANALOOP  XC    FUL,FUL                                                          
         GOTO1 =V(DATAMGR),DMCB,(X'08',=C'IND'),WRKZID,XX,AIO1,ABUFF            
         CLI   DMCB+8,0                                                         
         BE    ANARCI                                                           
         ST    R1,FUL              SET EOF/ERR CONDITION                        
         TM    DMCB+8,X'40'                                                     
         BZ    *+6                                                              
         DC    H'0'                DIE IF DISK ERROR                            
         MVC   P,SPACES                                                         
         TM    DMCB+8,X'80'        END OF INDEX                                 
         BO    ANAPRTA                                                          
         TM    DMCB+8,X'10'        REC NOT FOUND                                
         BZ    *+14                                                             
         MVC   P(30),=CL30'*** INDEX REC NOT FOUND ***'                         
         B     ANAPRTA                                                          
         DC    H'0'                                                             
*                                                                               
ANARCI   ICM   R0,7,XX+29          R0=..0TTTTT                                  
         SLL   R0,12                                                            
         ST    R0,DSKADR                                                        
         MVI   DSKADR+3,1          DSKADR=TTTTT001                              
         SRL   R0,12                                                            
         ST    R0,FRSTCI           FRSTCI=000TTTTT                              
         CLI   THISWF,C'N'                                                      
         BE    ANARCI1                                                          
         MVC   DSKADR(2),XX+30                                                  
         MVC   DSKADR+2(2),=X'0100'                                             
         XC    FRSTCI(2),FRSTCI    FRSTCI=0000TTTT                              
         MVC   FRSTCI+2(2),XX+30                                                
         DROP  R4                                                               
*                                                                               
ANARCI1  L     R5,=A(BUFF1)        R5=A(FIRST CI REC IN BUFF1)                  
         USING W_RECD,R5                                                        
         GOTO1 =V(DATAMGR),DMCB1,=C'DMREAD',WRKZID,DSKADR,(R5)                  
         CLI   8(R1),0                                                          
         BNE   ANABAD                                                           
*                                                                               
ANARCI2  SR    RE,RE               RE=NUM OF PART#1 CIS                         
         SR    RF,RF               RF=NUM OF PART#2 CIS                         
         CLI   THISWF,C'N'                                                      
         BE    ANARCI3                                                          
ANARCI2A XC    LASTCI(2),LASTCI    LAST CI=0000TTTT                             
         MVC   LASTCI+2(2),W_TTBR                                               
         XC    LASTTK(2),LASTTK    LAST TRK=0000TTTT                            
         MVC   LASTTK+2(2),W_TTBR                                               
         IC    RE,W_NCI                                                         
         CHI   RE,1                                                             
         BE    ANARCI4                                                          
         IC    RF,W_NCIX                                                        
         AR    RF,RE                                                            
         AHI   RF,-1                                                            
         LA    RE,1                                                             
         B     ANARCI4                                                          
*                                                                               
ANARCI3  ICM   R0,15,W_TTBR        NEW WF VALUES                                
         SRL   R0,12                                                            
         ST    R0,LASTCI           LAST CI=000TTTTT                             
         ICM   R0,15,W_TTBR                                                     
         SRL   R0,12                                                            
         ST    R0,LASTTK           LAST TRK=000TTTTT                            
         IC    RE,W_NCI                                                         
         CHI   RE,1                                                             
         BE    ANARCI4                                                          
         IC    RF,W_NCIX                                                        
         AR    RF,RE                                                            
         AHI   RF,-1                                                            
         LA    RE,1                                                             
*                                                                               
ANARCI4  STH   RF,OPART#2          SAVE NUMBER OF CIS                           
*                                                                               
ANARCI6  LTR   RF,RF               RF=NUM OF PART#2 CIS                         
         BNZ   ANARCI7                                                          
         CLC   FRSTCI,LASTCI       IF ONLY PART#1 CIS CHECK FIRST=LAST          
         BNE   ANABAD                                                           
         L     R0,LASTTK           COMPUTE NUM OF TRACKS                        
         S     R0,LASTCI                                                        
         BM    ANABAD              IGNORE FUNNIES                               
         AHI   R0,1                                                             
         STH   R0,NUMTRKS          TRACKS USED                                  
         LH    R1,OCISZ#1                                                       
         SR    R1,R0                                                            
         STH   R1,OTRKSW           TRACKS WASTED                                
         B     ANARCI8                                                          
*                                                                               
ANARCI7  LR    R0,RE               RE=NUM OF PART#1 CIS                         
         MH    R0,OCISZ#1                                                       
         ST    R0,FUL                                                           
         LR    R0,RF               RF=NUM OF PART#2 CIS                         
         AHI   R0,-1                                                            
         BM    ANABAD              IGNORE FUNNIES                               
         MH    R0,OCISZ#2                                                       
         A     R0,FUL                                                           
         ST    R0,FUL                                                           
         L     R0,LASTTK           COMPUTE NUM TRACKS IN LAST CI                
         S     R0,LASTCI                                                        
         BM    ANALOOP             IGNORE FUNNIES                               
         AHI   R0,1                                                             
         LH    R1,OCISZ#2                                                       
         SR    R1,R0                                                            
         STH   R1,OTRKSW           TRACKS WASTED                                
         A     R0,FUL                                                           
         STH   R0,NUMTRKS          TRACKS USED                                  
*                                                                               
ANARCI8  CLC   NUMTRKS,NCISZ#1     TEST IF NEEDS PART#2 CIS                     
         BH    ANARCI9                                                          
         LH    R0,NCISZ#1                                                       
         SH    R0,NUMTRKS                                                       
         STH   R0,NTRKSW                                                        
         MVC   NPART#2,=H'0'                                                    
         B     ANATOT                                                           
*                                                                               
ANARCI9  LH    R0,NUMTRKS          COMPUTE NEW PART#2 CIS                       
         SH    R0,NCISZ#1                                                       
         SRDL  R0,32                                                            
         LH    RF,NCISZ#2                                                       
         DR    R0,RF                                                            
         STH   R1,NPART#2                                                       
         MVC   NTRKSW,=H'0'                                                     
         LTR   R0,R0               TEST EXACT NUMBER OF PART#2S                 
         BZ    ANATOT                                                           
         AHI   R1,1                NEED ONE MORE PART#2                         
         STH   R1,NPART#2                                                       
         LH    R1,NCISZ#2          COMPUTE NEW TRACKS WASTED                    
         SR    R1,R0                                                            
         STH   R1,NTRKSW                                                        
         B     ANATOT                                                           
*                                                                               
ANABAD   L     R0,TTREPS           BUMP TOTAL FILES                             
         AHI   R0,1                                                             
         ST    R0,TTREPS                                                        
         L     R0,TTBADS           BUMP BAD FILES                               
         AHI   R0,1                                                             
         ST    R0,TTBADS                                                        
         B     ANALOOP                                                          
*                                                                               
ANATOT   L     R0,TTREPS           BUMP TOTAL FILES                             
         AHI   R0,1                                                             
         ST    R0,TTREPS                                                        
         CLC   NUMTRKS,=H'1'                                                    
         BNE   *+16                                                             
         L     R1,TTSGLS           BUMP TOTAL SINGLE TRACK REPS                 
         AHI   R1,1                                                             
         ST    R1,TTSGLS                                                        
         CLC   NUMTRKS,=H'2'                                                    
         BNE   *+16                                                             
         L     R1,TTDBLS           BUMP TOTAL DOUBLE TRACK REPS                 
         AHI   R1,1                                                             
         ST    R1,TTDBLS                                                        
         L     R0,TTTRKS           BUMP TOTAL TRACKS USED                       
         AH    R0,NUMTRKS                                                       
         ST    R0,TTTRKS                                                        
*                                                                               
ANATOT1  L     R0,TOTRKSW          BUMP TOTAL OLD TRACKS WASTED                 
         AH    R0,OTRKSW                                                        
         ST    R0,TOTRKSW                                                       
         L     R0,TNTRKSW          BUMP TOTAL NEW TRACKS WASTED                 
         AH    R0,NTRKSW                                                        
         ST    R0,TNTRKSW                                                       
         L     R0,TOPART#2         BUMP TOTAL OLD PART#2 CI COUNT               
         AH    R0,OPART#2                                                       
         ST    R0,TOPART#2                                                      
         L     R0,TNPART#2         BUMP TOTAL NEW PART#2 CI COUNT               
         AH    R0,NPART#2                                                       
         ST    R0,TNPART#2                                                      
*                                                                               
ANATOT2  TM    W_STAT,W_STLIVE     TEST LIVE FILE                               
         BZ    ANATOT3                                                          
         L     R0,TCIL#1           BUMP LIVE PART#1 CI COUNT                    
         AHI   R0,1                                                             
         ST    R0,TCIL#1                                                        
         L     R0,TCIL#2           BUMP LIVE PART#2 CI COUNT                    
         AH    R0,OPART#2                                                       
         ST    R0,TCIL#2                                                        
         B     ANAPRT                                                           
ANATOT3  L     R0,TCID#1           BUMP DEAD PART#1 CI COUNT                    
         AHI   R0,1                                                             
         ST    R0,TCID#1                                                        
         L     R0,TCID#2           BUMP DEAD PART#2 CI COUNT                    
         AH    R0,OPART#2                                                       
         ST    R0,TCID#2                                                        
*                                                                               
ANAPRT   CLI   PFLAG,C'Y'          TEST IF PRINTING DETAIL                      
         BNE   ANAPRT4                                                          
         GOTO1 =V(HEXOUT),PARM,XX+00,P+00,8,=C'TOG'                             
         GOTO1 =V(HEXOUT),PARM,XX+29,P+17,3,=C'TOG'                             
*                                                                               
         MVC   P+24(3),W_SYSPRG    FILE SYS/PRG                                 
         MVI   P+27,C','                                                        
         ICM   R0,15,W_FILENO      FILE NUMBER                                  
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+28(5),DUB                                                      
         MVI   P+34,C'L'           FILE STATUS                                  
         TM    W_STAT,W_STLIVE                                                  
         BNZ   *+8                                                              
         MVI   P+34,C'D'                                                        
*                                  EXCEPTION FLAGS ALREADY IN P+35              
         LA    R7,P+36                                                          
         EDIT  (B2,NUMTRKS),(5,0(R7)),ZERO=NOBLANK                              
         LA    R7,6(R7)                                                         
         EDIT  (B2,OPART#2),(5,0(R7)),ZERO=NOBLANK                              
         LA    R7,6(R7)                                                         
         EDIT  (B2,OTRKSW),(5,0(R7)),ZERO=NOBLANK                               
         LA    R7,6(R7)                                                         
         EDIT  (B2,NPART#2),(5,0(R7)),ZERO=NOBLANK                              
         LA    R7,6(R7)                                                         
         EDIT  (B2,NTRKSW),(5,0(R7)),ZERO=NOBLANK                               
         LA    R7,6(R7)                                                         
*                                                                               
ANAPRT1  CLI   THISWF,C'O'         OUTPUT FILE RECORDS/AVGRL                    
         BE    ANAPRT2                                                          
         EDIT  (B4,W_RECS),(5,0(R7)),ZERO=NOBLANK                               
         LA    R7,6(R7)                                                         
         EDIT  (B2,W_AVGRL),(5,0(R7)),ZERO=NOBLANK                              
         LA    R7,6(R7)                                                         
         B     ANAPRT3                                                          
*                                                                               
ANAPRT2  EDIT  (B4,W_RECS),(5,0(R7)),ZERO=NOBLANK                               
         LA    R7,6(R7)                                                         
         EDIT  (B2,W_AVGRL),(5,0(R7)),ZERO=NOBLANK                              
         LA    R7,6(R7)                                                         
         B     ANAPRT3                                                          
*                                                                               
ANAPRT3  GOTO1 =V(PRINTER)         PRINT FILE DETAIL LINE                       
*                                                                               
ANAPRT4  OC    FUL,FUL                                                          
         BZ    ANALOOP                                                          
*                                                                               
ANAPRTA  MVC   MID1(84),ANAEND1    PRINT TOTAL LINE                             
         MVC   MID2(84),ANAEND2                                                 
         ZAP   LINE,=P'99'                                                      
*                                                                               
         EDIT  (B4,TTREPS),(5,P+00),ZERO=NOBLANK                                
         EDIT  (B4,TTSGLS),(5,P+06),ZERO=NOBLANK                                
         EDIT  (B4,TTDBLS),(5,P+12),ZERO=NOBLANK                                
         EDIT  (B4,TTBADS),(5,P+18),ZERO=NOBLANK                                
         EDIT  (B4,TTMIXS),(5,P+24),ZERO=NOBLANK                                
*                                                                               
         LA    R7,P+36                                                          
         EDIT  (B4,TTTRKS),(5,0(R7)),ZERO=NOBLANK                               
         LA    R7,6(R7)                                                         
         EDIT  (B4,TOPART#2),(5,0(R7)),ZERO=NOBLANK                             
         LA    R7,6(R7)                                                         
         EDIT  (B4,TOTRKSW),(5,0(R7)),ZERO=NOBLANK                              
         LA    R7,6(R7)                                                         
         EDIT  (B4,TNPART#2),(5,0(R7)),ZERO=NOBLANK                             
         LA    R7,6(R7)                                                         
         EDIT  (B4,TNTRKSW),(5,0(R7)),ZERO=NOBLANK                              
         GOTO1 =V(PRINTER)                                                      
         MVC   P(85),ANAEND3                                                    
         GOTO1 =V(PRINTER)                                                      
ANAPRTX  OC    FUL,FUL                                                          
         BZ    ANALOOP                                                          
*                                                                               
ANAT     GOTO1 =V(PRINTER)                                                      
         MVC   P(35),=CL35'WRKZX CI TOTALS OLD=NN/NN NEW=NN/NN'                 
         MVC   P(5),WRKZID                                                      
         LH    R0,OCISZ#1                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+20(2),DUB                                                      
         LH    R0,OCISZ#2                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+23(2),DUB                                                      
         LH    R0,NCISZ#1                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+30(2),DUB                                                      
         LH    R0,NCISZ#2                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+33(2),DUB                                                      
         GOTO1 =V(PRINTER)                                                      
         GOTO1 =V(PRINTER)                                                      
         LA    R7,P                                                             
*                                                                               
         MVC   0(7,R7),=C'TOTL#1='                                              
         ICM   R0,15,CICITOT                                                    
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
ANATX    GOTO1 =V(PRINTER)                                                      
         B     ANAX                                                             
*                                                                               
ANAERR   MVC   P(24),=CL24'INVALID ANALYZE REQUEST'                             
         GOTO1 =V(PRINTER)                                                      
*                                                                               
ANAX     MVC   MID1,SPACES                                                      
         MVC   MID2,SPACES                                                      
         MVC   TITLE+31(5),SPACES                                               
         XIT1                                                                   
         DROP  R5                                                               
         LTORG                                                                  
ANAMID1  DC    CL36'FILE KEY         0TTTTT FILE ID   S '                       
         DC    CL48'#TRKS   OLD   OLD   NEW   NEW RECS  AVGRL       '           
ANAMID2  DC    CL36'---------------- ------ --------- - '                       
         DC    CL48'----- #2CIS WASTE #2CIS WASTE ----- -----       '           
ANAEND1  DC    CL36'TOTAL 1-TRK 2-TRK  BAD   MIX        '                       
         DC    CL48'#TRKS   OLD   OLD   NEW   NEW       '                       
ANAEND2  DC    CL36'----- ----- ----- ----- ----- ----- '                       
         DC    CL48'----- #2CIS WASTE #2CIS WASTE'                              
ANAEND3  DC    CL36'----- ----- ----- ----- ----- ----- '                       
         DC    CL48'----- ----- ----- ----- -----'                              
                                                                                
         DS    0D                                                               
                                                                                
***********************************************************************         
* CARD AND DSECTS                                                               
***********************************************************************         
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
                                                                                
*DMWRKZX                                                                        
       ++INCLUDE DMWRKZX                                                        
                                                                                
*DMWRKZD                                                                        
       ++INCLUDE DMWRKZD                                                        
         EJECT                                                                  
*DMWRKZK                                                                        
       ++INCLUDE DMWRKZK                                                        
         EJECT                                                                  
*DMWRKZL                                                                        
       ++INCLUDE DMWRKZL                                                        
         EJECT                                                                  
*DMWRKZS                                                                        
       ++INCLUDE DMWRKZS                                                        
         EJECT                                                                  
*DDDPRINTL                                                                      
       ++INCLUDE DDDPRINTL                                                      
         EJECT                                                                  
SSOD     DSECT                                                                  
*FASSBOFF                                                                       
       ++INCLUDE FASSBOFF                                                       
                                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'001DMWRKZT   03/22/11'                                      
         END                                                                    
