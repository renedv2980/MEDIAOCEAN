*          DATA SET PPINF15    AT LEVEL 012 AS OF 03/16/05                      
*PHASE T41A15A                                                                  
         TITLE 'T41A15   PRINTPAK  INFO  JOB RECS'                              
*                                                                               
*  CHANGE LOG                                                                   
*                                                                               
*  SMYE  03/05     ADD AD ID TO JOBREC (ADREC) DISPLAY                          
*                                                                               
*  KWAN  05/99     CORRECT FILTER ERROR DISPLAY                                 
*                                                                               
*  SMYE  12/20/95  CHANGED VDTCNV TO VDATCON WITH NEW PARAM'S                   
*                                                                               
T41A15   CSECT                                                                  
         NMOD1 0,T41A15                                                         
         L     RC,0(R1)                                                         
         USING GENOLD,RC                                                        
         L     RA,4(R1)                                                         
         USING T41AFFD,RA                                                       
         USING FLDHDRD,R2                                                       
         LA    R5,REC              SET RECORD ADDRESS                           
         ST    R5,AREC                                                          
*                                                                               
         MVI   FLTSW,0             INITIALIZE FILTER SWITCH                     
*                                                                               
SETFILT  DS    0H                                                               
         MVI   INACTSW,0                                                        
         GOTO1 GETFLTR,DUB,(64,SINIFLT),(9,=C'INACTIVE=')                       
         OC    4(4,R1),4(R1)                                                    
         BZ    SETSCRN                                                          
         L     R4,4(R1)                                                         
         LA    R4,9(R4)                                                         
         XC    WORK,WORK                                                        
         MVC   WORK(9),=C'YES OR NO'                                            
         LA    RE,9                                                             
         LR    R6,R4                                                            
         CLI   0(R4),C'Y'                                                       
         BE    SETF5                                                            
         CLI   0(R4),C'N'                                                       
         BE    SETF5                                                            
         B     FLTERR                                                           
*                                                                               
SETF5    MVC   INACTSW,0(R4)                                                    
*                                                                               
         MVI   FLTSW,1             CONTAIN A VALID FIELD                        
*                                                                               
SETSCRN  DS    0H                                                               
*                                                                               
*                                                                               
*                                                                               
         CLI   SINIFLTH+5,0        ANY BAD INPUTS?                              
         BE    SETSCR30                                                         
         CLI   FLTSW,1                                                          
         BE    SETSCR30                                                         
         LA    R2,SINIFLTH         POINT TO FILTER FIELD                        
         LA    R3,2                FIELD INVALID ERR MSG                        
         B     ERROR                                                            
*                                                                               
*                                                                               
*                                                                               
SETSCR30 LA    R4,SINHDR                                                        
         USING JOBSCRND,R4                                                      
         LA    R2,SINHDRH                                                       
         MVC   JSPRD,=C'PRD'                                                    
         MVC   JSJOB,=C'AD NO.'                                                 
         MVC   JSADID(5),=C'Ad-ID'                                              
         MVC   JSCAP(7),=C'CAPTION'                                             
         MVC   JSCOPY(11),=C'COPY NUMBER'                                       
         MVC   JSDATES(15),=C'START-END DATES'                                  
         FOUT  (R2)                                                             
         LA    R2,LINLEN(R2)                                                    
         LA    R4,LINLEN(R4)                                                    
         MVC   JSPRD,DASH                                                       
         MVC   JSJOB,DASH                                                       
         MVC   JSADID(5),DASH                                                   
         MVC   JSCAP(7),DASH                                                    
         MVC   JSCOPY(11),DASH                                                  
         MVC   JSDATES(15),DASH                                                 
         FOUT  (R2)                                                             
         LA    R2,LINLEN(R2)                                                    
         LA    R2,LINLEN(R2)                                                    
         LA    R4,LINLEN(R4)                                                    
         LA    R4,LINLEN(R4)                                                    
         EJECT                                                                  
         LA    R9,14                                                            
         XC    KEY,KEY                                                          
         LA    R5,KEY                                                           
         USING JOBRECD,R5                                                       
         MVC   KEY,SVKEY                                                        
         CLC   SVPRD,=C'ALL'                                                    
         BE    *+10                                                             
         MVC   PJOBKPRD,SVPRD                                                   
         OC    PREVKEY,PREVKEY                                                  
         BZ    GBHIGH                                                           
         MVC   KEY,PREVKEY                                                      
         XC    PREVKEY,PREVKEY                                                  
GBHIGH   BAS   RE,HIGH                                                          
         B     HAVREC                                                           
*                                                                               
GBSEQ    BAS   RE,SEQ                                                           
HAVREC   LA    R5,KEY                                                           
         USING JOBRECD,R5                                                       
         CLC   KEY(7),KEYSAVE                                                   
         BNE   BEND                                                             
         CLC   SVPRD,=C'ALL'    CHECK PRODUCT                                   
         BE    PRDOK                                                            
         CLC   PJOBKPRD,SVPRD                                                   
         BNE   BEND                                                             
PRDOK    DS    0H                                                               
         OC    PJOBKJOB+6(6),PJOBKJOB+6                                         
         BNZ   GBSEQ               BYPASS INS RECS                              
         L     R5,AREC                                                          
         BAS   RE,GETREC              GET A RECORD                              
ACTCHK   DS    0H                  SEE IF FILTERING INACTIVE JOBS               
         CLI   INACTSW,C'Y'       INACTIVE ONLY                                 
         BNE   ACTCHK5                                                          
         CLC   =C'INACTIVE',PJOBCAP1                                            
         BE    JOBOK                                                            
         B     GBSEQ                                                            
*                                                                               
ACTCHK5  CLI   INACTSW,C'N'        NO INACTIVE JOBS                             
         BNE   JOBOK                                                            
         CLC   =C'INACTIVE',PJOBCAP1                                            
         BE    GBSEQ               BYPASS INACTIVES                             
         B     JOBOK                                                            
         EJECT                                                                  
* END OF FILTERS SO DISPLAY RECORD                                              
JOBOK    MVC   JSPRD,PJOBKPRD                                                   
         MVC   JSJOB,PJOBKJOB                                                   
         MVC   JSADID,PJOBADID                                                  
         MVC   JSCAP,PJOBCAP1                                                   
         MVC   JSCOPY,PJOBCPY                                                   
         OC    PJOBSTA(6),PJOBSTA     SEE IF DATES THERE                        
         BZ    JOBOK10                                                          
         GOTO1 VDATCON,DMCB,(3,PJOBSTA),(5,JSDATES)                             
         MVI   JSDATES+8,C'-'                                                   
         GOTO1 VDATCON,DMCB,(3,PJOBEND),(5,JSDATES+9)                           
*                                                                               
JOBOK10  FOUT  (R2)                                                             
         LA    R2,LINLEN(R2)                                                    
         LA    R4,LINLEN(R4)                                                    
         BCT   R9,GBSEQ                                                         
         BAS   RE,SEQ                                                           
         CLC   KEY(4),KEYSAVE                                                   
         BNE   MODEXIT                                                          
         MVC   PREVKEY,KEY                                                      
         B     MODEXIT                                                          
*                                                                               
BEND     DS    0H                                                               
         B     MODEXIT                                                          
         EJECT                                                                  
FLTERR   BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   SINMSG(0),0(R6)                                                  
         LA    RE,3(RE)                                                         
         LA    RF,SINMSG(RE)                                                    
         MVC   0(22,RF),=C'- INVALID FILTER FIELD'                              
         LA    RF,23(RF)                                                        
         MVC   0(20,RF),WORK                                                    
         LA    R2,SINIFLTH                                                      
         MVI   ERRCD,X'FF'                                                      
         MVI   ERRAREA,X'FF'                                                    
         B     MODEXIT2                                                         
*                                                                               
GETNUM   LA    R5,0                                                             
GN1      CLI   0(R4),C'-'                                                       
         BER   R9                                                               
         CLI   0(R4),C','                                                       
         BER   R9                                                               
         CLI   0(R4),X'00'                                                      
         BER   R9                                                               
         CLI   0(R4),C' '                                                       
         BER   R9                                                               
         CLI   0(R4),C'0'                                                       
         BL    GNERR                                                            
         CLI   0(R4),C'9'                                                       
         BH    GNERR                                                            
         LA    R4,1(R4)                                                         
         LA    R5,1(R5)                                                         
         B     GN1                                                              
GNERR    SR    R5,R5                                                            
         BR    R9                                                               
         EJECT                                                                  
MODEXIT  LA    R2,SINIKEYH                                                      
         OC    PREVKEY,PREVKEY                                                  
         BZ    *+8                                                              
         LA    R2,SINENDH                                                       
MODEXIT2 OI    6(R2),X'C0'                                                      
         XMOD1 1                                                                
       ++INCLUDE PPGENEROL                                                      
DASH     DC    40C'-'                                                           
HIGHK    DC    10X'FF'                                                          
INACTSW  DC    X'00'                                                            
*                                                                               
FLTSW    DC    X'00'                                                            
*                                                                               
LINLEN   EQU   88                                                               
         LTORG                                                                  
JOBSCRND DSECT                                                                  
JSPRD    DS    CL3                                                              
         DS    CL1                                                              
JSJOB    DS    CL6                                                              
         DS    CL1                                                              
JSADID   DS    CL12                                                             
         DS    CL1                                                              
JSCAP    DS    CL19                                                             
         DS    CL1                                                              
JSCOPY   DS    CL17                                                             
         DS    CL1                                                              
JSDATES  DS    CL17                                                             
         EJECT                                                                  
JOBRECD DSECT                                                                   
       ++INCLUDE PJOBREC                                                        
*                                                                               
       ++INCLUDE PPSINFOWRK                                                     
