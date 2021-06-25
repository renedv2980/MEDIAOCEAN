*          DATA SET DDSTAVAL   AT LEVEL 024 AS OF 04/20/16                      
*PHASE T00A68B   <=====                                                         
         TITLE 'DDSTAVAL - VALIDATES STATION'                                   
*                                                                               
*============================================================                   
* INPUT/OUTPUT - STABLOCK                                                       
*============================================================                   
* 14FEB05 MHER  PICK UP SOFT LEN OF CABLETAB                                    
* 06AUG03 MHER  PICK UP COMFACS TO GET A(CABLETAB)                              
*============================================================                   
         PRINT NOGEN                                                            
STAVAL   CSECT                                                                  
         NMOD1 STAX-STAD,**STAV**                                               
         USING STAD,RC                                                          
         ST    R1,SAVER1                                                        
         L     R4,0(R1)            A(INPUT BLOCK)                               
         USING STABLKD,R4                                                       
         MVC   STBSTA,SPACES       CLEAR STATION/NETWORK OUTPUT                 
         MVC   STBNET,SPACES                                                    
         LA    R2,VMED                                                          
*                                                                               
SV10     CLI   0(R2),X'FF'         END OF VALID MEDIA LIST                      
         BE    INVMED                                                           
         CLC   0(1,R2),STBMED      VALID MEDIA                                  
         BE    SV20                                                             
         LA    R2,1(R2)                                                         
         B     SV10                                                             
*                                                                               
SV20     DS    0H                                                               
         L     R2,STBADDR          A(FLDHDR/STRING)                             
         LA    R2,0(R2)            CLEAR HOB                                    
         MVC   MYBYTE,STBADDR                                                   
         TM    MYBYTE,X'80'        TEST STRING                                  
         BO    *+8 YES                                                          
         LA    R2,8(R2)            FLDHDR - BUMP TO START OF INPUT              
*                                                                               
         CLC   =C'ALL',0(R2)       IF REQUESTING 'ALL' SPECIAL CHECKS           
         BE    SALL                                                             
         CLI   0(R2),C'0'          IF 1ST CHAR IS A DIGIT                       
         BL    SV30                                                             
         CLI   STBMED,C'T'         MEDIA S/B TV                                 
         BNE   INVSTA                                                           
         B     CABLE               AND IT IS A CABLE STATION                    
*                                                                               
SV30     MVI   STOPCHAR,0          CLEAR STOPCHAR                               
         LR    R1,R2               MOVE STATION TO OUTPUT AREA                  
         LA    R3,4                MOVE UP TO 1ST 4 CHARS                       
         LA    R5,STBOUT                                                        
*                                                                               
SV40     CLI   0(R1),C' '          END OF INPUT                                 
         BNH   SV50                                                             
         CLI   0(R1),C'-'                                                       
         BE    SV50                                                             
         CLI   0(R1),C'/'                                                       
         BE    SV50                                                             
         TM    MYBYTE,X'80'        IF THIS IS A STRING                          
         BNO   SV45                                                             
         CLI   0(R1),C','          END OF INPUT CAN BE ','                      
         BE    SV52                                                             
*                                                                               
SV45     MVC   0(1,R5),0(R1)                                                    
         LA    R1,1(R1)                                                         
         LA    R5,1(R5)                                                         
         BCT   R3,SV40                                                          
*                                                                               
         CLI   0(R1),C' '          IF WE'RE AT A BLANK OR '-'                   
         BNH   SV50                                                             
         CLI   0(R1),C'/'                                                       
         BE    SV50                                                             
         CLI   0(R1),C'-'                                                       
         BNE   SV52                                                             
*                                                                               
SV50     MVC   STOPCHAR,0(R1)      SAVE THE STOP CHAR                           
         LA    R1,1(R1)            SKIP TO NEXT CHAR                            
*                                                                               
SV52     CLI   STBMED,C'R'         IF THIS IS RADIO - GO CHECK BAND             
         BE    SV70                                                             
         CLI   STOPCHAR,C' '       IF THERE IS A STOP CHAR                      
         BH    SV54                THERE SHOULD BE MORE INPUT                   
*                                                                               
         CLI   0(R1),C' '          IS THERE ANYTHING ELSE INPUT                 
         BNH   SV100               NO - DONE                                    
         TM    MYBYTE,X'80'        IF THIS IS A STRING                          
         BNO   SV54                                                             
         CLI   0(R1),C','          END OF INPUT CAN BE ','                      
         BE    SV100                                                            
*                                                                               
SV54     CLI   STBMED,C'N'         IF THIS IS NETWORK                           
         BNE   SV60                                                             
         CLI   STBCTRY,C'C'        AND THIS IS CANADA                           
         BNE   SV60                                                             
*                                                                               
         CLI   STOPCHAR,C'/'                                                    
         BE    SV55                                                             
         CLI   0(R1),C'N'          IS IT THE MEDIA CODE ?                       
         BNE   SV55                                                             
         CLI   1(R1),C' '          TEST NO MORE INPUT                           
         BNH   SV100                                                            
         TM    MYBYTE,X'80'        IF THIS IS A STRING                          
         BNO   INVSTA                                                           
         CLI   1(R1),C','          END OF INPUT CAN BE ','                      
         BE    SV100                                                            
         B     INVSTA                                                           
*                                                                               
SV55     LA    R5,STBNET                                                        
         LHI   R0,1                REGION SHOULD BE 1 CHAR                      
*                                                                               
         CLI   STOPCHAR,C'-'                                                    
         BE    SV56                                                             
         LHI   R0,2                                                             
         CLI   STOPCHAR,C'/'       CBLNET SHOULD BE 2 CHAR                      
         BE    SV56                                                             
         B     INVSTA              ELSE INPUT IS INVALID                        
*                                                                               
SV56     TM    MYBYTE,X'80'        IF THIS IS A STRING                          
         BNO   SV58                                                             
         CLI   0(R1),C','          END OF INPUT CAN BE ','                      
         BE    SV100                                                            
*                                                                               
SV58     CLI   0(R1),C' '          IS THERE ANYTHING ELSE INPUT                 
         BNH   INVSTA                                                           
         MVC   0(1,R5),0(R1)       MOVE INPUT CHAR                              
         LA    R1,1(R1)                                                         
         LA    R5,1(R5)                                                         
         BCT   R0,SV58                                                          
         CLI   0(R1),C' '          TEST FOR MORE INPUT                          
         BNH   SV100               NO                                           
         TM    MYBYTE,X'80'                                                     
         BNO   INVSTA                                                           
         CLI   0(R1),C','                                                       
         BE    SV100                                                            
         B     INVSTA                                                           
*                                                                               
SV60     CLC   STBMED,0(R1)        ELSE IT MUST MATCH THE MEDIA CODE            
         BE    SV62                                                             
* ALLOW L FOR LOW FREQ TV/ S FOR STREAMING                                      
         CLI   STBMED,C'T'                                                      
         BNE   INVSTA                                                           
         CLI   0(R1),C'L'                                                       
         BE    SV90                GO SAVE LAST CHAR                            
         CLI   0(R1),C'D'          FOR DIGITAL                                  
         BE    SV90                                                             
         B     INVSTA                                                           
*                                                                               
SV62     CLI   1(R1),C' '                                                       
         BNH   SV100                                                            
         TM    MYBYTE,X'80'        IF THIS IS A STRING                          
         BNO   SV65                                                             
         CLI   1(R1),C','          END OF INPUT CAN BE ','                      
         BE    SV100                                                            
*                                                                               
SV65     CLI   STBMED,C'T'         AND ONLY OTHER INPUT CAN BE                  
         BNE   INVSTA                                                           
         CLI   1(R1),C'V'          FOR TV                                       
         BNE   INVSTA                                                           
         B     SV100                                                            
*                                                                               
SV70     CLI   0(R1),C'A'          RADIO MUST HAVE AM/FM                        
         BE    SV80                                                             
         CLI   0(R1),C'F'                                                       
         BE    SV80                                                             
         CLI   0(R1),C'S'          S FOR STREAMING MEDIA                        
         BE    SV80                                                             
         CLI   0(R1),C'C'          C FOR IHEARTRADIO                            
         BNE   INVSTA                                                           
*                                                                               
SV80     CLI   1(R1),C' '          MUST HAVE A/F/S/C OR AM/FM/SM/CM             
         BNH   SV90                                                             
         TM    MYBYTE,X'80'        IF THIS IS A STRING                          
         BNO   SV85                                                             
         CLI   1(R1),C','          END OF INPUT CAN BE ','                      
         BE    SV90                                                             
*                                                                               
SV85     CLI   1(R1),C'M'                                                       
         BNE   INVSTA                                                           
*                                                                               
SV90     MVC   STBSTA+4(1),0(R1)   IF RADIO - BAND = LAST CHAR OF STA           
         B     *+10                                                             
*                                                                               
SV100    MVC   STBSTA+4(1),STBMED  ELSE MEDIA = LAST CHAR OF STATION            
*                                                                               
SVX      TM    MYBYTE,X'80'        TEST STRING INPUT                            
         BZ    SVXIT                                                            
         L     R1,STBSTRAD         GET START OF NETWORK ADDR                    
         LA    R1,2(R1)            POINT TO LAST CHAR                           
         CLI   0(R1),C'A'          TEST DATA PRESENT                            
         BNH   *+8                 NO                                           
         LA    R1,1(R1)            ELSE POINT TO NEXT CHAR                      
         LA    R1,1(R1)            NOW POINT BEYOND LAST CHAAR                  
         ST    R1,STBSTRAD         AND SET ADDRESS                              
         SR    R1,R2               GET LENGTH OF STRING                         
         STC   R1,STBSTRLN         AND RETURN TO USER                           
*                                                                               
SVXIT    B     XIT                                                              
         EJECT                                                                  
*                                                                               
*        SPECIAL CHECKS FOR 'ALL' STATIONS                                      
*        IF CABLE - RETURN 'ALL /' OR 'ALL -', GO CHECK & RETURN NTWK           
*        ELSE     - RETURN 'ALL'                                                
*                                                                               
SALL     CLI   STBMED,C'T'         MEDIA S/B TV                                 
         BNE   INVSTA                                                           
         MVC   STBSTA(3),0(R2)     PUT 'ALL' IN STBSTA                          
         LA    R1,3(R2)            BUMP PAST 'ALL' INPUT                        
         CLI   0(R1),C'/'                                                       
         BE    SA10                                                             
         CLI   0(R1),C'-'                                                       
         BNE   SAX                                                              
*                                                                               
SA10     CLI   STBMED,C'T'         MEDIA S/B TV                                 
         BNE   INVSTA                                                           
         MVC   STBSTA+4(1),0(R1)                                                
*                                                                               
         LA    R1,3(R2)            BUMP PAST 'ALL' INPUT                        
         LA    R1,1(R1)            POINT TO THE NETWORK                         
         B     CBL35               GO CHECK CABLE NETWORK                       
*                                                                               
SAX      B     SVX                 GO SET LENGTH OF INPUT                       
         EJECT                                                                  
*==========================================================                     
*        THIS IS A CABLE STATION                                                
*        IF 1ST CHAR IS A DIGIT - ALL HAVE TO BE DIGITS                         
*==========================================================                     
*                                                                               
*                                                                               
CABLE    SR    R3,R3                                                            
         LA    R6,4                MAXIMUM OF 4 DIGITS                          
         LR    R1,R2                                                            
         LA    R5,STBSTA                                                        
         MVC   STBSTA(4),=C'0000'                                               
*                                                                               
CBL10    CLI   0(R1),C' '          END OF STRING                                
         BNH   CBL30                                                            
         CLI   0(R1),C'/'                                                       
         BE    CBL30                                                            
         TM    MYBYTE,X'80'        IF THIS IS A STRING                          
         BNO   CBL20                                                            
         CLI   0(R1),C','          END OF INPUT CAN BE ','                      
         BE    CBL30                                                            
*                                                                               
CBL20    LA    R1,1(R1)                                                         
         LA    R3,1(R3)                                                         
         BCT   R6,CBL10                                                         
*                                                                               
CBL30    BCTR  R3,0                                                             
         MVC   WORK(4),=4X'F0'                                                  
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVZ   WORK(0),0(R2)                                                    
         CLC   WORK(4),=4X'F0'                                                  
         BNE   INVSTA                                                           
*                                                                               
         AR    R5,R6               R6 = N'CHAR LESS THAN 4                      
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R5),0(R2)       SET DATA IN OUTPUT                           
*                                                                               
CBL35    ST    R1,STBSTRAD         SAVE CURRENT STRING ADDRESS                  
*                                                                               
         CLI   0(R1),C' '                                                       
         BNH   CBLX                                                             
         TM    MYBYTE,X'80'        IF THIS IS A STRING                          
         BNO   CBL40                                                            
         CLI   0(R1),C','          END OF INPUT CAN BE ','                      
         BE    CBLX                                                             
*                                                                               
CBL40    CLI   0(R1),C'/'          IF THERE IS A '/'                            
         BNE   *+8                                                              
         LA    R1,1(R1)            SKIP IT                                      
*                                                                               
         CLI   0(R1),C' '          IF END OF INPUT                              
         BNH   CBLX                DONE                                         
*                                                                               
         MVC   STBNET,0(R1)                                                     
         OI    STBNET+2,C' '       MAKE SURE NOT A X'00'                        
         CLI   STBNET+2,C','                                                    
         BNE   *+8                                                              
         MVI   STBNET+2,C' '                                                    
*                                                                               
         LA    R5,WORK             SET UP 3 --> 4 CHAR NET XLATE                
         USING STAPACKD,R5                                                      
         XC    WORK,WORK                                                        
         MVI   STAPACT,C'X'                                                     
         MVC   STAPQNET,STBNET                                                  
         MVC   STAPACOM,STBACOM                                                 
                                                                                
*================================================================               
* CALL STAPACK TO VALIDATE CABLE NETWORK USING TRANSLATE ACTION                 
*================================================================               
                                                                                
         L     RF,STBACOM                                                       
         L     RF,CCALLOV-COMFACSD(RF)                                          
         XC    DMCB(24),DMCB                                                    
         MVC   DMCB+4(4),=X'D9000A7A'  GET A(STAPACK)                           
         GOTO1 (RF),DMCB                                                        
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     RF,0(R1)            CALL STAPACK                                 
         GOTO1 (RF),WORK                                                        
*                                                                               
         CLI   STAPERR,QSTP_NOTACTV                                             
         BE    TURNITON                                                         
*                                                                               
         CLI   STAPERR,0                                                        
         BNE   INVNET                                                           
*                                                                               
CBLX     B     SVX                 RETURN TO USER                               
         DROP  R5                                                               
         EJECT                                                                  
*                                                                               
*        ERROR EXITS                                                            
*                                                                               
INVMED   MVI   STBERR,STBEMED      SET ERROR CODE - INVALID MEDIA               
         B     ERRXIT                                                           
*                                                                               
INVNET   MVI   STBERR,STBENET      SET ERROR CODE - INVALID NETWORK             
         B     ERRXIT                                                           
*                                                                               
INVSTA   MVI   STBERR,STBESTA      SET ERROR CODE - INVALID STATION             
         B     ERRXIT                                                           
*                                                                               
TURNITON MVI   STBERR,STBNOTON     SET ERROR CODE - NOT TURNED ON               
         B     ERRXIT                                                           
*                                                                               
ERRXIT   XC    STBSTA,STBSTA       CLEAR OUTPUT TO USER                         
         XC    STBNET,STBNET                                                    
         XC    STBSTRAD,STBSTRAD                                                
*                                                                               
XIT      L     R1,SAVER1                                                        
         XIT1                                                                   
*                                                                               
SPACES   DC    16C' '                                                           
*                                                                               
         LTORG                                                                  
VMED     DC    C'TRXNC'            VALID MEDIAS                                 
         DC    X'FF'                                                            
         EJECT                                                                  
STAD     DSECT                                                                  
DMCB     DS    6A                                                               
WORK     DS    CL32                                                             
SAVER1   DS    F                                                                
BAND     DS    CL1                                                              
MYBYTE   DS    CL1                                                              
SVDELIM  DS    CL1                                                              
STOPCHAR DS    CL1                                                              
STAX     EQU   *                                                                
         SPACE 1                                                                
*STABLOCK                                                                       
       ++INCLUDE SPSTABLK                                                       
*STAPACKD                                                                       
       ++INCLUDE SPSTAPACKD                                                     
       ++INCLUDE DDCOMFACS                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'024DDSTAVAL  04/20/16'                                      
         END                                                                    
