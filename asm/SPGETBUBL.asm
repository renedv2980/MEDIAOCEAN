*          DATA SET SPGETBUBL  AT LEVEL 008 AS OF 06/28/02                      
*PHASE T00A77A                                                                  
*====================================================================*          
*                                                                    *          
*  PARAM 1     A(GETBUBLD)                                           *          
*                                                                    *          
*  USER PASSES FLDHDR CONTAINING NAME                                *          
*                                                                    *          
*  IF NAME IS NOT ON FILE, USER MUST PROVIDE A NAME                  *          
*                                                                    *          
*  IF NAME IS ON FILE, USER **MAY** PROVIDE A NAME                   *          
*     IF THE NAME BEGINS WITH '=', IT REPLACES EXISTING NAME         *          
*     ELSE NAME IN FLDHDR IS IGNORED AND RECORD IS UPDATED WITH      *          
*     NAME FROM FLDHDR                                               *          
*====================================================================*          
GETBUBL  TITLE 'RETRIEVE/STORE NAMES IN SPOTPAK STATUS RECORDS'                 
GETBUBL  CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 WORKX-WORKD,GETBUBL,CLEAR=YES                                    
         USING WORKD,RC                                                         
*                                                                               
         L     R9,0(R1)            SAVE PARAM POINTER                           
         USING GETBUBLD,R9                                                      
         MVI   GBERR,0                                                          
*                                                                               
         L     RA,GBCOMFAC                                                      
         USING COMFACSD,RA                                                      
         SPACE 1                                                                
*======================================================*                        
* READ ST PROFILE                                      *                        
*======================================================*                        
         SPACE 1                                                                
         MVC   KEY(4),=C'S0ST'                                                  
         MVC   KEY+4(2),GBAGY                                                   
         MVC   KEY+6(1),GBMEDEBC                                                
         MVC   KEY+7(3),GBCLTEBC                                                
         MVI   KEY+10,C'*'                                                      
         MVC   KEY+11(1),GBOFFICE                                               
*                                                                               
         GOTO1 CGETPROF,DMCB,KEY,SVPROF,CDATAMGR                                
*                                                                               
         OC    SVPROF,SVPROF                                                    
         BNZ   *+10                                                             
         MVC   SVPROF(3),=C'CME'   DEFAULT TO CLIENT/MARKET/ESTIMATE            
*                                                                               
         CLI   SVPROF+4,C'Y'       TEST SUPPRESS THIS LEVEL                     
         BE    EXIT                                                             
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING STATKEY,R4                                                       
*                                                                               
         MVC   STKTYPE,=X'0D71'                                                 
         MVC   STKAGMD,GBAGYMD                                                  
         MVC   STKCLT,GBCLT                                                     
*                                                                               
         MVI   STKPRD,0            SET CLT LEVEL (PRD=0)                        
         CLI   SVPROF+0,C'P'       TEST PROFILE                                 
         BNE   *+10                                                             
         MVC   STKPRD,GBPRD                                                     
*                                                                               
         MVI   STRECTYP,0          FORCE RECTYPE                                
*                                                                               
         MVI   STKEST,0            SET PPRD LEVEL (EST=0)                       
         CLI   SVPROF+2,C'E'       TEST PROFILE                                 
         BNE   *+10                                                             
         MVC   STKEST,GBEST                                                     
*                                                                               
         MVC   STKMKT,GBMKT        SET MARKET                                   
*                                                                               
         XC    STKSTA,STKSTA       FORCE STATION                                
         CLI   SVPROF+1,C'S'       TEST PROFILE                                 
         BNE   *+10                                                             
         MVC   STKSTA,GBSTA                                                     
*                                                                               
         MVC   KEYSAVE,KEY                                                      
         GOTO1 CDATAMGR,DMCB,=C'DMRDHI',=C'SPTDIR',KEY,KEY                      
         CLC   KEY(13),KEYSAVE                                                  
         BE    GB50                                                             
         B     GB10                                                             
         SPACE 1                                                                
EQXIT    CR    RB,RB               SET CC EQ                                    
         B     EXIT                                                             
*                                                                               
NEQXIT   LTR   RB,RB               SET CC NEQ                                   
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
*==================================*                                            
* CREATE NEW RECORD                *                                            
*==================================*                                            
         SPACE 1                                                                
GB10     L     R4,GBIOA                                                         
         XC    0(256,R4),0(R4)                                                  
         MVC   STATKEY,KEYSAVE     SET KEY IN RECORD                            
         LA    RE,24                                                            
         LA    RE,BPLENQ(RE)                                                    
         LA    RE,CSLENQ(RE)                                                    
         STCM  RE,3,STLEN                                                       
         MVC   STALPHA,GBAGY                                                    
*                                                                               
         MVI   BPCODE,BPCODEQ                                                   
         MVI   BPLEN,BPLENQ                                                     
         MVC   BPBUYER,SPACES                                                   
         MVC   BPPAYER,SPACES                                                   
*                                                                               
         BAS   RE,GBMOVE           MOVE NAME TO RECORD                          
         BNE   EXIT                                                             
*                                                                               
         MVI   CSCODE,CSCODEQ                                                   
         MVI   CSLEN,CSLENQ                                                     
*                                                                               
         GOTO1 CDATAMGR,DMCB,=C'ADDREC',=C'SPTFILE',KEY+14,GBIOA,      X        
               DMWORK                                                           
*                                                                               
         MVC   GBERR,8(R1)         PASS ERROR CODE TO USER (IF ANY)             
         B     EXIT                                                             
         EJECT                                                                  
*===========================================*                                   
* UPDATE/RETURN VALUES FROM EXISTING RECORD *                                   
*===========================================*                                   
         SPACE 1                                                                
GB50     DS    0H                                                               
         GOTO1 CDATAMGR,DMCB,(X'80',=C'GETREC'),=C'SPTFILE',KEY+14,    X        
               GBIOA,DMWORK                                                     
         CLI   8(R1),0                                                          
         BE    GB60                                                             
         MVC   GBERR,8(R1)                                                      
         B     EXIT                                                             
*                                                                               
GB60     L     R4,GBIOA                                                         
         CLI   24(R4),X'01'        TEST 01 ELEMENT PRESENT                      
         BE    GB62                                                             
*                                                                               
         XC    ELEM,ELEM           SO MAKE ONE !                                
         MVI   ELEM,X'01'                                                       
         MVI   ELEM+1,26                                                        
*                                                                               
         SR    R1,R1                                                            
         ICM   R1,3,13(R4)         GET RECORD LEN                               
         AR    R1,R4                                                            
         XC    0(2,R1),0(R1)       CLEAR BYTES FOLLOWING RECORD                 
*                                                                               
         LA    R1,24(R4)           NOW GO THROUGH ELEMENTS                      
         SR    R0,R0                                                            
*                                                                               
GB61     IC    R0,1(R1)                                                         
         AR    R1,R0                                                            
         CLI   0(R1),0             STOP ON ELEM CODE=0                          
         BE    *+12                                                             
         CLI   1(R1),0             OR ELEM LEN=0                                
         BNE   GB61                                                             
         XC    0(3,R1),0(R1)                                                    
         GOTO1 CHELLO,DMCB,(C'P',=C'SPTFILE'),(R4),ELEM                         
*                                                                               
GB62     LA    R1,BPBUYER                                                       
         CLI   GBTYPE,C'B'                                                      
         BE    *+8                                                              
         LA    R1,BPPAYER                                                       
         CLC   0(12,R1),SPACES     IS THERE A NAME IN RECORD                    
         BNH   GB70                NO - GO UPDATE RECORD                        
         SPACE 1                                                                
*=========================================*                                     
* NAME IS IN RECORD - TEST USER OVERRIDE  *                                     
*=========================================*                                     
         SPACE 1                                                                
         L     R2,GBNAMFLD         POINT TO FLDHDR                              
         CLI   8(R2),C'='          TEST FOR OVERRIDE                            
         BE    GB70                IF YES, GO UPDATE RECORD                     
         SPACE 1                                                                
*=========================================*                                     
* RETURN NAME TO USER                     *                                     
*=========================================*                                     
         SPACE 1                                                                
         ZIC   RE,0(R2)            GET FLDHDR LEN                               
         SH    RE,=H'8'                                                         
         TM    1(R2),X'02'         TEST EXTENDED FLDHDR                         
         BZ    *+8                                                              
         SH    RE,=H'8'                                                         
*                                                                               
         LA    R0,L'BPBUYER        MAX LENGTH                                   
         CR    RE,R0                                                            
         BNH   *+6                                                              
         LR    RE,R0                                                            
         BCTR  RE,0                SET FOR EX                                   
*                                                                               
         LTR   RE,RE                                                            
         BNM   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),0(R1)       **EXECUTED**                                 
*                                                                               
         OI    4(R2),X'20'         SET PREVIOUSLY VALIDATED                     
         OI    6(R2),X'80'         SET XMT BIT                                  
         B     EXIT                                                             
         SPACE 2                                                                
*======================================*                                        
* UPDATE RECORD                        *                                        
*======================================*                                        
         SPACE 1                                                                
GB70     BAS   RE,GBMOVE           MOVE OVERRIDE TO RECORD                      
         BNE   EXIT                                                             
*                                                                               
         GOTO1 CDATAMGR,DMCB,=C'PUTREC',=C'SPTFILE',KEY+14,GBIOA,      X        
               DMWORK                                                           
         B     EXIT                                                             
         EJECT                                                                  
*============================================================*                  
* SUBROUTINE MOVES USER NAME TO RECORD AND ADJUSTS FOR '='   *                  
*============================================================*                  
         SPACE 1                                                                
GBMOVE   NTR1                                                                   
         LA    R1,BPBUYER                                                       
         CLI   GBTYPE,C'B'                                                      
         BE    *+8                                                              
         LA    R1,BPPAYER                                                       
         MVC   0(12,R1),SPACES     BLANK PREVIOUS                               
*                                                                               
         L     R2,GBNAMFLD         POINT TO FLDHDR                              
         ZIC   RE,5(R2)            GET ACTUAL FIELD LEN                         
         LA    R0,L'BPBUYER        SET MAX LEN IN REG                           
         CR    RE,R0                                                            
         BL    *+6                                                              
         LR    RE,R0                                                            
         BCTR  RE,0                                                             
*                                                                               
         CLI   8(R2),C'='                                                       
         BNE   *+10                                                             
         LA    R2,1(R2)                                                         
         BCTR  RE,0                                                             
*                                                                               
         LTR   RE,RE                                                            
         BNM   *+12                                                             
         MVI   GBERR,X'81'         SET MISSING INPUT FIELD                      
         B     NEQXIT                                                           
*                                                                               
         EX    RE,*+8              MOVE NAME TO ELEMENT                         
         B     *+10                                                             
         MVC   0(0,R1),8(R2)      **EXECUTED**                                  
         OC    0(12,R1),SPACES                                                  
         B     EQXIT                                                            
         SPACE 1                                                                
SPACES   DC    CL12' '                                                          
         LTORG                                                                  
         EJECT                                                                  
WORKD    DSECT                                                                  
KEY      DS    CL24                                                             
KEYSAVE  DS    CL24                                                             
DMCB     DS    6F                                                               
DMWORK   DS    6D                                                               
ELEM     DS    XL48                                                             
SVPROF   DS    CL16                                                             
WORKX    EQU   *                                                                
         EJECT                                                                  
       ++INCLUDE SPGENSTAT                                                      
         EJECT                                                                  
BUBLD    DSECT                                                                  
       ++INCLUDE SPGETBUBLD                                                     
         EJECT                                                                  
       ++INCLUDE DDCOMFACS                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'008SPGETBUBL 06/28/02'                                      
         END                                                                    
