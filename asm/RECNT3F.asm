*          DATA SET RECNT3F    AT LEVEL 209 AS OF 01/15/02                      
*PHASE T8023FA +0                                                               
         TITLE 'T8023F - PAY S/P DISPLAY/UPDATE'                                
*                                                                               
*******************************************************************             
*                                                                 *             
*        RECNT3F (T8023F) --- PAY S/P DISPLAY/UPDATE              *             
*                                                                 *             
* --------------------------------------------------------------- *             
*  05DEC01 (BU ) --- CREATION                                     *             
*                                                                 *             
*                                                                 *             
*                                                                 *             
*                                                                 *             
*                                                                 *             
*                                                                 *             
*******************************************************************             
* IF YOU ARE GOING TO ADD ANYMORE MESSAGES TO THE BOTTOM OF THE                 
* SCREEN MAKE SURE YOU CHECK FOR END OF SCREEN (HISLAST)!!!                     
* YOU MAY NEED TO MOVE SOME MESSAGES TO THE RIGHT                               
*******************************************************************             
*                                                                               
T8023F   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T8023F,R9,R7                                                   
         L     RC,0(R1)                                                         
         USING GENOLD,RC                                                        
         USING TWAD,RA                                                          
         LR    R8,RA                                                            
         AH    R8,=Y(TWAWORKQ)                                                  
         USING TWAWORK,R8                                                       
         TM    PROFILES+CNTPSALB,CNTPSALA                                       
*                                  PROF 49 (PAY S/P USED)                       
         BZ    PAYERROR            NOT USED                                     
         TM    PSPPSPH+4,X'20'     NEW COMP S/P DISPLAYED                       
         BNO   PAYCHG              NO  - DO THE CHANGE                          
         EJECT                                                                  
PAYDIS   EQU   *                                                                
         LA    R1,PSPPSPH          CLEAR PAY S/P CODE FIELD                     
         XC    PSPPSP,PSPPSP       CLEAR CODE FIELD                             
         OI    6(R1),X'80'         SET TO TRANSMIT                              
         LA    R1,PSPPNAMH         CLEAR PAY S/P NAME FIELD                     
         XC    PSPPNAM,PSPPNAM     CLEAR NAME FIELD                             
         OI    6(R1),X'80'         SET TO TRANSMIT                              
*                                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,6               SET SALESPERSON KEY TYPE                     
         MVC   RSALKREP-RSALREC+KEY,REPALPHA                                    
*                                  INSERT REP CODE INTO KEY                     
*                                                                               
*   RETRIEVE RANDOM FLAG ELEMENT FOR COMPENSATION S/P USE                       
*                                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'1E'        FLAGS ELEMENT                                
         BAS   RE,GETEL                                                         
         BNE   PDIS0010            NOT FOUND - USE S/P OF RECORD                
         USING RCONRFEL,R6                                                      
         CLC   RCONRPSP,MYSPACES   COMPENSATION S/P ENTERED?                    
         BH    PDIS0020            YES - USE IT                                 
PDIS0010 EQU   *                                                                
         MVC   RSALKSAL-RSALREC+KEY,RCONSAL                                     
*                                  NO  - USE ORDER S/P CODE                     
         B     PDIS0040                                                         
PDIS0020 EQU   *                                                                
         MVC   RSALKSAL-RSALREC+KEY,RCONRPSP                                    
*                                  USE COMPENSATION S/P CODE                    
PDIS0040 EQU   *                                                                
         GOTO1 VHIGH               READ KEY SET UP                              
                                                                                
         CLC   KEY(27),KEYSAVE                                                  
         BE    PDIS0060                                                         
         MVC   PSPPSP,RCONRPSP     USE COMPENSATION S/P CODE                    
         MVC   PSPPNAM(16),=C'CODE NOT ON FILE'                                 
         MVC   PSPPOFF,RCONRSPO                                                 
         B     PDIS0080                                                         
*                                                                               
PDIS0060 EQU   *                                                                
         GOTO1 VGETREC,DMCB,IOAREA                                              
         MVC   PSPPSP,RSALKSAL     INSERT S/P CODE ON SCREEN                    
         MVC   PSPPNAM,RSALNAME    INSERT SALESPERSON NAME                      
         MVC   PSPPOFF,RSALOFF     INSERT SALESPERSON OFFICE                    
PDIS0080 EQU   *                                                                
         OI    PSPPSPH+4,X'20'     SET COMP S/P PREV VALID                      
         FOUT  PSPPSPH             TRANSMIT ALL FIELDS                          
         FOUT  PSPPNAMH                                                         
         FOUT  PSPPOFFH                                                         
                                                                                
         B     EXXMOD                                                           
         EJECT                                                                  
PAYCHG   EQU   *                                                                
         XC    KEY,KEY             VALIDATE EXISTENCE OF S/P                    
         MVI   KEY,6                                                            
         MVC   RSALKREP-RSALREC+KEY,REPALPHA                                    
*                                  INSERT REP CODE                              
         MVC   RSALKSAL-RSALREC+KEY(3),PSPPSP                                   
*                                  INSERT SALESPERSON FROM SCREEN               
         OC    RSALKSAL-RSALREC+KEY(3),=C'   '                                  
*                                  RESET BIN ZERO TO SPACES                     
         GOTO1 VHIGH               READ KEY SET UP                              
                                                                                
         CLC   KEY(27),KEYSAVE                                                  
         BNE   PCHGER01            S/P NOT ON FILE: ERROR # 1                   
         GOTO1 VGETREC,DMCB,IOAREA                                              
         MVC   PSPPSP,RSALKSAL     INSERT S/P CODE ON SCREEN                    
         MVC   PSPPNAM,RSALNAME    INSERT SALESPERSON NAME                      
         MVC   PSPPOFF,RSALOFF     INSERT SALESPERSON OFFICE                    
         OI    PSPPSPH+4,X'20'     SET COMP S/P PREV VALID                      
         FOUT  PSPPSPH             TRANSMIT ALL FIELDS                          
         FOUT  PSPPNAMH                                                         
         FOUT  PSPPOFFH                                                         
         MVC   KEY(27),RCONREC     RESET CONTRACT RECORD                        
         MVI   UPDATE,C'Y'                                                      
         GOTO1 VHIGH                                                            
         CLC   KEY(27),KEYSAVE     KEY FOUND?                                   
         BE    *+6                 YES                                          
         DC    H'0'                KEY MUST BE ON FILE                          
         MVI   UPDATE,C'Y'                                                      
         GOTO1 VGETREC,DMCB,RCONREC                                             
*                                  READ CONTRACT BACK IN FOR UPDATE             
*                                                                               
*   RETRIEVE THE RANDOM FLAG ELEMENT TO INSERT NEW COMP S/P                     
*                                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'1E'        FLAGS ELEMENT                                
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING RCONRFEL,R6                                                      
         MVC   RCONRPSP,RSALKSAL   COMPENSATION S/P FROM RECORD                 
         MVC   RCONRSPO,RSALOFF    COMPENSATION S/P OFFICE FROM RECORD          
         GOTO1 VPUTREC,DMCB,RCONREC                                             
         B     EXXMOD                                                           
         EJECT                                                                  
PCHGER01 EQU   *                                                                
         LA    R3,154              SET ERROR MESSAGE NUMBER                     
         LA    R2,PSPPSPH          SET CURSOR POSITION                          
         B     ERROR                                                            
PAYERROR EQU   *                                                                
         LA    R3,908              SET ERROR MESSAGE NUMBER                     
         LA    R2,CONBACTH         SET CURSOR POSITION                          
         B     ERROR                                                            
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE RECNTWR2K                                                      
         EJECT                                                                  
         ORG   CONLAST                                                          
       ++INCLUDE RECNTCBD                                                       
         EJECT                                                                  
       ++INCLUDE FAFACTS                                                        
         EJECT                                                                  
       ++INCLUDE FAUTL                                                          
         EJECT                                                                  
       ++INCLUDE REGENAUD                                                       
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'209RECNT3F   01/15/02'                                      
         END                                                                    
