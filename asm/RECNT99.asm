*          DATA SET RECNT99    AT LEVEL 030 AS OF 02/05/96                      
*PHASE T80228B,+0                                                               
*INCLUDE RETIMVAL                                                               
*INCLUDE DAYVAL                                                                 
         TITLE 'T80228 - REPPAK KATZ DELETER:  ONE-SHOT'                        
*                                                                               
*******************************************************************             
*                                                                 *             
*     RECNT15 (T80228) --- DELETE ENTIRE ORDER WHEN 'RTS' ENTERED *             
*                                                                 *             
*                                                                 *             
* --------------------------------------------------------------- *             
* UPDATE HISTORY:                                                 *             
*                                                                 *             
*                                                                 *             
*                                                                 *             
*                   ***  END TOMBSTONE  ***                       *             
*******************************************************************             
*                                                                               
T80228   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T80228,R9,R8                                                   
         L     RC,0(R1)            WORK                                         
         USING GENOLD,RC                                                        
         USING TWAD,RA                                                          
*                                                                               
         MVI   STAT2,0             CLEAR OUT STATUS BYTE                        
         LA    R2,CONBACTH                                                      
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)                                                  
         USING TWAWORK,RF                                                       
         MVC   KEY+28(4),TWAKADDR  K DISK ADDR                                  
         MVI   UPDATE,C'Y'                                                      
         DROP  RF                                                               
         GOTO1 VGETREC,DMCB,IOAREA                                              
         SPACE 1                                                                
         LA    R2,CONBACTH                                                      
         LA    R3,BACERR                                                        
         TM    IOAREA+29,X'01'     COMPRESSED CONTRACT                          
         BO    ERROR                                                            
         SPACE 1                                                                
         GOTO1 VMOVEREC,DMCB,IOAREA,RCONREC                                     
         SPACE 1                                                                
         TM    RCONMODR+1,X'10'    KATZ CONVERTED ORDER?                        
         BZ    BUED1000            NO  - EXIT WITH NO ACTION                    
         MVC   KEY,IOAREA          RESET KEY FOR ANOTHER READ                   
         MVI   UPDATE,C'Y'                                                      
         GOTO1 VHIGH               RE-READ THE KEY                              
         MVI   UPDATE,C'Y'                                                      
         GOTO1 VGETREC,DMCB,IOAREA                                              
*                                  READ THE CONTRACT AGAIN                      
         GOTO1 HELLO,DMCB,(C'D',=C'REPFILE'),(X'03',RCONREC),0,0                
         OI    KEY+27,X'80'        TURN ON DELETE BIT                           
         GOTO1 VPUTREC,DMCB,RCONREC                                             
*                                  REWRITE CONTRACT RECORD                      
         GOTO1 VWRITE              REWRITE KEY AS DELETED                       
*                                                                               
*   THE BUYS ARE NOW DELETED                                                    
*                                                                               
         XC    KEY,KEY             CLEAR THE KEY                                
         MVI   KEY,X'0B'           INSERT RECORD TYPE                           
         MVC   KEY+16(2),TWAAGY    INSERT REP CODE                              
         ZAP   WORK+15(5),=P'99999999'                                          
         MVO   WORK+15(5),RCONKCON                                              
         ZAP   WORK+5(5),=P'99999999'                                           
         SP    WORK+5(5),WORK+15(5)                                             
         MVO   WORK+15(5),WORK+5(5)                                             
         PACK  KEY+18(1),WORK+18(1)    REVERSE THE COMPLIMENT                   
         PACK  KEY+19(1),WORK+17(1)                                             
         PACK  KEY+20(1),WORK+16(1)                                             
         PACK  KEY+21(1),WORK+15(1)                                             
         MVI   UPDATE,C'Y'                                                      
         GOTO1 VHIGH                                                            
         B     BUED0120                                                         
BUED0100 EQU   *                                                                
         GOTO1 VSEQ                                                             
BUED0120 EQU   *                                                                
         CLC   KEY(22),KEYSAVE     SAME KEY THROUGH CON #?                      
         BE    BUED0140            YES                                          
         B     BUED1000                                                         
BUED0140 EQU   *                                                                
         OI    KEY+27,X'80'        TURN ON DELETE BIT                           
         GOTO1 VWRITE              REWRITE KEY AS DELETED                       
BUED0160 EQU   *                                                                
         B     BUED0100            GO BACK FOR NEXT BUY                         
BUED1000 EQU   *                                                                
                                                                                
         B     EXXMOD                                                           
         SPACE 1                                                                
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE RECNTWR2K                                                      
         EJECT                                                                  
         ORG   CONLAST                                                          
       ++INCLUDE RECNTFED                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'030RECNT99   02/05/96'                                      
         END                                                                    
