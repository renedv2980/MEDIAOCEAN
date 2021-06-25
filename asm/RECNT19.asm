*          DATA SET RECNT19    AT LEVEL 008 AS OF 05/01/02                      
*PHASE T80219A                                                                  
         TITLE 'T80219 - BUY PASSIVE KEYS'                                      
*                                                                               
*******************************************************************             
*                                                                 *             
*     RECNT19 (T80219) --- PASSIVE KEYS FOR BUYS WITH SPOT ELEM   *             
*                                                                 *             
*  KEY MUST HAVE KEY OF LAST BUY UPDATED BEFORE ENTRY (INCL D/A)  *             
*  P1 = RC                                                        *             
*  P2 = A(BUYREC)                                                 *             
*                                                                 *             
* --------------------------------------------------------------- *             
* UPDATE HISTORY:                                                 *             
*                                                                 *             
* 14AUG91 (EFJ) --- ORIGINAL DEVELOPMENT                          *             
*                                                                 *             
* 06OCT95 (SKU) --- 2K CONTRACT SUPPORT                           *             
*                                                                 *             
* 19SEP01 (BU ) --- '9B01' KEY FOR BUYLINE CODE PASSIVE           *             
*                                                                 *             
* 29APR02 (BU ) --- '9B01': FOR DELETE, DON'T O/P NEW KEY         *             
*                                                                 *             
* 30APR02 (BU ) --- '9B01': FOR CANCEL, DON'T O/P NEW KEY         *             
*                                                                 *             
*                                                                 *             
*                                                                 *             
*******************************************************************             
*                                                                               
T80219   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T80219                                                         
         L     RC,0(R1)                                                         
         USING GENOLD,RC                                                        
         MVC   ABUYREC,4(R1)                                                    
*                                                                               
         L     R6,ABUYREC                                                       
         MVI   ELCODE,X'08'                                                     
         BAS   RE,GETEL                                                         
         BNE   PASS0040            NO SPOTPAK TRANSFER ELEM                     
*                                                                               
         MVI   KEY,X'9B'           INSERT KEY TYPE                              
         MVC   KEY+9(2),RCONKREP                                                
         MVC   KEY+11(4),RCONKADV                                               
         MVC   KEY+15(3),RCONPRD                                                
         GOTO1 VHIGH                                                            
         CLC   KEY(27),KEYSAVE                                                  
         BE    PASS0020                                                         
         MVC   KEY,KEYSAVE                                                      
         GOTO1 VADD                                                             
         B     PASS0040                                                         
*                                                                               
PASS0020 DS    0H                                                               
         MVI   UPDATE,C'Y'                                                      
         GOTO1 VREAD                                                            
         MVC   KEY,KEYSAVE         HAS CORRECT D/A & CTL BYTE                   
         GOTO1 VWRITE              UPDATE KEY                                   
         B     PASS0040                                                         
*                                                                               
PASS0040 DS    0H                                                               
         L     R6,ABUYREC                                                       
         MVI   ELCODE,X'5F'                                                     
         BAS   RE,GETEL                                                         
         BNE   PASS0200            NO BUYLINE CODE ELEMENT                      
*                                                                               
         MVC   KEY(2),=X'9B01'                                                  
         MVC   KEY+11(2),RCONKREP                                               
*                                  INSERT REP CODE                              
         MVC   KEY+13(3),RBYSCDBC-RBYSCDEL(R6)                                  
*                                  INSERT BUYLINE CODE                          
         MVC   KEY+16(4),RCONKCON  INSERT CONTRACT NUMBER                       
         L     RF,ABUYREC                                                       
         USING RBUYREC,RF                                                       
         MVC   KEY+20(1),RBUYKLIN  INSERT BUYLINE NUMBER                        
         MVC   KEY+21(3),=X'FFFFFF'                                             
*                                  INITIALIZE EARLIEST DATE                     
         XC    KEY+24(3),KEY+24    INITIALIZE LATEST DATE                       
*                                                                               
         DROP  RF                                                               
*                                                                               
         L     R6,ABUYREC                                                       
         MVI   ELCODE,X'03'                                                     
         BAS   RE,GETEL                                                         
         B     PASS0080                                                         
PASS0060 DS    0H                                                               
         BAS   RE,NEXTEL                                                        
PASS0080 DS    0H                                                               
         BNE   PASS0140            NO (MORE) EFF DATE ELTS                      
         USING RBUYDTEL,R6                                                      
         CLC   RBUYDTST,KEY+21     EFF START DATE EARLIER?                      
         BH    PASS0100            NO                                           
         MVC   KEY+21(3),RBUYDTST  YES - USE IT                                 
PASS0100 EQU   *                                                                
         CLC   RBUYDTED,KEY+21     EFF END   DATE LATER?                        
         BNH   PASS0120            NO                                           
         MVC   KEY+24(3),RBUYDTED  YES - USE IT                                 
PASS0120 EQU   *                                                                
         B     PASS0060            GO BACK FOR NEXT X'03' ELEMENT               
PASS0140 DS    0H                                                               
*                                                                               
*   NEED TO DELETE ANY PRIOR KEY WHICH MAY HAVE DIFFERENT FLIGHT                
*                                                                               
         MVC   KEEPKEY,KEY         SAVE NEWLY BUILT KEY                         
         XC    KEY+21(6),KEY+21    CLEAR OUT FLIGHT DATES                       
         GOTO1 VHIGH               READ FOR NON-DELETED KEYS ONLY               
         CLC   KEY(21),KEYSAVE     KEY THROUGH BUYLINE# FOUND?                  
         BNE   PASS0160            NO                                           
         MVI   UPDATE,C'Y'         READ WHOLE KEY FOR UPDATE                    
         GOTO1 VREAD                                                            
         OI    KEY+27,X'80'                                                     
*                                  SET KEY FOR DELETION                         
         GOTO1 VWRITE              REWRITE KEY AS DELETED                       
         CLC   BUYACT,=C'DEL'      DELETE ACTION?                               
         BE    PASS0200                                                         
         CLC   BUYACT,=C'CAN'      CANCEL ACTION?                               
         BE    PASS0200                                                         
PASS0160 EQU   *                                                                
         MVC   KEY(27),KEEPKEY     RESTORE NEWLY BUILT KEY                      
         OI    DMINBTS,X'08'       PASS BACK DELETED RECORDS ALSO               
         GOTO1 VHIGH                                                            
         CLC   KEY(27),KEYSAVE     KEY FOUND?                                   
         BE    PASS0180            YES                                          
         MVC   KEY,KEYSAVE         NO  - ADD AS NEW KEY                         
         GOTO1 VADD                                                             
         B     PASS0200                                                         
*                                                                               
PASS0180 DS    0H                                                               
         MVI   UPDATE,C'Y'         READ KEY FOR UPDATE                          
         GOTO1 VREAD                                                            
         MVC   KEY,KEYSAVE         HAS CORRECT D/A & CTL BYTE                   
         NI    KEY+27,X'FF'-X'80'                                               
         GOTO1 VWRITE              UPDATE KEY                                   
         B     PASS0200                                                         
PASS0200 EQU   *                                                                
         B     EXXMOD                                                           
*                                                                               
*                                                                               
ABUYREC  DS    A                                                                
KEEPKEY  DS    XL27                                                             
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE RECNTWR2K                                                      
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'008RECNT19   05/01/02'                                      
         END                                                                    
