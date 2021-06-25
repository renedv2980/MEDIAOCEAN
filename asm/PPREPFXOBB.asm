*          DATA SET PPREPFXOBB AT LEVEL 021 AS OF 03/22/96                      
*                                                                               
*     (3/5/96) CONVERTS BBCH (BB)  ACC OFFICES                                  
*                                                                               
*        QOPT1   X= CLEAR PCLTAOFC AND PCLTACCA                                 
*                (USE IF THERE WAS A PROBELM WITH THE CONVERSION                
*                 AND WE NEED TO GO BACK)                                       
*                                                                               
*        QOPT5   N= DON'T MARK FILE (EVEN IF WRITE=YES)                         
*                                                                               
*                                                                               
*PHASE PP0202B,+0,NOAUTO                                                        
*INCLUDE MININAM                                                                
         TITLE 'PP0202 - PRTFIX PROGRAM TO FIX OFFICE CODES'                    
         PRINT NOGEN                                                            
PP0202   CSECT                                                                  
         NMOD1 0,PP0202                                                         
*                                                                               
         L     RA,0(R1)                                                         
         USING PPWORKD,RA                                                       
         L     RC,PPFILEC                                                       
         LA    R9,1(RC)                                                         
         LA    R9,4095(R9)                                                      
         USING PPFILED,RC,R9                                                    
         LA    R8,SPACEND                                                       
         USING PP02WRKD,R8                                                      
**                                                                              
         CLI   MODE,RUNFRST                                                     
         BE    RUNF                                                             
         CLI   MODE,RUNLAST                                                     
         BE    RUNL                                                             
         CLI   MODE,PROCREQ                                                     
         BE    PROC                                                             
         B     EXIT                                                             
*                                                                               
RUNF     DS    0H                                                               
         LA    R0,PCLTREC                                                       
         ST    R0,AREC                                                          
         B     EXIT                                                             
*                                                                               
PROC     DS    0H                                                               
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
         CLI   QOPT5,C'N'                                                       
         BNE   *+8                                                              
         MVI   RCWRITE,C'N'                                                     
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(3),QAGENCY      AGENCY,MEDIA                                 
         MVI   KEY+3,X'02'         CLTREC                                       
AGYC2    GOTO1 HIGH                                                             
         B     AGYC4                                                            
*                                                                               
AGYC3    DS    0H                                                               
         GOTO1 SEQ                                                              
AGYC4    DS    0H                                                               
         CLC   KEY(3),QAGENCY     MATCH AGENCY/MEDIA/CLTREC TYPE                
         BNE   EXIT                                                             
         CLI   KEY+3,X'02'                                                      
         BNE   EXIT                                                             
*                                                                               
AGYC6    DS    0H                                                               
         LA    R0,PCLTREC                                                       
         ST    R0,AREC                                                          
         GOTO1 GETPRT                                                           
*                                                                               
         LA    R1,OFFTAB                                                        
         LA    R0,OFFTABN                                                       
*                                                                               
LOOP     CLC   PCLTOFF,2(R1)                                                    
         BL    ERR                                                              
         BE    PRT                                                              
         LA    R1,3(R1)                                                         
         BCT   R0,LOOP                                                          
*                                                                               
ERR      MVC   P(26),=C'COULDN''T CONVERT BB CLIENT'                            
         MVC   P+32(3),PCLTKCLT                                                 
         MVC   P+38(7),=C'OLD OFC'                                              
         MVC   P+46(1),PCLTOFF                                                  
         MVC   P+50(13),=C'OLD ACCO/AGY='                                       
         MVC   P+64(2),PCLTAOFC                                                 
         MVI   P+66,C'/'                                                        
         MVC   P+67(2),PCLTACCA                                                 
*                                                                               
         BAS   RE,RPRT                                                          
         B     AGYC3                                                            
*                                                                               
PRT      DS    0H                                                               
         XC    PCLTACCA,PCLTACCA                                                
*****    XC    PCLTAOFC,PCLTAOFC                                                
         CLI   QOPT1,C'X'      SEE IF "UNDOING" THE CONVERSION                  
         BE    PRT5            THEN JUST CLEAR PCLTAOFC AND PCLTACCA            
*                                                                               
*****    MVC   PCLTAOFC,0(R1)   SET PCLTAOFC FROM OFFTAB                        
         MVC   PCLTACCA,=C'BD'  SET ACC AGENCY TO BD                            
*                                                                               
PRT5     CLI   RCWRITE,C'N'                                                     
         BE    AGYC6D                                                           
         GOTO1 PUTPRT                                                           
*                                                                               
AGYC6D   MVC   P(3),PCLTKAGY                                                    
         MVC   P+4(3),PCLTKCLT                                                  
         MVC   P+8(1),PCLTOFF                                                   
         MVC   P+10(2),PCLTAOFC                                                 
         MVC   P+13(4),=C'ACCA'                                                 
         MVC   P+18(2),PCLTACCA                                                 
         BAS   RE,RPRT                                                          
         B     AGYC3                                                            
*                                                                               
EXIT     DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
RUNL     DS    0H                                                               
         MVI   FORCEHED,C'Y'                                                    
         BAS   RE,RPRT                                                          
         B     EXIT                                                             
*                             LINK TO REPORT                                    
         SPACE 2                                                                
         DS    0F                                                               
RPRT     NTR1                                                                   
         SPACE 2                                                                
         MVC   HEAD5+62(8),=C'WRITE=NO'                                         
         CLI   RCWRITE,C'Y'                                                     
         BNE   *+10                                                             
         MVC   HEAD5+68(3),=C'YES'                                              
*                                                                               
         GOTO1 REPORT                                                           
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
NEXTEL   DS    0H                                                               
         SR    R0,R0                                                            
         IC    R0,1(R2)                                                         
         AR    R2,R0                                                            
         CLC   ELCODE,0(R2)                                                     
         BER   RE                                                               
         CLI   0(R2),0                                                          
         BNE   NEXTEL+2                                                         
         LTR   RE,RE                                                            
         BR    RE                                                               
         SPACE 3                                                                
*                                                                               
         LTORG                                                                  
         SPACE 2                                                                
*---------------------------------------------------------------------*         
*        OFFICE CONVERSION TABLE - BBCH (BB)  (3/5/96)                *         
*---------------------------------------------------------------------*         
OFFTAB   DS    0H                                                               
         DC    CL2'CA',X'00'                                                    
         DC    CL2'CA',CL1' '                                                   
         DC    CL2'CA',CL1'C'                                                   
         DC    CL2'CA',CL1'1'                                                   
         DC    CL2'CA',CL1'8'                                                   
         DC    X'FF'                                                            
OFFTABN  EQU   (*-OFFTAB)/3                                                     
PP02WRKD DSECT                                                                  
ELCODE   DS    X                                                                
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE PPNEWFILE                                                      
       ++INCLUDE PPREPWORK                                                      
       ++INCLUDE PPMODEQU                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'021PPREPFXOBB03/22/96'                                      
         END                                                                    
