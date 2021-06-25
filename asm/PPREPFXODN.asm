*          DATA SET PPREPFXODN AT LEVEL 035 AS OF 06/26/98                      
*                                                                               
*     (5/27/98 CONVERTS DDB CANADA (DN) ACC OFFICES                             
*                                                                               
*        QOPT5   N= DON'T MARK FILE (EVEN IF WRITE=YES)                         
*                                                                               
*                                                                               
*PHASE PP0202D,+0,NOAUTO                                                        
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
         ZAP   INCNT,=P'0'         RECORDS TESTED                               
         ZAP   CHGCNT,=P'0'        RECORDS CHANGED                              
         ZAP   WRTCNT,=P'0'        RECORDS WRITTEN                              
         B     EXIT                                                             
*                                                                               
PROC     DS    0H                                                               
         MVI   FORCEHED,C'Y'                                                    
         MVI   RCSUBPRG,30                                                      
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
         AP    INCNT,=P'1'                                                      
         LA    R0,PCLTREC                                                       
         ST    R0,AREC                                                          
         GOTO1 GETPRT                                                           
*                                                                               
         LA    R1,OFFTAB                                                        
         LA    R0,OFFTABN                                                       
*                                                                               
         MVC   TESTOFC,PCLTAOFC                                                 
         CLI   PCLTAOFC,C' '       ACC OFFICE BLANK ?                           
         BH    LOOP                NO - USE IT FOR CONVERSION                   
         MVC   TESTOFC,PCLTOFF     YES - USE MEDIA OFFICE TO CONVERT            
*                                                                               
LOOP     CLC   TESTOFC,2(R1)                                                    
         BL    ERR                                                              
         BE    PRT                                                              
         LA    R1,3(R1)                                                         
         BCT   R0,LOOP                                                          
*                                                                               
ERR      MVC   P(26),=C'COULDN''T CONVERT DN CLIENT'                            
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
         AP    CHGCNT,=P'1'                                                     
         MVC   P+1(3),PCLTKAGY     "OLD" DATA                                   
         MVC   P+6(3),PCLTKCLT                                                  
         MVC   P+13(1),PCLTOFF                                                  
         MVC   P+20(2),PCLTAOFC                                                 
         MVC   P+29(2),PCLTACCA                                                 
*                                                                               
         XC    PCLTACCA,PCLTACCA                                                
         MVC   PCLTAOFC,0(R1)   SET PCLTAOFC FROM OFFTAB                        
******** MVC   PCLTACCA,=C'BS'  NO ACC AGENCY REQUIRED                          
         MVC   P+37(1),PCLTOFF     "NEW" DATA                                   
         MVC   P+44(2),PCLTAOFC                                                 
         MVC   P+53(2),PCLTACCA                                                 
*                                                                               
PRT5     CLI   RCWRITE,C'N'                                                     
         BE    AGYC6D                                                           
         AP    WRTCNT,=P'1'                                                     
         GOTO1 PUTPRT                                                           
*                                                                               
AGYC6D   DS    0H                                                               
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
*                                                                               
         LA    R4,COUNTS                                                        
RUNL5    CLI   0(R4),X'FF'                                                      
         BE    RUNL10                                                           
         MVC   P(15),8(R4)                                                      
         OI    7(R4),X'0F'                                                      
         UNPK  P+20(10),0(8,R4)                                                 
         GOTO1 REPORT                                                           
         LA    R4,23(R4)                                                        
         B     RUNL5                                                            
*                                                                               
RUNL10   DS    0H                                                               
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
*        OFFICE CONVERSION TABLE - DDB CANADA (DN)  5/27/98           *         
*---------------------------------------------------------------------*         
OFFTAB   DS    0H                                                               
*****    DC    CL2'11',XL1'00'                                                  
*****    DC    CL2'12',CL1'!'                                                   
*****    DC    CL2'11',CL1'#'                                                   
*****    DC    CL2'11',XL1'89'                                                  
         DC    CL2'AT',CL1'A'                                                   
         DC    CL2'DB',CL1'B'                                                   
         DC    CL2'DM',CL1'C'                                                   
         DC    CL2'AC',CL1'D'                                                   
         DC    CL2'AE',CL1'E'                                                   
         DC    CL2'AF',CL1'F'                                                   
*****    DC    CL2'33',CL1'G'                                                   
         DC    CL2'DH',CL1'H'                                                   
         DC    CL2'GT',CL1'J'                                                   
*****    DC    CL2'6K',CL1'K'                                                   
         DC    CL2'GL',CL1'L'                                                   
         DC    CL2'AM',CL1'M'                                                   
*****    DC    CL2'11',CL1'N'                                                   
         DC    CL2'OC',CL1'O'                                                   
*****    DC    CL2'2P',CL1'P'                                                   
*****    DC    CL2'1Q',CL1'Q'                                                   
         DC    CL2'ZR',CL1'R'                                                   
         DC    CL2'DA',CL1'S'                                                   
         DC    CL2'DP',CL1'T'                                                   
*****    DC    CL2'1U',CL1'U'                                                   
*****    DC    CL2'1V',CL1'V'                                                   
*****    DC    CL2'5W',CL1'W'                                                   
*****    DC    CL2'1X',CL1'X'                                                   
*****    DC    CL2'1Y',CL1'Y'                                                   
*****    DC    CL2'11',CL1'Z'                                                   
*****    DC    CL2'11',CL1'1'                                                   
         DC    CL2'ZD',CL1'2'                                                   
         DC    CL2'DG',CL1'3'                                                   
         DC    CL2'DE',CL1'4'                                                   
         DC    CL2'ZH',CL1'5'                                                   
         DC    CL2'ZA',CL1'6'                                                   
*****    DC    CL2'A7',CL1'7'                                                   
*****    DC    CL2'48',CL1'8'                                                   
         DC    CL2'99',CL1'9'                                                   
         DC    X'FF'                                                            
OFFTABN  EQU   (*-OFFTAB)/3                                                     
*                                                                               
COUNTS   DS    0C                                                               
INCNT    DS    PL8                                                              
         DC    CL15'RECORDS TESTED'                                             
CHGCNT   DS    PL8                                                              
         DC    CL15'RECORDS CHANGED'                                            
WRTCNT   DS    PL8                                                              
         DC    CL15'RECORDS WRITTEN'                                            
         DC    X'FF'                                                            
DUMPCNT  DS    PL8                                                              
*                                                                               
*                                                                               
PP02WRKD DSECT                                                                  
ELCODE   DS    X                                                                
TESTOFC  DS    C                                                                
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE PPNEWFILE                                                      
       ++INCLUDE PPREPWORK                                                      
       ++INCLUDE PPMODEQU                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'035PPREPFXODN06/26/98'                                      
         END                                                                    
