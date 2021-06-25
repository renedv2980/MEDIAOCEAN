*          DATA SET PPREPFXOBS AT LEVEL 028 AS OF 01/30/97                      
*                                                                               
*     (1/13/97 CONVERTS BACKER (BS) ACC OFFICES                                 
*                                                                               
*        QOPT5   N= DON'T MARK FILE (EVEN IF WRITE=YES)                         
*                                                                               
*                                                                               
*PHASE PP0202S,+0,NOAUTO                                                        
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
ERR      MVC   P(26),=C'COULDN''T CONVERT BS CLIENT'                            
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
*        OFFICE CONVERSION TABLE - BS (BS)  (1/13/97)               *           
*---------------------------------------------------------------------*         
OFFTAB   DS    0H                                                               
         DC    CL2'11',XL1'00'                                                  
         DC    CL2'12',CL1'!'                                                   
         DC    CL2'11',CL1'#'                                                   
         DC    CL2'11',XL1'89'                                                  
         DC    CL2'6A',CL1'A'                                                   
         DC    CL2'6B',CL1'B'                                                   
         DC    CL2'6C',CL1'C'                                                   
         DC    CL2'6D',CL1'D'                                                   
         DC    CL2'6E',CL1'E'                                                   
         DC    CL2'6F',CL1'F'                                                   
         DC    CL2'33',CL1'G'                                                   
         DC    CL2'6H',CL1'H'                                                   
         DC    CL2'6J',CL1'J'                                                   
         DC    CL2'6K',CL1'K'                                                   
         DC    CL2'6L',CL1'L'                                                   
         DC    CL2'6M',CL1'M'                                                   
         DC    CL2'11',CL1'N'                                                   
         DC    CL2'11',CL1'O'                                                   
         DC    CL2'2P',CL1'P'                                                   
         DC    CL2'1Q',CL1'Q'                                                   
         DC    CL2'1M',CL1'R'                                                   
         DC    CL2'11',CL1'S'                                                   
         DC    CL2'1T',CL1'T'                                                   
         DC    CL2'1U',CL1'U'                                                   
         DC    CL2'1V',CL1'V'                                                   
         DC    CL2'5W',CL1'W'                                                   
         DC    CL2'1X',CL1'X'                                                   
         DC    CL2'1Y',CL1'Y'                                                   
         DC    CL2'11',CL1'Z'                                                   
         DC    CL2'11',CL1'1'                                                   
         DC    CL2'22',CL1'2'                                                   
         DC    CL2'33',CL1'3'                                                   
         DC    CL2'34',CL1'4'                                                   
         DC    CL2'A5',CL1'5'                                                   
         DC    CL2'A6',CL1'6'                                                   
         DC    CL2'A7',CL1'7'                                                   
         DC    CL2'48',CL1'8'                                                   
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
**PAN#1  DC    CL21'028PPREPFXOBS01/30/97'                                      
         END                                                                    
