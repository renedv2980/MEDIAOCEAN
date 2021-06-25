*          DATA SET PPREPFXOBD AT LEVEL 024 AS OF 03/22/96                      
         TITLE 'PP0202 - PRTFIX PROGRAM TO FIX OFFICE CODES'                    
*PHASE PP0202D,+0,NOAUTO                                                        
*INCLUDE MININAM                                                                
*                                                                               
*        CONVERT BDNY (BD) ACC OFFICE CODES                                     
*                                                                               
*        QOPT1  X=  CLEAR PCLTACCA AND REST PCLTAOFC                            
*                                                                               
*                  (USE THIS OPTION IF CONVERSION FAILED AND                    
*                   WE NEED TO GO BACK)                                         
*                                                                               
*        QOPT5  N= DON'T MARK FILE (EVEN IF WRITE=YES)                          
*                                                                               
*                                                                               
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
ERR      DS    0H                                                               
*****    B     AGYC3            NOT IN OFFTAB - JUST SKIP                       
*                                                                               
         MVC   P(25),=C'COULDNT CONVERT BD CLIENT'                              
         MVC   P+27(3),PCLTKAGY                                                 
         MVC   P+31(3),PCLTKCLT                                                 
         MVC   P+35(7),=C'OLD OFC'                                              
         MVC   P+43(1),PCLTOFF                                                  
         GOTO1 HEXOUT,DMCB,PCLTOFF,P+46,1                                       
         BAS   RE,RPRT                                                          
         B     AGYC3                                                            
*                                                                               
PRT      DS    0H                                                               
         XC    PCLTACCA,PCLTACCA                                                
         MVC   PCLTAOFC,=X'0040'    RESET PCLTAOFC TO "NULL-BLANK"              
         CLI   PCLTOFF,X'00'                                                    
         BE    PRT2                                                             
         MVC   PCLTAOFC,=C'  '      RESET PCLTAOFC TO BLANK                     
         CLI   PCLTOFF,C' '                                                     
         BE    PRT2                                                             
         MVC   PCLTAOFC,=C'1 '      RESET PCLTAOFC TO 1                         
         CLI   PCLTOFF,C'1'                                                     
         BE    PRT2                                                             
         MVC   PCLTAOFC,=C'2 '      RESET PCLTAOFC TO 2                         
         CLI   PCLTOFF,C'2'                                                     
         BE    PRT2                                                             
         MVC   PCLTAOFC,=C'3 '      RESET PCLTAOFC TO 3                         
         CLI   PCLTOFF,C'3'                                                     
         BE    PRT2                                                             
         MVC   PCLTAOFC,=C'4 '      RESET PCLTAOFC TO 4                         
         CLI   PCLTOFF,C'4'                                                     
         BE    PRT2                                                             
         MVC   PCLTAOFC,=C'CA'      RESET PCLTAOFC TO CA                        
         CLI   PCLTOFF,C'5'                                                     
         BE    PRT2                                                             
         MVC   PCLTAOFC,=C'AA'      RESET PCLTAOFC TO AA                        
         CLI   PCLTOFF,C'6'                                                     
         BE    PRT2                                                             
         MVC   PCLTAOFC,=C'7 '      RESET PCLTAOFC TO 7                         
         CLI   PCLTOFF,C'7'                                                     
         BE    PRT2                                                             
         MVC   PCLTAOFC,=C'8 '      RESET PCLTAOFC TO 8                         
         CLI   PCLTOFF,C'8'                                                     
         BE    PRT2                                                             
         MVC   PCLTAOFC,=C'9 '      RESET PCLTAOFC TO 9                         
         CLI   PCLTOFF,C'9'                                                     
         BE    PRT2                                                             
         MVC   PCLTAOFC,=C'A '      RESET PCLTAOFC TO A                         
         CLI   PCLTOFF,C'A'                                                     
         BE    PRT2                                                             
         MVC   PCLTAOFC,=C'B '      RESET PCLTAOFC TO B                         
         CLI   PCLTOFF,C'B'                                                     
         BE    PRT2                                                             
         MVC   PCLTAOFC,=C'CA'      RESET PCLTAOFC TO CA                        
         CLI   PCLTOFF,C'C'                                                     
         BE    PRT2                                                             
         MVC   PCLTAOFC,=C'D '      RESET PCLTAOFC TO D                         
         CLI   PCLTOFF,C'D'                                                     
         BE    PRT2                                                             
         MVC   PCLTAOFC,=C'E '      RESET PCLTAOFC TO E                         
         CLI   PCLTOFF,C'E'                                                     
         BE    PRT2                                                             
         MVC   PCLTAOFC,=C'F '      RESET PCLTAOFC TO F                         
         CLI   PCLTOFF,C'F'                                                     
         BE    PRT2                                                             
         MVC   PCLTAOFC,=C'G '      RESET PCLTAOFC TO G                         
         CLI   PCLTOFF,C'G'                                                     
         BE    PRT2                                                             
         MVC   PCLTAOFC,=C'I '      RESET PCLTAOFC TO I                         
         CLI   PCLTOFF,C'I'                                                     
         BE    PRT2                                                             
         MVC   PCLTAOFC,=C'L '      RESET PCLTAOFC TO L                         
         CLI   PCLTOFF,C'L'                                                     
         BE    PRT2                                                             
         MVC   PCLTAOFC,=C'AA'      RESET PCLTAOFC TO AA                        
         CLI   PCLTOFF,C'S'                                                     
         BE    PRT2                                                             
         MVC   PCLTAOFC,=C'T '      RESET PCLTAOFC TO T                         
         CLI   PCLTOFF,C'T'                                                     
         BE    PRT2                                                             
         MVC   PCLTAOFC,=C'X '      RESET PCLTAOFC TO X                         
         CLI   PCLTOFF,C'X'                                                     
         BE    PRT2                                                             
         DC    H'0'                UNKNOWN OFFICE                               
*                                                                               
PRT2     CLI   QOPT1,C'X'           SEE IF UNDOING CONVERSION                   
         BE    PRT5                                                             
*                                                                               
         MVC   PCLTAOFC,0(R1)                                                   
*****    MVC   PCLTACCA,=C'BP'       SET ACC AGENCY TO BP                       
*                                                                               
PRT5     DS    0H                                                               
         CLI   RCWRITE,C'N'                                                     
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
*        OFFICE CONVERSION TABLE - BDNY (BD)                          *         
*---------------------------------------------------------------------*         
OFFTAB   DS    0H                                                               
         DC    CL2'NA',XL1'00'                                                  
         DC    CL2'NA',CL1' '                                                   
         DC    CL2'DA',CL1'A'                                                   
         DC    CL2'NA',CL1'B'                                                   
         DC    CL2'CA',CL1'C'                                                   
         DC    CL2'DB',CL1'D'                                                   
         DC    CL2'NA',CL1'E'                                                   
         DC    CL2'WB',CL1'F'                                                   
         DC    CL2'NA',CL1'G'                                                   
         DC    CL2'NA',CL1'I'                                                   
         DC    CL2'WA',CL1'L'                                                   
         DC    CL2'AA',CL1'S'                                                   
         DC    CL2'NA',CL1'T'                                                   
         DC    CL2'NA',CL1'X'                                                   
         DC    CL2'NA',CL1'1'                                                   
         DC    CL2'MA',CL1'2'                                                   
         DC    CL2'DB',CL1'3'                                                   
         DC    CL2'WA',CL1'4'                                                   
         DC    CL2'CA',CL1'5'                                                   
         DC    CL2'AA',CL1'6'                                                   
         DC    CL2'WB',CL1'8'                                                   
         DC    CL2'NA',CL1'9'                                                   
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
**PAN#1  DC    CL21'024PPREPFXOBD03/22/96'                                      
         END                                                                    
