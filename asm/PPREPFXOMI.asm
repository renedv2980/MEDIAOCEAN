*          DATA SET PPREPFXOMI AT LEVEL 002 AS OF 02/27/97                      
*                                                                               
*     (2/26/97 CONVERTS MCKIM (MI) ACC OFFICES)                                 
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
         CLC   PCLTACCA,=C'S4'                                                  
         BNE   AGYC8                                                            
         CLC   PCLTAOFC,=C'WA'                                                  
         BNE   AGYC8                                                            
*                                  BYPASS WA/S4 ACC OFFICE/ACC AGENCY           
         MVC   P+1(3),PCLTKAGY     "OLD" DATA                                   
         MVC   P+6(3),PCLTKCLT                                                  
         MVC   P+13(1),PCLTOFF                                                  
         MVC   P+20(2),PCLTAOFC                                                 
         MVC   P+29(2),PCLTACCA                                                 
         MVC   P+36(33),=C'*** THIS CLIENT NOT CONVERTED ***'                   
         BAS   RE,RPRT                                                          
         B     AGYC3               NEXT RECORD                                  
*                                                                               
AGYC8    LA    R1,OFFTAB                                                        
         LA    R0,OFFTABN          NUMBER OF TABLE ENTRIES                      
*                                                                               
         MVC   OFCTEST,PCLTAOFC                                                 
         CLI   PCLTAOFC,C' '       ACC OFFICE PRESENT ?                         
         BH    LOOP                YES - USE IT FOR SEARCH                      
         MVC   OFCTEST,PCLTOFF     NO - USE MEDIA OFFICE                        
*                                                                               
LOOP     CLC   OFCTEST,2(R1)                                                    
         BL    ERR                                                              
         BE    PRT                                                              
         LA    R1,3(R1)                                                         
         BCT   R0,LOOP                                                          
*                                                                               
ERR      MVC   P(26),=C'COULDN''T CONVERT MI CLIENT'                            
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
*******  XC    PCLTACCA,PCLTACCA                                                
         MVC   PCLTAOFC,0(R1)   SET PCLTAOFC FROM OFFTAB                        
*******  MVC   PCLTACCA,=C'MI'  NO ACC AGENCY REQUIRED                          
         MVC   P+37(1),PCLTOFF     "NEW" DATA                                   
         MVC   P+44(2),PCLTAOFC                                                 
         MVC   P+53(2),PCLTACCA                                                 
*                                                                               
PRT5     CLI   RCWRITE,C'N'                                                     
         BE    PRT10                                                            
         GOTO1 PUTPRT                                                           
*                                                                               
PRT10    DS    0H                                                               
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
*        OFFICE CONVERSION TABLE - MKTO  (MI)    (2/26/97)            *         
*---------------------------------------------------------------------*         
OFFTAB   DS    0H                                                               
*****    DC    CL2'  ',XL1'00'                                                  
         DC    CL2'GP',CL1'!'                                                   
         DC    CL2'GW',XL1'5E'                                                  
         DC    CL2'GE',CL1'/'                                                   
         DC    CL2'BM',CL1'A'                                                   
         DC    CL2'ZB',CL1'B'                                                   
         DC    CL2'BC',CL1'C'                                                   
         DC    CL2'BR',CL1'D'                                                   
         DC    CL2'BS',CL1'E'                                                   
         DC    CL2'ZS',CL1'F'                                                   
         DC    CL2'MW',CL1'G'                                                   
         DC    CL2'BM',CL1'H'                                                   
         DC    CL2'ZW',CL1'I'                                                   
         DC    CL2'CA',CL1'J'                                                   
         DC    CL2'BK',CL1'K'                                                   
         DC    CL2'CA',CL1'L'                                                   
         DC    CL2'ZM',CL1'M'                                                   
         DC    CL2'ZN',CL1'N'                                                   
         DC    CL2'ZD',CL1'O'                                                   
         DC    CL2'ZV',CL1'P'                                                   
         DC    CL2'ZQ',CL1'Q'                                                   
         DC    CL2'ZR',CL1'R'                                                   
         DC    CL2'GC',CL1'\'                                                   
         DC    CL2'BS',CL1'S'                                                   
         DC    CL2'BT',CL1'T'                                                   
         DC    CL2'MW',CL1'U'                                                   
         DC    CL2'BV',CL1'V'                                                   
         DC    CL2'ZO',CL1'W'                                                   
         DC    CL2'GS',CL1'X'                                                   
         DC    CL2'BY',CL1'Y'                                                   
         DC    CL2'BZ',CL1'Z'                                                   
         DC    CL2'BP',CL1'1'                                                   
         DC    CL2'BA',CL1'2'                                                   
         DC    CL2'CC',CL1'3'                                                   
         DC    CL2'UC',CL1'4'                                                   
         DC    CL2'GA',CL1'5'                                                   
         DC    CL2'ZP',CL1'6'                                                   
         DC    CL2'DM',CL1'7'                                                   
         DC    CL2'OB',CL1'8'                                                   
         DC    CL2'99',CL1'9'                                                   
         DC    X'FF'                                                            
OFFTABN  EQU   (*-OFFTAB)/3                                                     
PP02WRKD DSECT                                                                  
ELCODE   DS    X                                                                
OFCTEST  DS    C                                                                
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE PPNEWFILE                                                      
       ++INCLUDE PPREPWORK                                                      
       ++INCLUDE PPMODEQU                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002PPREPFXOMI02/27/97'                                      
         END                                                                    
