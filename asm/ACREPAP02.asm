*          DATA SET ACREPAP02  AT LEVEL 054 AS OF 05/01/02                      
*PHASE ACAP02A,*                                                                
         TITLE 'ACAP02 - LIST APG SUPERLEDGER - ROOT'                           
ACAP02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*ACAP02*                                                       
         L     RA,0(R1)                                                         
         USING ACWORKD,RA                                                       
         CLI   MODE,REQFRST                                                     
         BNE   REQLST                                                           
         MVI   FORCEHED,C'Y'                                                    
         MVC   PAGE,=H'1'                                                       
         ZAP   LINESWK,=P'1'                                                    
         L     RF,VEXTRAS                                                       
         USING RUNXTRAD,RF                                                      
         L     RF,ADMASTD                                                       
         USING MASTD,RF                                                         
         L     RF,MCBXAREA                                                      
         L     R7,=A(ADBOX)                                                     
         ST    RF,0(R7)                                                         
         L     RF,=A(HOOK)                                                      
         ST    RF,HEADHOOK                                                      
         L     RF,=A(SAVERC)                                                    
         ST    RC,0(RF)                                                         
         SPACE 1                                                                
         B     XIT                                                              
REQLST   CLI   MODE,REQLAST                                                     
         BNE   PRCACC                                                           
         MVC   P+1(18),=C'* END OF LISTING *'                                   
         BAS   RE,PRT                                                           
         B     XIT                                                              
PRCACC   CLI   MODE,PROCACC                                                     
         BNE   XIT                                                              
         L     R6,ADACC                                                         
         MVC   ACNM,SPACES                                                      
         MVC   P+1(12),3(R6)                                                    
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         BNE   GET15                                                            
         USING ACNAMED,R6                                                       
         ZIC   R1,ACNMLEN                                                       
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   ACNM(0),ACNMNAME                                                 
         MVC   CHOP,SPACES                                                      
         GOTO1 CHOPPER,DMCB,(36,ACNM),(23,CHOP),(0,2)                           
         MVC   P+14(23),CHOP                                                    
GET15    MVI   ELCODE,X'15'                                                     
         L     R6,ADACC                                                         
         BAS   RE,GETEL                                                         
         BE    FILPT                                                            
         CLC   CHOP+23(23),SPACES                                               
         BE    NONO                                                             
         CLC   P+1(13),SPACES                                                   
         BE    NONO                                                             
         CP    LINESWK,=P'46'                                                   
         BL    *+14                                                             
         MVI   FORCEHED,C'Y'                                                    
         ZAP   LINESWK,=P'1'                                                    
         BAS   RE,PRT                                                           
NONO     BAS   RE,PRT                                                           
         BAS   RE,PRT                                                           
         B     XIT                                                              
FILPT    LA    R3,P+45                                                          
         LA    R7,MYWORK                                                        
         USING ACGENLD,R6                                                       
         MVC   MYWORK,SPACES                                                    
         MVC   MYWORK(14),ACGLACC                                               
         CLI   ACGLLEN,X'1A'                                                    
         BE    UNSPACC                                                          
         MVC   MYWORK+14(14),ACGLACC2                                           
UNSPACC  BAS   RE,APGUNSPL                                                      
         LA    R3,P+81                                                          
         LA    R7,MYWORK                                                        
         MVC   MYWORK,SPACES                                                    
         CLI   ACGLLEN,X'28'                                                    
         BNH   MVFILT                                                           
         MVC   MYWORK(14),ACGLCON                                               
         CLI   ACGLLEN,X'36'                                                    
         BE    UNSPCON                                                          
         MVC   MYWORK+14(14),ACGLCON2                                           
UNSPCON  BAS   RE,APGUNSPL                                                      
MVFILT   MVC   P+74(5),ACGLFLT                                                  
         CLI   ACGLBTYP,C' '       ANY BUCKET TYPE?                             
         BNH   *+10                NOT                                          
         MVC   P+108(1),ACGLBTYP   MOVE IN BUCKET TYPE                          
*                                                                               
         MVC   P+38(6),=C'+ADD+ '                                               
         CLI   ACGLACT,0           DEFAULT ACTION IS TO ADD                     
         BE    PRTIT                                                            
         MVC   P+38(6),=C'-SUB- '                                               
         CLI   ACGLACT,C'-'        SUBTRACT?                                    
         BE    PRTIT                                                            
         MVC   P+38(6),=C'%PCT% '                                               
         CLI   ACGLACT,C'%'        PERCENT?                                     
         BE    PRTIT                                                            
         MVC   P+38(6),=C'/DIV/ '                                               
         CLI   ACGLACT,C'/'        DIVIDE?                                      
         BE    PRTIT                                                            
         MVC   P+38(6),=C'-???- '  NO SUCH ACTION , JACKSON                     
PRTIT    BAS   RE,PRT                                                           
         MVI   ELCODE,X'15'                                                     
         BAS   RE,NEXTEL                                                        
         BE    FILPT                                                            
         CLC   CHOP+23(23),SPACES                                               
         BE    *+8                                                              
         BAS   RE,PRT                                                           
PRT1     BAS   RE,PRT                                                           
         B     XIT                                                              
         EJECT                                                                  
PRT      NTR1                                                                   
         CLC   CHOP+23(23),SPACES                                               
         BE    GOPRT                                                            
         CLC   P+14(23),SPACES                                                  
         BNE   CKLINE                                                           
         MVC   P+14(23),CHOP+23                                                 
         MVC   CHOP,SPACES                                                      
         B     GOPRT                                                            
CKLINE   CP    LINESWK,=P'46'                                                   
         BL    *+14                                                             
         MVI   FORCEHED,C'Y'                                                    
         ZAP   LINESWK,=P'1'                                                    
GOPRT    CLI   FORCEHED,C'Y'                                                    
         BNE   GOPRT2                                                           
         CLC   P,SPACES                                                         
         BE    XIT                                                              
GOPRT2   GOTO1 ACREPORT                                                         
         AP    LINESWK,=P'1'                                                    
         CP    LINESWK,=P'46'                                                   
         BNH   XIT                                                              
         ZAP   LINESWK,=P'1'                                                    
         MVI   FORCEHED,C'Y'                                                    
XIT      XIT1                                                                   
         SPACE 1                                                                
MYWORK   DS    CL28                                                             
         GETEL R6,DATADISP,ELCODE                                               
ELCODE   DS    XL1                                                              
         LTORG                                                                  
         EJECT                                                                  
APGUNSPL NTR1                      UNSPLIT ACC/CON FOR SCREEN DISPLAY           
*                                  R3 CONTAINS SCREEN FIELD                     
*                                  R7 CONTAINS DSECT FIELD                      
*                                  ROUTINE STICKS A DASH BETWEEN FIELDS         
         ST    R7,SV7                                                           
         MVI   SW2,C'1'                                                         
APGU0    LA    R5,14                                                            
         BAS   RE,REVERSSC                                                      
APGU1    CLI   0(R7),X'40'                                                      
         BE    APGU1A                                                           
         CLI   0(R7),X'FF'         SUBSTITUTE FOR BLANK                         
         BNE   *+8                                                              
         MVI   0(R7),X'40'         REMOVE IT                                    
         MVC   0(1,R3),0(R7)                                                    
         LA    R7,1(R7)                                                         
         LA    R3,1(R3)                                                         
         BCT   R5,APGU1                                                         
APGU1A   CLI   SW2,C'2'            SECOND PASS THRU                             
         BE    XIT                                                              
         L     R7,SV7                                                           
         CLI   14(R7),X'40'        EMPTY TO FIELD                               
         BE    XIT                                                              
         MVI   0(R3),C'-'                                                       
         MVI   SW2,C'2'                                                         
         LA    R3,1(R3)                                                         
         LA    R7,14(R7)                                                        
         B     APGU0                                                            
         SPACE 3                                                                
REVERSSC NTR1                      REVERSE SCAN FOR BLANKS                      
*                                  SO DISPLAY DOESN'T TERMINATE                 
*                                  AT EMBEDDED SPACES.  ROUTINE                 
*                                  SUSTITUTES X'FF'S.                           
         LA    R7,13(R7)                                                        
REV1     CLI   0(R7),X'40'                                                      
         BNE   REV2                                                             
         BCTR  R7,0                                                             
         C     R7,SV7                                                           
         BE    XIT                                                              
         B     REV1                                                             
         SPACE 1                                                                
REV2     BCTR  R7,0                                                             
         C     R7,SV7                                                           
         BE    XIT                                                              
         CLI   0(R7),X'40'                                                      
         BNE   REV2                                                             
         MVI   0(R7),X'FF'                                                      
         B     REV2                                                             
         EJECT                                                                  
HOOK     NMOD1 0,*HOOK*                                                         
         L     RC,SAVERC                                                        
         L     R4,ADBOX                                                         
         USING BOXD,R4                                                          
         MVC   MYROW,SPACES                                                     
         MVC   MYCOL,SPACES                                                     
         MVI   MYCOL,C'L'                                                       
         MVI   MYROW+5,C'T'                                                     
         MVI   MYROW+8,C'M'                                                     
         MVI   MYROW+55,C'B'                                                    
         MVI   MYCOL+37,C'C'                                                    
         MVI   MYCOL+43,C'C'                                                    
         MVI   MYCOL+73,C'C'                                                    
         MVI   MYCOL+79,C'C'                                                    
         MVI   MYCOL+107,C'C'                                                   
         MVI   MYCOL+109,C'R'                                                   
         MVC   BOXCOLS,MYCOL                                                    
         MVC   BOXROWS,MYROW                                                    
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXWT,1                                                          
         MVI   BOXINIT,0                                                        
         XMOD1 1                                                                
SAVERC   DC    A(0)                                                             
ADBOX    DS    A                                                                
SV7      DS    F                                                                
SW2      DC    C'1'                                                             
ACNM     DS    CL36                                                             
CHOP     DS    CL46                                                             
LINESWK  DC    PL2'0'                                                           
MYCOL    DS    CL132                                                            
MYROW    DS    CL100                                                            
         EJECT                                                                  
         PRINT   OFF                                                            
       ++INCLUDE ACGENMODES                                                     
       ++INCLUDE ACGENBOTH                                                      
       ++INCLUDE ACREPWORKD                                                     
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE DDREPXTRAD                                                     
       ++INCLUDE DDREPMASTD                                                     
         PRINT   ON                                                             
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'054ACREPAP02 05/01/02'                                      
         END                                                                    
