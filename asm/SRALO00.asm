*          DATA SET SRALO00    AT LEVEL 005 AS OF 05/01/02                      
*PHASE T12700A                                                                  
         TITLE '$PLOG/ALOG - DISPLAY/ALTER LINE LOGGING STATUS'                 
         PRINT NOGEN                                                            
ALOG     CSECT                                                                  
         NMOD1 9,*$ALOG**                                                       
         USING WRKD,RC                                                          
         LR    R2,R1                                                            
         USING SRPARMD,R2          R2=A(S/R PARAM LIST)                         
         L     RA,SRPARM1                                                       
         USING SYSFACD,RA          RA=A(SYS FAC LIST)                           
*                                                                               
         L     RE,VSSB             EXIT IF VTAM APPLICATION                     
         CLI   SSBVTID-SSBD(RE),C' '                                            
         BH    EXIT                                                             
*                                                                               
         L     R3,SRPARM6                                                       
         USING SRALOFFD,R3         R3=A(TWA)                                    
         NI    SRVIDH+6,OI0C                                                    
         XC    MSG,MSG                                                          
         CLI   SRVID+1,C'P'                                                     
         BE    PL                                                               
         CLI   SRVID+1,C'A'                                                     
         BE    AL                                                               
         DC    H'0'                                                             
         EJECT                                                                  
PL       BAS   RE,SETBXLE                                                       
         USING DECBD,R7            R7=A(DECB LIST ENTRY)                        
         LA    R4,SRVP1H                                                        
         USING FLDHDRD,R4                                                       
         BAS   RE,INPUT            HAS A PARAM BEEN INPUT                       
         BE    PL2                 NO                                           
         CLI   FLDILEN,3           YES MUST BE A 3 OR 4 CHR LINE ID             
         BNE   *+12                                                             
         MVI   FLDILEN,4                                                        
         MVI   FLDDATA+3,C' '                                                   
         CLI   FLDILEN,4                                                        
         BNE   ERR2                                                             
PL1      CLC   FLDDATA(4),DECBID                                                
         BE    PL2                 START DISPLAY FROM INPUT LINE ID             
         BXLE  R7,R8,PL1                                                        
         B     ERR2                                                             
         SPACE 2                                                                
PL2      LA    R4,SRVL1H           R4=A(SCR LINE INFO FLD HDR)                  
PL3      MVI   FLDDATA,C'+'                                                     
         TM    DECBSTA2,X'80'                                                   
         BZ    *+8                                                              
         MVI   FLDDATA,C'-'                                                     
         MVC   FLDDATA+1(4),DECBID                                              
         OI    FLDOIND,OI1T                                                     
         SR    R6,R6                                                            
         SPACE 2                                                                
         IC    R6,FLDLEN           BUMP TO NEXT SCR ENTRY                       
         AR    R4,R6                                                            
         CLI   FLDLEN,9                                                         
         BE    PLX                                                              
         CLI   FLDLEN,0                                                         
         BE    PLX                                                              
         BXLE  R7,R8,PL3           BUMP TO NEXT DECB LIST ENTRY                 
         SPACE 2                                                                
PLX      CLI   SRVID+1,C'A'        DISPLAY AFTER $AL                            
         BE    ALX                 YES                                          
         MVC   MSG(34),=C'LOGGING STATUS DISPLAYED - ALTER ?'                   
         FOUT  SRVMSGH,MSG                                                      
         FOUT  SRVIDH,=C'$ALOG'                                                 
         SPACE 2                                                                
         LA    R4,SRVL1H                                                        
         OI    FLDOIND,OI1C        POSN CURSOR TO 1ST LINE STATUS FLD           
         B     EXIT                                                             
         EJECT                                                                  
AL       BAS   RE,SETBXLE                                                       
         USING DECBD,R7            R7=A(DECB LIST ENTRY)                        
         LA    R4,SRVP1H                                                        
         USING FLDHDRD,R4                                                       
         BAS   RE,INPUT            HAS A PARAM BEEN INPUT                       
         BE    AL0                 NO                                           
         CLI   FLDILEN,3           YES MUST BE A 3 OR 4 CHR LINE ID             
         BNE   *+12                                                             
         MVI   FLDILEN,4                                                        
         MVI   FLDDATA+3,C' '                                                   
         CLI   FLDILEN,4                                                        
         BNE   ERR2                                                             
         CLC   FLDDATA(4),=C'+ALL' OR OPEN ALL OPTION                           
         BE    ALL                                                              
         CLC   FLDDATA(4),=C'-ALL' OR CLOSE ALL OPTION                          
         BE    ALL                                                              
         CLC   FLDDATA(4),DECBID                                                
         BE    AL0                                                              
         BXLE  R7,R8,*-10                                                       
         B     ERR2                                                             
         SPACE 2                                                                
AL0      LA    R4,SRVL1H           R4=A(SCR LINE INFO FLD HDR)                  
AL0A     BAS   RE,INPUT            HAS LINE INFO BEEN INPUT                     
         BE    ALA                 NO                                           
         CLI   FLDDATA,C'+'        1ST CHR LINE STATUS                          
         BE    AL2                                                              
         CLI   FLDDATA,C'-'                                                     
         BNE   ERR1                                                             
AL2      CLI   FLDILEN,4           NEXT 4 CHRS LINE ID                          
         BNE   *+12                                                             
         MVI   FLDILEN,5                                                        
         MVI   FLDDATA+4,C' '                                                   
         CLI   FLDILEN,5                                                        
         BL    ERR2                                                             
         BAS   RE,SETBXLE                                                       
AL3      CLC   FLDDATA+1(4),DECBID                                              
         BE    AL4                                                              
         BXLE  R7,R8,AL3                                                        
         B     ERR2                                                             
AL4      EQU   *                                                                
         CLI   FLDDATA,C'-'                                                     
         BE    AL7                                                              
         SPACE 2                                                                
AL6      NI    DECBSTA2,X'7F'    SET TO LOG                                     
         B     ALA                                                              
*                                                                               
AL7      OI    DECBSTA2,X'80'    SET NO LOGGING                                 
         B     ALA                                                              
         SPACE 2                                                                
         EJECT                                                                  
ALL      L     R1,DECBDTF          ALL LINES OPTION                             
         CLI   FLDDATA,C'-'                                                     
         BE    ALL2                                                             
         SPACE 2                                                                
ALL1     NI    DECBSTA2,X'7F'    SET TO LOG                                     
         B     ALLA                                                             
*                                                                               
ALL2     OI    DECBSTA2,X'80'    SET NO LOGGING                                 
         SPACE 2                                                                
ALLA     BXLE  R7,R8,ALL           BUMP TO NEXT DECB ENTRY                      
         BAS   RE,SETBXLE          DISPLAY NEW LINE STATUS                      
         B     PL2                                                              
         SPACE 2                                                                
ALA      SR    R1,R1               BUMP TO NEXT SCR ENTRY                       
         IC    R1,FLDLEN                                                        
         AR    R4,R1                                                            
         CLI   FLDLEN,9                                                         
         BE    ALX                                                              
         CLI   FLDLEN,0                                                         
         BE    ALX                                                              
         B     AL0A                                                             
         SPACE 2                                                                
ALX      MVC   MSG(22),=C'LOGGING STATUS ALTERED'                               
         FOUT  SRVMSGH,MSG                                                      
         FOUT  SRVIDH,=C'$PLOG'                                                 
         SPACE 2                                                                
         LA    R4,SRVIDH                                                        
         OI    FLDOIND,OI1C        POSN CURSOR TO ID                            
         L     R6,SRPARM3                                                       
         USING UTLD,R6                                                          
         MVC   TSVCREQ,=X'01EE'    END OF S/R $BYE                              
         B     EXIT                                                             
         EJECT                                                                  
ERR1     MVC   MSG(25),=C'LOGGING STATUS NOT + OR -'                            
         B     ERRX                                                             
ERR2     MVC   MSG(15),=C'LINE ID INVALID'                                      
         B     ERRX                                                             
ERR3     MVC   MSG(19),=C'LINE SYSNUM INVALID'                                  
         B     ERRX                                                             
ERRNONO  MVC   MSG(29),=C'LINE NOT COMPATIBLE WITH LINE'                        
         MVC   MSG+30(4),DECBID                                                 
         B     ERRX                                                             
         SPACE 2                                                                
ERRX     MVC   SRVMSG(12),=C'***ERROR*** '                                      
         MVC   SRVMSG+12(48),MSG                                                
         FOUT  SRVMSGH                                                          
         OI    FLDOIND,OI1C        POSN CURSOR TO ERROR FLD                     
         SPACE 2                                                                
EXIT     XMOD1 1                                                                
         EJECT                                                                  
INPUT    SR    R0,R0               SET COND CODE TO = IF NO INPUT               
         IC    R0,FLDILEN                                                       
         LTR   R0,R0                                                            
         BR    RE                                                               
         SPACE 2                                                                
SETBXLE  L     R7,VDECBLST         SET R7,R8,R9 FOR DECB LIST                   
         LH    R8,0(R7)                                                         
         L     R9,2(R7)                                                         
         LA    R7,6(R7)                                                         
         BR    RE                                                               
         SPACE 2                                                                
         LTORG                                                                  
         SPACE 2                                                                
WRKD     DSECT                                                                  
DUB      DS    D                                                                
MSG      DS    CL60                                                             
         EJECT                                                                  
* DDFLDING                                                                      
       ++INCLUDE DDFLDIND                                                       
* DDFLDHDR                                                                      
       ++INCLUDE DDFLDHDR                                                       
         EJECT                                                                  
* FADSECTS                                                                      
       ++INCLUDE FADSECTS                                                       
         EJECT                                                                  
SRALOFFD DSECT                                                                  
         DS    CL64                                                             
* SRALOFFD                                                                      
       ++INCLUDE SRALOFFD                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'005SRALO00   05/01/02'                                      
         END                                                                    
