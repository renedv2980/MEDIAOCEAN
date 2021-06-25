*          DATA SET YYUNPQT    AT LEVEL 164 AS OF 10/01/01                      
*PHASE YYUNPQTA                                                                 
*INCLUDE DMDMGRL                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE PRNTBL                                                                 
*                                                                               
         TITLE 'YYUNPQT - TEST PROGRAM TO WRITE TO PQ'                          
YYUNPQT  CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE 0,*YYUNPQT,=A(R13CHAIN)                                          
*                                                                               
         ENTRY UTL                 FOR DATAMGR                                  
         ENTRY SSB                                                              
*                                                                               
         L     RA,=V(CPRINT)                                                    
         USING DPRINT,RA                                                        
*                                                                               
         L     RF,=A(SSB)          SET DSPACE=T                                 
         MVI   SSODSPAC-SSOOFF(RF),C'T'                                         
*                                                                               
         XC    R,R                 PRINT LINE FOR PRTQUE CALLS                  
         LA    R2,R                                                             
         USING PQPLD,R2                                                         
         MVC   QLSRCID,=H'17'      USERID 'SJR'                                 
         MVI   QLEXTRA,X'FF'       OPEN PRINT QUEUE REPORT                      
         MVC   QLSUBID,=C'BDF'                                                  
         MVI   QLCLASS,C'G'        MUST BE CLASS "G" REPORT                     
         MVI   QLLINET,X'C0'                                                    
         MVI   QLLINEW,132                                                      
         MVC   QLRETNL,=H'36'                                                   
         MVC   QLRETND,=H'2'                                                    
         MVC   QLDESC,=C'STARCOM BDE'                                           
*                                                                               
         L     RE,=A(CIREC)                                                     
         XCEFL (RE),14336          CLEAR PQ C/I BUFFER                          
*                                                                               
         GOTO1 =V(DATAMGR),DMCB,=C'DMPRINT',=C'PRTQUE',0,R,A(CIREC)             
         CLI   DMCB+8,0                                                         
         BE    YPQT20                                                           
*                                                                               
         MVC   P+11(13),=C'*** ERROR ***'                                       
         MVC   P+30(28),=C'COULD NOT OPEN PRTQUE REPORT'                        
         GOTO1 =V(PRINTER)                                                      
         B     YPQTX                                                            
*                                                                               
YPQT20   DS    0H                                                               
         MVC   P(L'QLSRCID),QLSRCID                                             
         MVC   P+10(L'QLSUBID),QLSUBID                                          
         MVC   P+20(L'QLREPRNO),QLREPRNO                                        
         MVC   P+30(L'QLREPRCI),QLREPRCI                                        
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         MVI   R,X'89'             SET SKIP TO TOP OF PAGE                      
         MVC   R+1(132),SPACES                                                  
         GOTO1 =V(DATAMGR),DMCB,=C'DMPRINT',=C'PRTQUE',0,R,A(CIREC)             
         CLI   DMCB+8,0                                                         
         BNE   YPQTERR                                                          
*                                                                               
*1ST RECORD: HEADER WITH EDICT RECORD KEY                                       
         MVI   R,X'09'             SET SINGLE SPACE WRITE                       
         MVC   R+1(132),SPACES                                                  
         MVC   R+5(11),=C'*HDR*EDICT='                                          
         MVC   R+16(8),=CL8'YYUN'         8-BYTE EDICT RECORD NAME              
         GOTO1 =V(DATAMGR),DMCB,=C'DMPRINT',=C'PRTQUE',0,R,A(CIREC)             
         CLI   DMCB+8,0                                                         
         BNE   YPQTERR                                                          
*                                                                               
*2ND RECORD: ++DDS CARD FOR "SUB" - "SUBJECT" FIELD IN BDE CUI                  
         MVI   R,X'09'             SET SINGLE SPACE WRITE                       
         MVC   R+1(132),SPACES                                                  
         MVC   R+1(14),=C'++DDS      SUB'                                       
         MVC   R+16(60),=CL60'MY SUBJECT'                                       
         GOTO1 =V(DATAMGR),DMCB,=C'DMPRINT',=C'PRTQUE',0,R,A(CIREC)             
         CLI   DMCB+8,0                                                         
         BNE   YPQTERR                                                          
*                                                                               
*3RD RECORD: ++DDS CARD FOR "FIL" - "DOC NAME" FIELD IN BDE CUI                 
         MVI   R,X'09'             SET SINGLE SPACE WRITE                       
         MVC   R+1(132),SPACES                                                  
         MVC   R+1(14),=C'++DDS      FIL'                                       
         MVC   R+16(60),=CL60'MY DOC NAME'                                      
         GOTO1 =V(DATAMGR),DMCB,=C'DMPRINT',=C'PRTQUE',0,R,A(CIREC)             
         CLI   DMCB+8,0                                                         
         BNE   YPQTERR                                                          
*                                                                               
*4TH RECORD: ++DDS CARD FOR "DSN" - MVS DATASET NAME TO BE TRANSFERRED          
         MVI   R,X'09'             SET SINGLE SPACE WRITE                       
         MVC   R+1(132),SPACES                                                  
         MVC   R+1(14),=C'++DDS      DSN'                                       
         MVC   R+16(60),=CL60'YYUN.FILE'                                        
         GOTO1 =V(DATAMGR),DMCB,=C'DMPRINT',=C'PRTQUE',0,R,A(CIREC)             
         CLI   DMCB+8,0                                                         
         BNE   YPQTERR                                                          
*                                                                               
*5TH RECORD: ONE DUMMY LINE (MUST HAVE AT LEAST ONE LINE OF NOT ++DDS)          
         MVI   R,X'09'             SET SINGLE SPACE WRITE                       
         MVC   R+1(132),SPACES                                                  
         MVC   R+1(16),=C'*** ANYTHING ***'                                     
         GOTO1 =V(DATAMGR),DMCB,=C'DMPRINT',=C'PRTQUE',0,R,A(CIREC)             
         CLI   DMCB+8,0                                                         
         BNE   YPQTERR                                                          
*                                                                               
         MVI   R,X'FF'             SET END OF REPORT                            
         MVC   R+1(132),SPACES                                                  
         GOTO1 =V(DATAMGR),DMCB,=C'DMPRINT',=C'PRTQUE',0,R,A(CIREC)             
         B     YPQTX                                                            
*                                                                               
YPQTERR  MVC   P+11(13),=C'*** ERROR ***'                                       
         MVC   P+30(27),=C'COULD NOT WRITE PRTQUE LINE'                         
         GOTO1 =V(PRINTER)                                                      
*                                                                               
YPQTX    XBASE                                                                  
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
         DS    0D                                                               
         DC    CL8'**SSB***'                                                    
         PRINT OFF                                                              
       ++INCLUDE FASSBOFF                                                       
         PRINT ON                                                               
         SPACE 2                                                                
         ORG   SSOOFF                                                           
SSB      DC    XL(SSOOFFX-SSOOFF)'00'                                           
         ORG   SSOXTND                                                          
         DC    X'FF'               SET EXTENDED OFFLINE SSB                     
         ORG                                                                    
SSBL     EQU   *-SSB                                                            
         SPACE 3                                                                
         DC    C'**UTL***'                                                      
UTL      DC    F'0',X'0A'          FOR DATAMGR (CONTROL SYSTEM)                 
         EJECT                                                                  
DMWORK   DS    12D                                                              
DMCB     DS    6F                                                               
DUB      DS    D                                                                
FULL     DS    F                                                                
HALF     DS    H                                                                
BYTE     DS    X                                                                
*                                                                               
R        DS    CL133                                                            
*                                                                               
         DS    0D                                                               
         DC    C'*CIREC**'                                                      
CIREC    DC    14336X'00'          PRINT QUEUE BUFFER                           
*                                                                               
         DS    0D                                                               
         DC    C'R13CHAIN'                                                      
R13CHAIN DS    5000D               WORKING STORAGE                              
*                                                                               
       ++INCLUDE DMPRTQK                                                        
         EJECT                                                                  
       ++INCLUDE DMPRTQL                                                        
         EJECT                                                                  
       ++INCLUDE DDDPRINT                                                       
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'164YYUNPQT   10/01/01'                                      
         END                                                                    
