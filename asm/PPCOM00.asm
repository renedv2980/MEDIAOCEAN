*          DATA SET PPCOM00    AT LEVEL 099 AS OF 12/21/11                      
*PHASE T40800A                                                                  
         TITLE 'PRINTPAK COMMENT MAINTENANCE'                                   
*                                                                               
*        CHANGE LOG                                                             
*                                                                               
* 12/11    SMYE - ALLOW DELETE AND RESTORE OF NVTEXT, I/OCOM AND                
*            CONCOM RECORDS                                                     
*                                                                               
* 09/09    SMYE - HEX SECURITY FOR I/OCOM RECORD AND CLEAR KEY AREA             
*            OF RECORD AT "ADD00" LABEL                                         
*                                                                               
* 10/05    CHANGES FOR 2-CHARACTER OFFICE CODES                                 
*                                                                               
* 06/02    NEW SECURITY INCLUDING CLIENT STRINGS AND TRAFFIC OFFICE             
*                                                                               
* 06/01    ADD TWO MORE COMMENT RECORDS FOR LEGAL WARNINGS -                    
*          "LWARNA,B,C OR D" FOR PLEGWCREC (X'45') AND                          
*          "LQWRN1,2,3 OR 4" FOR PQUARCREC (X'46')                              
*                                                                               
* 06/01    ADD CLIENT AND OFFICE SECURITY                                       
*                                                                               
* 04/01    ADD CODE TO VALIDATE STANDARD COMMENT IF "COM=NNNNNN"                
*          (NESTED COMMENT) IS ENTERED IN AN I/OCOM OR CONCOM COMMENT           
*          AND INCLUDE I/OCOM AND CONCOM IN AUTOREQ                             
*                                                                               
* 02/01    ADD TWO COMMENT RECORDS LIKE "NVTEXT" - "I/OCOM" FOR                 
*          PIOCREC (X'42') AND "CONCOM" FOR PCONCREC (X'44')                    
*                                                                               
T40800   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 400,T40800,R8                                                    
         USING GENOLD,RC                                                        
         USING T408FFD,RA                                                       
MDERR    EQU   13                  INVALID MEDIA                                
ACERR    EQU   12                  INVALID ACTION                               
CHGERR   EQU   142                                                              
RSIZERR  EQU   227                                                              
COMERR   EQU   228                 INVALID 'COM=' LINE                          
DUPERR   EQU   52                  DUPLICATE KEY ON ADD                         
MTCHERR  EQU   53                  RECORD NOT FOUND                             
MSERR    EQU   1                   MISSING INPUT                                
INVERR   EQU   2                   INVALID INPUT                                
LNADDERR EQU   235                 NO ROOM ON SCREEN FOR ADD/SPLIT              
SECLOCK  EQU   55                  SECURITY LOCKOUT                             
RECBKOK  EQU   238                 RECORD IS RESTORED                           
RECBKERR EQU   239                 RECORD NOT DELETED (NO RESTORE)              
*                                                                               
         SPACE 2                                                                
         BAS   RE,INITL                                                         
*                                                                               
         MVC   ACOMFACS,16(R1)                                                  
*                                                                               
         XC    DMCB(12),DMCB                                                    
         MVC   DMCB+4(4),=X'D9000A38'                                           
         GOTO1 VCALLOV,DMCB                                                     
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   VOFFICER,DMCB          SAVE OFFICER ADDRESS                      
*                                                                               
         XC    CMTMSG,CMTMSG                                                    
         FOUT  CMTMSGH                                                          
         EJECT                                                                  
*                                  EDIT ACTION                                  
         LA    R2,CMTACH                                                        
         LA    R3,ACERR                                                         
*                                  SET SO ERROR ON ACTION WILL NOT              
*                                  DISALLOW CHANGE NEXT TIME                    
         MVC   KEYSAVE,SVKEY                                                    
         BAS   RE,ANY                                                           
         CLI   5(R2),3                                                          
         BL    ERROR                                                            
         OC    CMTAC,BLANKS                                                     
*                                  HEX SECURITY SETTINGS                        
         MVI   HEXFLG,0            CLEAR                                        
         TM    12(RA),X'C0'                                                     
         BNO   HEX10                                                            
         MVI   HEXFLG,2            DISPLAY ONLY FOR I/OCOM ONLY                 
         B     HEXTST                                                           
HEX10    DS    0H                                                               
         TM    12(RA),X'40'                                                     
         BNO   HEX20                                                            
         MVI   HEXFLG,1            DISPLAY ONLY FOR ALL RECORDS                 
         B     HEXTST                                                           
HEX20    DS    0H                                                               
         TM    12(RA),X'80'                                                     
         BNO   HEXTST                                                           
         MVI   HEXFLG,3            NO ACCESS FOR I/OCOM RECORDS                 
         B     HEXTST                                                           
HEXTST   DS    0H                                                               
         CLI   HEXFLG,0            ANYTHING SET ?                               
         BE    HEXEND              NO                                           
         CLI   HEXFLG,1            DISPLAY ONLY FOR ALL RECORDS ?               
         BNE   HEXEND              NO - TEST MORE IN NUM LATER                  
         CLC   CMTAC(3),=C'DIS'                                                 
         BE    HEXEND              ONLY DISPLAY ALLOWED                         
HEXERR   DS    0H                                                               
         LA    R3,SECLOCK          SECURITY LOCKOUT                             
         B     ERROR                                                            
HEXEND   DS    0H                                                               
*                                                                               
         CLC   CMTAC(3),=C'ADD'                                                 
         BNE   ACT                                                              
         FOUT  CMTACH,=C'ADD    '                                               
         B     ACT4                                                             
ACT      CLC   CMTAC(3),=C'DIS'                                                 
         BNE   ACT2                                                             
         MVC   PREVCOM(3),CMTAC                                                 
         FOUT  CMTACH,=C'DISPLAY'                                               
         B     ACT8                                                             
ACT2     CLC   CMTAC(3),=C'CHA'                                                 
         BE    ACT2K                                                            
         CLC   CMTAC(3),=C'DEL'                                                 
         BE    ACT2G                                                            
         CLC   CMTAC(3),=C'RES'                                                 
         BNE   ERROR                                                            
         CLC   =C'NVTEXT',CMTNO    RESTORE ONLY FOR NVTEXT, I/OCOM              
         BE    ACT2D                 AND CONCOM                                 
         CLC   =C'I/OCOM',CMTNO                                                 
         BE    ACT2D                                                            
         CLC   =C'CONCOM',CMTNO                                                 
         BNE   ERROR                                                            
ACT2D    DS    0H                                                               
         FOUT  CMTACH,=C'RESTORE'                                               
         B     ACT2M                                                            
ACT2G    DS    0H                                                               
         CLC   =C'NVTEXT',CMTNO    DELETE ONLY FOR NVTEXT, I/OCOM               
         BE    ACT2I                 AND CONCOM                                 
         CLC   =C'I/OCOM',CMTNO                                                 
         BE    ACT2I                                                            
         CLC   =C'CONCOM',CMTNO                                                 
         BNE   ERROR                                                            
ACT2I    DS    0H                                                               
         FOUT  CMTACH,=C'DELETE '                                               
         B     ACT2M                                                            
ACT2K    FOUT  CMTACH,=C'CHANGE '                                               
*                                                                               
*                                  IF CHANGE PREVIOUS COMMENT SHOULD            
*                                      HAVE BEEN DISPLAY.  IF NOT,              
*                                      TREAT CHANGE AS A DISPLAY.               
ACT2M    DS    0H                                                               
         CLC   PREVCOM(3),=C'DIS'                                               
         BE    ACT3                                                             
         MVC   PREVCOM(3),=C'DIS'                                               
         MVI   CHSW,0              INDICATE DISPLAYING                          
         B     ACT8                                                             
ACT3     DS    0H                                                               
         MVI   CHSW,1               0=DIS,1=CHA,2=ADD                           
         B     ACT8                                                             
ACT4     MVC   PREVCOM(3),=C'DIS'                                               
         MVI   CHSW,2                                                           
ACT8     OI    4(R2),X'20'                                                      
         OI    1(R2),X'01'         SET AS MODIFIED FIELD                        
         OI    7(R2),X'01'         SET AS MODIFIED FIELD                        
         EJECT                                                                  
*                                                                               
         MVI   TRFAGSW,0           SET OFF TRAFFIC ID SWITCH                    
*                                                                               
         OC    4(2,RA),4(RA)       TEST AGENCY ON NEW SECURITY                  
         BNZ   *+14                                                             
         OC    6(2,RA),6(RA)       TEST ANY LIMIT ACCESS                        
         BZ    NOSECRET                                                         
*                                                                               
*******  CALL TO FASECRET TO BUILD A SECURITY AUTHORIZATION TABLE               
*                                                                               
         LA    R0,SECBLK           1024 BYTE "SECRET BLOCK"                     
         LHI   R1,1024                                                          
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
*  INITIALIZE SECURITY BLOCK                                                    
         L     RF,ACOMFACS                                                      
         L     RF,CSECRET-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,('SECPINIT',SECBLK),0                                  
         BE    *+6                                                              
         DC    H'0'                BLOCK NOT BIG ENOUGH                         
*                                                                               
NOSECRET DS    0H                                                               
*                                                                               
         BRAS  RE,CKTRAFID         SEE IF "SIGN-ON" HAS A TRAFFIC ID            
         BNE   *+8                                                              
         MVI   TRFAGSW,C'Y'        YES - "SIGN-ON" HAS A TRAFFIC ID             
*                                                                               
         EJECT                                                                  
*                                  EDIT MEDIA                                   
MED      LA    R2,CMTMDH                                                        
         LA    R3,MDERR                                                         
         TM    4(R2),X'20'                                                      
         BO    NUM                                                              
         NI    CMTMDH+4,X'DF'                                                   
         XC    CMTMDNM,CMTMDNM                                                  
         FOUT  CMTMDNMH                                                         
         BAS   RE,ANY                                                           
*                                  READ AGY MED REC                             
         XC    KEY,KEY                                                          
         MVC   KEY(2),AGYALPHA                                                  
         MVC   KEY+2(1),CMTMD                                                   
         MVI   KEY+3,1                                                          
         BAS   RE,HIGH                                                          
         CLC   KEY(4),KEYSAVE                                                   
         BNE   ERROR                                                            
         BAS   RE,GETREC                                                        
         FOUT  CMTMDNMH,PAGYMED                                                 
         OI    4(R2),X'20'                                                      
         EJECT                                                                  
*                                                                               
***********************************************************************         
*                                  EDIT COMMENT NUMBER                          
***********************************************************************         
NUM      LA    R2,CMTNOH                                                        
*****                                                                           
         CLI   HEXFLG,0            ANYTHING TO TEST FOR I/OCOM ?                
         BE    NUM01               NO - CONTINUE                                
         CLC   8(6,R2),=C'I/OCOM'  I/O COMMENT ?                                
         BNE   NUM01               NO - CONTINUE                                
         CLI   HEXFLG,3            ANY ACCESS TO I/OCOM ALLOWED ?               
         BE    HEXERR              NO - SECURITY LOCKOUT                        
         CLI   HEXFLG,2            I/OCOM DISPLAY ONLY ALLOWED ?                
         BNE   NUM01               NO - CONTINUE                                
         CLC   CMTAC(3),=C'DIS'    ACTION DISPLAY ?                             
         BNE   HEXERR              NO - SECURITY LOCKOUT                        
*                                                                               
NUM01    DS    0H                                                               
         LA    R3,SECLOCK          SECURITY LOCKOUT                             
         CLC   8(5,R2),=C'LQWRN'                                                
         BE    NUM02                                                            
         CLC   8(5,R2),=C'LWARN'                                                
         BNE   NUM08                                                            
NUM02    CLC   CMTAC(3),=C'DIS'    ACTION DISPLAY ?                             
         BE    NUM08               YES                                          
         TM    12(RA),X'10'                                                     
         BO    ERROR               SECURITY LOCKOUT                             
NUM08    DS    0H                                                               
         LA    R3,INVERR                 INVALID INPUT                          
*                                                                               
         CLC   8(6,R2),=C'NVTEXT'        NEW TEXT?                              
         BE    NUM10                                                            
         CLC   8(6,R2),=C'I/OCOM'        I/O COMMENT ?                          
         BE    NUM20                                                            
         CLC   8(6,R2),=C'CONCOM'        CONTRACT COMMENT ?                     
         BE    NUM30                                                            
         CLC   8(5,R2),=C'LWARN'         LEGAL WARNING ?                        
         BE    NUM40                                                            
         CLC   8(5,R2),=C'LQWRN'         QUARTERLY COMMENT ?                    
         BE    NUM50                                                            
         B     NUM70                     MUST BE STANDARD COMMENT               
*                                                                               
*                               ****  NVTEXT HANDLING  ****                     
*                                                                               
NUM10    CLI   5(R2),6                   IF ONLY 'NVTEXT' ENTERED,              
         BH    NUM10D                    CLIENT = ALL                           
NUM10B   XC    KEY,KEY                                                          
         MVC   KEY+4(3),=X'FFFFFF'       3X'FF'S IN REC KEY FOR ALL CLI         
         B     NUM10T                                                           
NUM10D   CLI   14(R2),C','               WAS CLIENT OR OFFICE SPECIFIED         
         BNE   ERROR                     (OR COULD BE 'ALL')                    
         OC    15(3,R2),BLANKS                                                  
         CLC   15(3,R2),BLANKS           IF ',BLANKS' ERROR                     
         BE    ERROR                                                            
         CLC   15(3,R2),=C'ALL'          WAS ALL SPECIFIED                      
         BE    NUM10B                                                           
         CLI   15(R2),C'*'               WAS OFFICE SPECIFIED                   
         BNE   NUM10H                                                           
         CLI   5(R2),10                  IF INPUT LONGER THEN THIS IS           
         BH    ERROR                     NOT A VALID OFFICE CODE                
         MVC   TSTOFF2,16(R2)                                                   
         BAS   RE,OFCCHG           GET 1-CHR OFFICE                             
         BNE   ERROR                                                            
*                                                                               
         LA    R1,WORK                                                          
         USING OFFICED,R1          REPLACE ENTERED OFFICE CODE WITH             
         MVC   TSTOFF,OFCOFC         "INTERNAL" 1-CHR OFIICE                    
         DROP  R1                       (MAY BE IDENTICAL)                      
*                                                                               
NUM10F   DS    0H                                                               
         OC    6(2,RA),6(RA)     CHK FOR LIMIT ACCESS                           
         BZ    NUM10G              NO LIMIT ACCESS                              
*                                                                               
         LA    R3,SECLOCK          SECURITY LOCKOUT                             
*                                                                               
         BAS   RE,PPCLIVER              LIMIT ACCESS TESTING                    
         BNE   ERROR                                                            
*                                                                               
*                                                                               
NUM10G   DS    0H                                                               
         MVC   KEY+4(3),=X'FF4040'       FOR OFFICE CODE IN REC, STORE          
         MVC   KEY+5(1),TSTOFF           X'FF' , OFFICE CODE, BLANK             
         B     NUM10T                                                           
*                                                                               
NUM10H   LA    R3,40                     CLIENT NOT ON FILE ERROR MSG           
         XC    KEY,KEY                   READ CLIENT HEADER                     
         MVC   KEY(2),AGYALPHA                                                  
         MVC   KEY+2(1),CMTMD                                                   
         MVI   KEY+3,X'02'                                                      
         MVC   KEY+4(3),15(R2)                                                  
         BAS   RE,HIGH                                                          
         CLC   KEY(7),KEYSAVE                                                   
         BNE   ERROR                                                            
*                                                                               
         LA    R3,SECLOCK          SECURITY LOCKOUT                             
*                                                                               
         OC    6(2,RA),6(RA)     CHK FOR LIMIT ACCESS                           
         BZ    NUM10R              NO LIMIT ACCESS                              
*                                                                               
         BAS   RE,GETREC           GET THE CLIENT RECORD                        
         LA    R4,IOAREA                                                        
         USING PCLTREC,R4                                                       
*                                                                               
         XC    TSTOFF2,TSTOFF2                                                  
         MVC   TSTOFF,PCLTOFF     OFFICE CODE TO TSTOFF                         
*                                                                               
         CLI   TRFAGSW,C'Y'       TRAFFIC AGENCY ID ?                           
         BNE   NUM10P             NO                                            
*                                 SEE IF TRAFFIC OFFICE EXISTS                  
         LA    R5,PCLTREC+33                                                    
         MVI   ELCODE,X'50'       CLT TRAFFIC OFFICE ELEM CODE                  
         BAS   R9,NEXTEL          FOUND ?                                       
         BNE   NUM10P             NO                                            
         MVC   TSTOFF,2(R5)       REPLACE TSTOFF WITH TRAFFIC OFFICE            
NUM10P   BAS   RE,PPCLIVER        LIMIT ACCESS TESTING                          
         BNE   ERROR                                                            
******   B     NUM10R             OK                                            
*                                                                               
         DROP  R4                                                               
*                                                                               
NUM10R   DS    0H                                                               
         XC    KEY,KEY                                                          
         MVC   KEY+4(3),15(R2)                                                  
NUM10T   MVC   KEY(2),AGYALPHA                                                  
         MVC   KEY+2(1),CMTMD                                                   
         MVI   KEY+3,X'41'                                                      
         B     NUM80                                                            
         EJECT                                                                  
*                                                                               
*                               ****  I/OCOM HANDLING  ****                     
*                                                                               
NUM20    CLI   5(R2),6                   IF ONLY 'I/OCOM' ENTERED,              
         BH    NUM20D                    CLIENT = ALL                           
NUM20B   XC    KEY,KEY                                                          
         MVC   KEY+4(3),=X'FFFFFF'       3X'FF'S IN REC KEY FOR ALL CLI         
         B     NUM20T                                                           
NUM20D   CLI   14(R2),C','               WAS CLIENT OR OFFICE SPECIFIED         
         BNE   ERROR                     (OR COULD BE 'ALL')                    
         OC    15(3,R2),BLANKS                                                  
         CLC   15(3,R2),BLANKS           IF ',BLANKS' ERROR                     
         BE    ERROR                                                            
         CLC   15(3,R2),=C'ALL'          WAS ALL SPECIFIED                      
         BE    NUM20B                                                           
         CLI   15(R2),C'*'               WAS OFFICE SPECIFIED                   
         BNE   NUM20H                                                           
         CLI   5(R2),10                  IF INPUT LONGER THEN THIS IS           
         BH    ERROR                     NOT A VALID OFFICE CODE                
         MVC   TSTOFF2,16(R2)                                                   
         BAS   RE,OFCCHG           GET 1-CHR OFFICE                             
         BNE   ERROR                                                            
*                                                                               
         LA    R1,WORK                                                          
         USING OFFICED,R1          REPLACE ENTERED OFFICE CODE WITH             
         MVC   TSTOFF,OFCOFC         "INTERNAL" 1-CHR OFIICE                    
         DROP  R1                       (MAY BE IDENTICAL)                      
*                                                                               
NUM20F   DS    0H                                                               
         OC    6(2,RA),6(RA)     CHK FOR LIMIT ACCESS                           
         BZ    NUM20G              NO LIMIT ACCESS                              
*                                                                               
         LA    R3,SECLOCK          SECURITY LOCKOUT                             
*                                                                               
         BAS   RE,PPCLIVER              LIMIT ACCESS TESTING                    
         BNE   ERROR                                                            
*                                                                               
******   B     NUM20G              OFFICE OK                                    
*                                                                               
NUM20G   DS    0H                                                               
         MVC   KEY+4(3),=X'FF4040'       FOR OFFICE CODE IN REC, STORE          
         MVC   KEY+5(1),TSTOFF           X'FF' , OFFICE CODE, BLANK             
         B     NUM20T                                                           
*                                                                               
NUM20H   LA    R3,40                     CLIENT NOT ON FILE ERROR MSG           
         XC    KEY,KEY                   READ CLIENT HEADER                     
         MVC   KEY(2),AGYALPHA                                                  
         MVC   KEY+2(1),CMTMD                                                   
         MVI   KEY+3,X'02'                                                      
         MVC   KEY+4(3),15(R2)                                                  
         BAS   RE,HIGH                                                          
         CLC   KEY(7),KEYSAVE                                                   
         BNE   ERROR                                                            
*                                                                               
         LA    R3,SECLOCK          SECURITY LOCKOUT                             
*                                                                               
         OC    6(2,RA),6(RA)     CHK FOR LIMIT ACCESS                           
         BZ    NUM20R              NO LIMIT ACCESS                              
*                                                                               
         BAS   RE,GETREC           GET THE CLIENT RECORD                        
         LA    R4,IOAREA                                                        
         USING PCLTREC,R4                                                       
*                                                                               
         MVC   TSTOFF,PCLTOFF     OFFICE CODE TO TSTOFF                         
*                                                                               
         CLI   TRFAGSW,C'Y'       TRAFFIC AGENCY ID ?                           
         BNE   NUM20P             NO                                            
*                                 SEE IF TRAFFIC OFFICE EXISTS                  
         LA    R5,PCLTREC+33                                                    
         MVI   ELCODE,X'50'       CLT TRAFFIC OFFICE ELEM CODE                  
         BAS   R9,NEXTEL          FOUND ?                                       
         BNE   NUM20P             NO                                            
         MVC   TSTOFF,2(R5)       REPLACE TSTOFF WITH TRAFFIC OFFICE            
NUM20P   BAS   RE,PPCLIVER        LIMIT ACCESS TESTING                          
         BNE   ERROR                                                            
******   B     NUM20R             OK                                            
*                                                                               
         DROP  R4                                                               
*                                                                               
NUM20R   DS    0H                                                               
         XC    KEY,KEY                                                          
         MVC   KEY+4(3),15(R2)                                                  
NUM20T   MVC   KEY(2),AGYALPHA                                                  
         MVC   KEY+2(1),CMTMD                                                   
         MVI   KEY+3,X'42'        PIOCREC                                       
         B     NUM80                                                            
         EJECT                                                                  
*                                                                               
*                               ****  CONCOM HANDLING  ****                     
*                                                                               
NUM30    CLI   5(R2),6                   IF ONLY 'CONCOM' ENTERED,              
         BH    NUM30D                    CLIENT = ALL                           
NUM30B   XC    KEY,KEY                                                          
         MVC   KEY+4(3),=X'FFFFFF'       3X'FF'S IN REC KEY FOR ALL CLI         
         B     NUM30T                                                           
NUM30D   CLI   14(R2),C','               WAS CLIENT OR OFFICE SPECIFIED         
         BNE   ERROR                     (OR COULD BE 'ALL')                    
         OC    15(3,R2),BLANKS                                                  
         CLC   15(3,R2),BLANKS           IF ',BLANKS' ERROR                     
         BE    ERROR                                                            
         CLC   15(3,R2),=C'ALL'          WAS ALL SPECIFIED                      
         BE    NUM30B                                                           
         CLI   15(R2),C'*'               WAS OFFICE SPECIFIED                   
         BNE   NUM30H                                                           
         CLI   5(R2),10                  IF INPUT LONGER THEN THIS IS           
         BH    ERROR                     NOT A VALID OFFICE CODE                
         MVC   TSTOFF2,16(R2)                                                   
         BAS   RE,OFCCHG           GET 1-CHR OFFICE                             
         BNE   ERROR               INVALID OFFICE                               
*                                                                               
         LA    R1,WORK                                                          
         USING OFFICED,R1          REPLACE ENTERED OFFICE CODE WITH             
         MVC   TSTOFF,OFCOFC         "INTERNAL" 1-CHR OFIICE                    
         DROP  R1                       (MAY BE IDENTICAL)                      
*                                                                               
NUM30F   DS    0H                                                               
         OC    6(2,RA),6(RA)     CHK FOR LIMIT ACCESS                           
         BZ    NUM30G              NO LIMIT ACCESS                              
*                                                                               
         LA    R3,SECLOCK          SECURITY LOCKOUT                             
*                                                                               
         BAS   RE,PPCLIVER              LIMIT ACCESS TESTING                    
         BNE   ERROR                                                            
*                                                                               
******   B     NUM30G              OFFICE OK                                    
*                                                                               
NUM30G   DS    0H                                                               
         MVC   KEY+4(3),=X'FF4040'       FOR OFFICE CODE IN REC, STORE          
         MVC   KEY+5(1),TSTOFF           X'FF' , OFFICE CODE, BLANK             
         B     NUM30T                                                           
*                                                                               
NUM30H   LA    R3,40                     CLIENT NOT ON FILE ERROR MSG           
         XC    KEY,KEY                   READ CLIENT HEADER                     
         MVC   KEY(2),AGYALPHA                                                  
         MVC   KEY+2(1),CMTMD                                                   
         MVI   KEY+3,X'02'                                                      
         MVC   KEY+4(3),15(R2)                                                  
         BAS   RE,HIGH                                                          
         CLC   KEY(7),KEYSAVE                                                   
         BNE   ERROR                                                            
*                                                                               
         LA    R3,SECLOCK          SECURITY LOCKOUT                             
*                                                                               
         OC    6(2,RA),6(RA)     CHK FOR LIMIT ACCESS                           
         BZ    NUM30R              NO LIMIT ACCESS                              
*                                                                               
         BAS   RE,GETREC           GET THE CLIENT RECORD                        
         LA    R4,IOAREA                                                        
         USING PCLTREC,R4                                                       
*                                                                               
         MVC   TSTOFF,PCLTOFF     OFFICE CODE TO TSTOFF                         
*                                                                               
         CLI   TRFAGSW,C'Y'       TRAFFIC AGENCY ID ?                           
         BNE   NUM30P             NO                                            
*                                 SEE IF TRAFFIC OFFICE EXISTS                  
         LA    R5,PCLTREC+33                                                    
         MVI   ELCODE,X'50'       CLT TRAFFIC OFFICE ELEM CODE                  
         BAS   R9,NEXTEL          FOUND ?                                       
         BNE   NUM30P             NO                                            
         MVC   TSTOFF,2(R5)       REPLACE TSTOFF WITH TRAFFIC OFFICE            
NUM30P   BAS   RE,PPCLIVER        LIMIT ACCESS TESTING                          
         BNE   ERROR                                                            
******   B     NUM30R             OK                                            
*                                                                               
         DROP  R4                                                               
*                                                                               
NUM30R   DS    0H                                                               
         XC    KEY,KEY                                                          
         MVC   KEY+4(3),15(R2)                                                  
NUM30T   MVC   KEY(2),AGYALPHA                                                  
         MVC   KEY+2(1),CMTMD                                                   
         MVI   KEY+3,X'44'        PCONCREC                                      
         B     NUM80                                                            
         EJECT                                                                  
******                                                                          
*                   ****  LEGAL WARNING (LWARNA,B,C & D) HANDLING  ****         
*                                                                               
NUM40    DS    0H                                                               
         CLI   13(R2),C'A'         MUST BE A,B,C OR D                           
         BL    ERROR                                                            
         CLI   13(R2),C'D'         MUST BE A,B,C OR D                           
         BH    ERROR                                                            
         CLI   5(R2),6                   IF ONLY 'LWARNX' ENTERED,              
         BNE   ERROR                                                            
*                                                                               
***  NUM40D TO NUM40R WILL ONLY BE USED IF THIS COMMENT CODE IS                 
***  TO BE MADE CLIENT (OR OFFICE) SPECIFIC                                     
*                                                                               
NUM40B   XC    KEY,KEY                                                          
         MVC   KEY+4(3),=X'FFFFFF'       3X'FF'S IN REC KEY FOR ALL CLI         
         B     NUM40T                                                           
NUM40D   CLI   14(R2),C','               WAS CLIENT OR OFFICE SPECIFIED         
         BNE   ERROR                     (OR COULD BE 'ALL')                    
         OC    15(3,R2),BLANKS                                                  
         CLC   15(3,R2),BLANKS           IF ',BLANKS' ERROR                     
         BE    ERROR                                                            
         CLC   15(3,R2),=C'ALL'          WAS ALL SPECIFIED                      
         BE    NUM40B                                                           
         CLI   15(R2),C'*'               WAS OFFICE SPECIFIED                   
         BNE   NUM40H                                                           
         CLI   5(R2),10                  IF INPUT LONGER THEN THIS IS           
         BH    ERROR                     NOT A VALID OFFICE CODE                
         MVC   TSTOFF2,16(R2)                                                   
         BAS   RE,OFCCHG           GET 1-CHR OFFICE                             
         BNE   ERROR               INVALID OFFICE                               
*                                                                               
         LA    R1,WORK                                                          
         USING OFFICED,R1          REPLACE ENTERED OFFICE CODE WITH             
         MVC   TSTOFF,OFCOFC         "INTERNAL" 1-CHR OFIICE                    
         DROP  R1                       (MAY BE IDENTICAL)                      
*                                                                               
NUM40F   DS    0H                                                               
         OC    6(2,RA),6(RA)     CHK FOR LIMIT ACCESS                           
         BZ    NUM40G              NO LIMIT ACCESS                              
*                                                                               
         LA    R3,SECLOCK          SECURITY LOCKOUT                             
*                                                                               
         BAS   RE,PPCLIVER              LIMIT ACCESS TESTING                    
         BNE   ERROR                                                            
*                                                                               
******   B     NUM40G              OFFICE OK                                    
*                                                                               
NUM40G   DS    0H                                                               
         MVC   KEY+4(3),=X'FF4040'       FOR OFFICE CODE IN REC, STORE          
         MVC   KEY+5(1),TSTOFF           X'FF' , OFFICE CODE, BLANK             
         B     NUM40T                                                           
*                                                                               
NUM40H   LA    R3,40                     CLIENT NOT ON FILE ERROR MSG           
         XC    KEY,KEY                   READ CLIENT HEADER                     
         MVC   KEY(2),AGYALPHA                                                  
         MVC   KEY+2(1),CMTMD                                                   
         MVI   KEY+3,X'02'                                                      
         MVC   KEY+4(3),15(R2)                                                  
         BAS   RE,HIGH                                                          
         CLC   KEY(7),KEYSAVE                                                   
         BNE   ERROR                                                            
*                                                                               
         LA    R3,SECLOCK          SECURITY LOCKOUT                             
*                                                                               
         OC    6(2,RA),6(RA)     CHK FOR LIMIT ACCESS                           
         BZ    NUM40R              NO LIMIT ACCESS                              
*                                                                               
         BAS   RE,GETREC           GET THE CLIENT RECORD                        
         LA    R4,IOAREA                                                        
         USING PCLTREC,R4                                                       
*                                                                               
         MVC   TSTOFF,PCLTOFF     OFFICE CODE TO TSTOFF                         
*                                                                               
         CLI   TRFAGSW,C'Y'       TRAFFIC AGENCY ID ?                           
         BNE   NUM40P             NO                                            
*                                 SEE IF TRAFFIC OFFICE EXISTS                  
         LA    R5,PCLTREC+33                                                    
         MVI   ELCODE,X'50'       CLT TRAFFIC OFFICE ELEM CODE                  
         BAS   R9,NEXTEL          FOUND ?                                       
         BNE   NUM40P             NO                                            
         MVC   TSTOFF,2(R5)       REPLACE TSTOFF WITH TRAFFIC OFFICE            
NUM40P   BAS   RE,PPCLIVER        LIMIT ACCESS TESTING                          
         BNE   ERROR                                                            
******   B     NUM40R             OK                                            
*                                                                               
         DROP  R4                                                               
*                                                                               
NUM40R   DS    0H                                                               
         XC    KEY,KEY                                                          
         MVC   KEY+4(3),15(R2)                                                  
NUM40T   MVC   KEY(2),AGYALPHA                                                  
         MVC   KEY+2(1),CMTMD                                                   
         MVI   KEY+3,X'45'        PLEGWCREC                                     
         MVC   KEY+7(1),13(R2)    MUST BE A,B,C OR D                            
         B     NUM80                                                            
         EJECT                                                                  
******                                                                          
*            ****  "QUARTERLY" COMMENT (LQWRN1,2,3 & 4) HANDLING  ****          
*                                                                               
NUM50    DS    0H                                                               
         CLI   13(R2),C'1'         MUST BE 1,2,3 OR 4                           
         BL    ERROR                                                            
         CLI   13(R2),C'4'         MUST BE 1,2,3 OR 4                           
         BH    ERROR                                                            
         CLI   5(R2),6                   IF ONLY 'LQWRNN' ENTERED,              
         BNE   ERROR                                                            
*                                                                               
*NOP*    BH    NUM50D                                                           
***  NUM50D TO NUM50R WILL ONLY BE USED IF THIS COMMENT CODE IS                 
***  TO BE MADE CLIENT (OR OFFICE) SPECIFIC                                     
*                                                                               
NUM50B   XC    KEY,KEY                                                          
         MVC   KEY+4(3),=X'FFFFFF'       3X'FF'S IN REC KEY FOR ALL CLI         
         B     NUM50T                                                           
NUM50D   CLI   14(R2),C','               WAS CLIENT OR OFFICE SPECIFIED         
         BNE   ERROR                     (OR COULD BE 'ALL')                    
         OC    15(3,R2),BLANKS                                                  
         CLC   15(3,R2),BLANKS           IF ',BLANKS' ERROR                     
         BE    ERROR                                                            
         CLC   15(3,R2),=C'ALL'          WAS ALL SPECIFIED                      
         BE    NUM50B                                                           
         CLI   15(R2),C'*'               WAS OFFICE SPECIFIED                   
         BNE   NUM50H                                                           
         CLI   5(R2),10                  IF INPUT LONGER THEN THIS IS           
         BH    ERROR                     NOT A VALID OFFICE CODE                
         MVC   TSTOFF2,16(R2)                                                   
         BAS   RE,OFCCHG           GET 1-CHR OFFICE                             
         BNE   ERROR               INVALID OFFICE                               
*                                                                               
         LA    R1,WORK                                                          
         USING OFFICED,R1          REPLACE ENTERED OFFICE CODE WITH             
         MVC   TSTOFF,OFCOFC         "INTERNAL" 1-CHR OFIICE                    
         DROP  R1                       (MAY BE IDENTICAL)                      
*                                                                               
NUM50F   DS    0H                                                               
         OC    6(2,RA),6(RA)     CHK FOR LIMIT ACCESS                           
         BZ    NUM50G              NO LIMIT ACCESS                              
*                                                                               
         LA    R3,SECLOCK          SECURITY LOCKOUT                             
*                                                                               
         BAS   RE,PPCLIVER              LIMIT ACCESS TESTING                    
         BNE   ERROR                                                            
*                                                                               
******   B     NUM50G              OFFICE OK                                    
*                                                                               
NUM50G   DS    0H                                                               
         MVC   KEY+4(3),=X'FF4040'       FOR OFFICE CODE IN REC, STORE          
         MVC   KEY+5(1),TSTOFF           X'FF' , OFFICE CODE, BLANK             
         B     NUM50T                                                           
*                                                                               
NUM50H   LA    R3,40                     CLIENT NOT ON FILE ERROR MSG           
         XC    KEY,KEY                   READ CLIENT HEADER                     
         MVC   KEY(2),AGYALPHA                                                  
         MVC   KEY+2(1),CMTMD                                                   
         MVI   KEY+3,X'02'                                                      
         MVC   KEY+4(3),15(R2)                                                  
         BAS   RE,HIGH                                                          
         CLC   KEY(7),KEYSAVE                                                   
         BNE   ERROR                                                            
*                                                                               
         LA    R3,SECLOCK          SECURITY LOCKOUT                             
*                                                                               
         OC    6(2,RA),6(RA)     CHK FOR LIMIT ACCESS                           
         BZ    NUM50R              NO LIMIT ACCESS                              
*                                                                               
         BAS   RE,GETREC           GET THE CLIENT RECORD                        
         LA    R4,IOAREA                                                        
         USING PCLTREC,R4                                                       
*                                                                               
         MVC   TSTOFF,PCLTOFF     OFFICE CODE TO TSTOFF                         
*                                                                               
         CLI   TRFAGSW,C'Y'       TRAFFIC AGENCY ID ?                           
         BNE   NUM50P             NO                                            
*                                 SEE IF TRAFFIC OFFICE EXISTS                  
         LA    R5,PCLTREC+33                                                    
         MVI   ELCODE,X'50'       CLT TRAFFIC OFFICE ELEM CODE                  
         BAS   R9,NEXTEL          FOUND ?                                       
         BNE   NUM50P             NO                                            
         MVC   TSTOFF,2(R5)       REPLACE TSTOFF WITH TRAFFIC OFFICE            
NUM50P   BAS   RE,PPCLIVER        LIMIT ACCESS TESTING                          
         BNE   ERROR                                                            
******   B     NUM50R             OK                                            
*                                                                               
         DROP  R4                                                               
*                                                                               
NUM50R   DS    0H                                                               
         XC    KEY,KEY                                                          
         MVC   KEY+4(3),15(R2)                                                  
NUM50T   MVC   KEY(2),AGYALPHA                                                  
         MVC   KEY+2(1),CMTMD                                                   
         MVI   KEY+3,X'46'        PQUARCREC                                     
         MVC   KEY+7(1),13(R2)    MUST BE 1,2,3 OR 4                            
         B     NUM80                                                            
         EJECT                                                                  
*****                                                                           
*                               ****  STANDARD COMMENT HANDLING  ****           
*                                                                               
NUM70    LA    R3,MSERR                                                         
         BAS   RE,ANY                                                           
*                                  RIGHT JUSTIFY COMMENT NO                     
         CLI   5(R2),6                                                          
         BNH   NUM70C                                                           
NUMERR   LA    R3,INVERR             MAX 6 CHARS                                
         B     ERROR                                                            
*                                                                               
NUM70C   SR    R4,R4                                                            
         IC    R4,5(R2)                                                         
         LA    R3,6                                                             
         SR    R3,R4                                                            
         LA    R5,WKNO(R3)                                                      
         MVC   WKNO(6),BLANKS                                                   
         EX    R4,MVNO                                                          
*                                                                               
NUM70F   LA    R4,WKNO                                                          
         LA    R5,6                                                             
NUM70I   CLI   0(R4),C','          DISALLOW COMMAS IN NUMBER                    
         BE    NUMERR                                                           
         LA    R4,1(R4)                                                         
         BCT   R5,NUM70I                                                        
*                                                                               
NUM70L   LA    R4,WKNO                                                          
         LA    R5,6                                                             
NUM70P   CLI   0(R4),C'('          DISALLOW COMMAS IN NUMBER                    
         BE    NUM70U                                                           
NUM70R   LA    R4,1(R4)                                                         
         BCT   R5,NUM70P                                                        
         B     NUM70X                                                           
*                                                                               
NUM70U   CLI   2(R4),C')'          DISALLOW (X) IN CODE FOR ANY X               
         BE    NUMERR                                                           
         B     NUM70R                                                           
*                                  READ COMMENT REC                             
NUM70X   XC    KEY,KEY                                                          
         MVC   KEY(2),AGYALPHA                                                  
         MVC   KEY+2(1),CMTMD                                                   
         MVI   KEY+3,X'40'                                                      
         MVC   KEY+4(6),WKNO                                                    
*                                                                               
NUM80    DS    0H                                                               
         CLC   CMTAC(3),=C'RES'    RESTORE ?                                    
         BNE   NUM80G              NO                                           
         OI    DMINBTS,X'08'       READ DELETES                                 
NUM80G   DS    0H                                                               
         BAS   RE,HIGH                                                          
         NI    DMINBTS,X'FF'-X'08'   TURN OFF                                   
         CLI   CMTAC,C'A'                                                       
         BE    ADD00                                                            
         CLC   CMTAC(3),=C'RES'    RESTORE ?                                    
         BNE   NUM80K              NO                                           
         TM    KEY+25,X'80'        FLAGGED FOR DELETION ?                       
         BO    NUM80K              YES                                          
         LA    R3,RECBKERR         RECORD NOT DELETED                           
         B     ERROR                                                            
*                                  IF DISPLAY OR CHANGE - REC                   
*                                      MUST BE FOUND                            
NUM80K   DS    0H                                                               
         LA    R3,MTCHERR                                                       
         CLC   KEY(10),KEYSAVE                                                  
         BNE   ERROR                                                            
         CLC   CMTAC(3),=C'RES'    RESTORE ?                                    
         BNE   NUM80R              NO                                           
         OI    DMINBTS,X'08'       READ DELETES                                 
NUM80R   DS    0H                                                               
         BAS   RE,GETREC                                                        
         NI    DMINBTS,X'FF'-X'08'   TURN OFF                                   
         CLI   CHSW,1                                                           
         BE    CHNG                                                             
         EJECT                                                                  
*                                  GET ELEMENTS                                 
DISP     LA    R5,PCOMREC+33       COULD ALSO BE PNVTREC OR PIOCREC OR          
         LA    R2,CMTCM1H            PCONCREC OR PLEGWCREC OR PQUARCREC         
         SPACE 2                                                                
DISP2    CLI   0(R5),X'40'                                                      
         BNE   DISP4                                                            
         XC    8(70,R2),8(R2)      FIRST CLEAR LINE                             
         SR    R4,R4                                                            
         IC    R4,1(R5)                                                         
         SH    R4,=H'3'                                                         
         EX    R4,MVCMT                                                         
         FOUT  (R2)                                                             
         LA    R2,78(R2)                                                        
         MVI   ELCODE,X'40'                                                     
         BAS   R9,NEXTEL                                                        
         BNE   DISP4                                                            
         B     DISP2                                                            
DISP4    FOUT  CMTMSGH,=C'COMMENT DISPLAYED SUCCESSFULLY'                       
         BAS   RE,CLRSCRN          CLEAR REST OF SCREEN                         
         LA    R2,CMTACH                                                        
         CLI   CHSW,2                                                           
         BNE   *+8                                                              
         LA    R2,CMTCM1H                                                       
         B     EXIT                                                             
         SPACE 2                                                                
MVCMT    MVC   8(0,R2),2(R5)                                                    
MVNO     MVC   0(0,R5),8(R2)                                                    
         EJECT                                                                  
*                                  IF ACTION IS CHANGE COMPARE                  
*                                      TO PRIOR KEY                             
CHNG     CLC   SVKEY(10),KEYSAVE                                                
         BE    CHNG2                                                            
         B     DISP                ELSE DISPLAY                                 
*                                                                               
*                                                                               
CHNG2    DS    0H                  CREATE ALTERED REC OR . . .                  
         CLC   CMTAC(3),=C'DEL'                                                 
         BE    DEL00               DELETE THE RECORD OR                         
         CLC   CMTAC(3),=C'RES'                                                 
         BE    RES00               RESTORE THE RECORD                           
*                                                                               
         LA    R2,CMTCM1H          CHANGE RECORD                                
         LA    R3,MSERR                                                         
         B     ADD01                                                            
*                                  RETURN FROM ADD0I                            
CHNG4    BAS   RE,PUTREC                                                        
         FOUT  CMTMSGH,=C'COMMENT CHANGED SUCCESSFULLY'                         
         LA    R2,CMTACH                                                        
         B     REQ                                                              
         EJECT                                                                  
*                                                                               
DEL00    DS    0H                  DELETE RECORD                                
         OI    PCOMREC+27,X'80'    FLAG RECORD FOR DELETION                     
         BAS   RE,PUTREC                                                        
         OI    KEY+25,X'80'        FLAG KEY FOR DELETION                        
         BAS   RE,WRITE                                                         
*                                                                               
         FOUT  CMTMSGH,=C'COMMENT HAS BEEN DELETED'                             
         LA    R2,CMTACH                                                        
         B     REQ                                                              
*                                                                               
RES00    DS    0H                  RESTORE RECORD                               
         NI    PCOMREC+27,X'FF'-X'80'    TURN OFF RECORD DELETE FLAG            
         BAS   RE,PUTREC                                                        
         NI    KEY+25,X'FF'-X'80'        TURN OFF KEY DELETE FLAG               
         BAS   RE,WRITE                                                         
*                                                                               
         FOUT  CMTMSGH,=C'COMMENT HAS BEEN RESTORED'                            
         LA    R2,CMTACH                                                        
         B     REQ                                                              
         EJECT                                                                  
*                                                                               
*                                  ON ADD MATCHING REC SHOULD NOT BE            
*                                      FOUND.                                   
ADD00    LA    R3,DUPERR                                                        
         CLC   KEY(10),KEYSAVE                                                  
         BE    ERROR                                                            
*                                                                               
*                                  THERE SHOULD BE AT LEAST ONE NON-            
*                                      BLANK COMMENT LINE.                      
         LA    R2,CMTCM1H                                                       
         LA    R3,MSERR                                                         
*                                  SET UP REC IN IOAREA                         
         MVC   PCOMKEY(10),KEYSAVE                                              
         XC    PCOMKEY+10(23),PCOMKEY+10                                        
ADD01    MVC   PCOMLEN(2),=H'33'                                                
*                                                                               
*                                                                               
*        FIND NUMBER OF LAST LINE SCREEN WITH DATA                              
*                                                                               
         LR    R5,R2               SAVE R2                                      
*                                                                               
         LA    R2,CMTCM1H          POINT TO FIRST COMMENT LINE                  
         SR    R6,R6               INIT LINE COUNTER                            
         MVI   LASTLIN,0           INIT LINE NUMBER SAVEAREA                    
*                                                                               
ADD01LP  DS    0H                                                               
*                                                                               
         AH    R6,=H'1'            BUMP LINE COUNTER                            
*                                                                               
         CLI   5(R2),0             IF LINE HAS DATA                             
         BE    ADD01CN                                                          
*                                                                               
         OC    8(L'CMTCM1,R2),BLANKS MUST BE GREATER THAN SPACES                
         CLC   8(L'CMTCM1,R2),BLANKS                                            
         BNH   ADD01CN                                                          
*                                                                               
         STC   R6,LASTLIN             UPDATE LAST LINE NUMBER                   
*                                                                               
ADD01CN  DS    0H                                                               
*                                                                               
         BAS   R9,BMPFLD           BUMP TO NEXT COMMENT                         
         BNE   ADD01LP                                                          
*                                  END OF SCREEN                                
         LR    R2,R5               RESTORE R2                                   
*                                                                               
         ICM   R5,15,ATIOB         POINT TO TIOB                                
         USING TIOBD,R5            ESTABLISH TIOB                               
*                                                                               
         CLI   TIOBAID,0           IF PFKEY ENTERED                             
         BE    *+12                                                             
         BAS   RE,PFKEYS              GO ANALYZE                                
         BNE   ERROR                  ERROR FOUND                               
*                                  SET UP ELEMENTS                              
         LA    R4,PCOMREC+33                                                    
         LR    RE,R4               CLEAR COMMENT AREA                           
         LA    RF,1000                                                          
         XCEF                                                                   
*                                                                               
         SR    R6,R6               INIT LINE COUNTER                            
*                                                                               
ADD02    DS    0H                                                               
*                                                                               
         LA    R6,1(R6)            BUMP LINE COUNTER                            
*                                                                               
         CLI   5(R2),0                                                          
         BE    ADD03                                                            
*                                                                               
         OC    8(L'CMTCM1,R2),BLANKS    MAKE UPPERCASE                          
*                                                                               
         CLC   8(L'CMTCM1,R2),BLANKS    SKIP IF SOMETHING ON LINE               
         BH    ADD04                                                            
*                                                                               
ADD03    DS    0H                  NO DATA ON LINE                              
*                                                                               
         CLM   R6,1,LASTLIN        IF MORE LINES WITH DATA TO FOLLOW            
         BNH   ADD04                  TREAT AS A LINE WITH DATA                 
*                                                                               
         BAS   R9,BMPFLD           ELSE BUMP TO NEXT LINE                       
         BE    ADD06               END OF SCREEN                                
         B     ADD02                                                            
*                                                                               
ADD04    MVI   ADDSW,1                                                          
*                                                                               
***********************************************************************         
*               CHECK FOR STANDARD COMMENTS FOR I/OCOM AND CONCOM     *         
***********************************************************************         
INE05    DS    0H                                                               
         LA    R9,8(R2)                                                         
         LA    R5,8+L'CMTCM1(R2)  EOL                                           
INE10    DS    0H                                                               
         CLC   0(4,R9),=C'COM='                                                 
         BNE   INE20                                                            
*                                                                               
         CLC   CMTNO(6),=C'I/OCOM'                                              
         BE    INE11                                                            
         CLC   CMTNO(6),=C'CONCOM'                                              
         BNE   INE24          ERROR - "COM=" ONLY FOR I/OCOM AND CONCOM         
INE11    DS    0H                                                               
         LA    R9,4(R9)                                                         
         LR    R7,R9               SAVE START                                   
INE12    DS    0H                                                               
         CR    R9,R5                                                            
         BNL   INE14                                                            
         CLI   0(R9),C','                                                       
         BE    INE14                                                            
         CLC   0(2,R9),BLANKS                                                   
         BE    INE14                                                            
         LA    R9,1(R9)                                                         
         B     INE12                                                            
*                                                                               
INE14    DS    0H                                                               
         SR    R9,R7               R9 = LENGTH                                  
         BNP   INE24                                                            
         CH    R9,=H'6'                                                         
         BH    INE24                                                            
         MVC   WORK,BLANKS                                                      
         LA    RF,WORK+6                                                        
         SR    RF,R9                                                            
         BCTR  R9,R0                                                            
         EX    R9,*+8              MOVE TO WORK+6-LENGTH                        
         B     *+10                                                             
         MVC   0(0,RF),0(R7)                                                    
*                                  LOOK FOR RECORD                              
         XC    KEY,KEY                                                          
         MVC   KEY(2),AGYALPHA                                                  
         MVC   KEY+2(1),CMTMD                                                   
         MVI   KEY+3,X'40'                                                      
         MVC   KEY+4(6),WORK                                                    
         BAS   RE,HIGH                                                          
         CLC   KEY(25),KEYSAVE                                                  
         BNE   INE15               ERROR - NOT FOUND                            
         XC    KEYSAVE,KEYSAVE                                                  
         MVC   KEYSAVE(10),SVKEY                                                
         B     INE16                                                            
*                                                                               
INE15    DS    0H                                                               
         MVC   CMTMSG(L'COMERM),COMERM                                          
         MVC   CMTMSG+L'COMERM+1(6),WORK                                        
         OI    CMTMSGH+6,X'80'                                                  
         MVI   ERRAREA,X'FF'                                                    
*                                                                               
         XC    KEYSAVE,KEYSAVE                                                  
         MVC   KEYSAVE(10),SVKEY                                                
*                                                                               
         B     EXIT                                                             
*                                                                               
INE16    DS    0H                                                               
         LA    R9,2(R9,R7)         POINT TO NEXT 'COM='                         
         B     INE10                                                            
INE20    DS    0H                                                               
         LA    R0,8(R2)                                                         
         CR    R9,R0               IF NO 'COM=' AT START OF LINE                
         BE    INE26               TREAT AS FREE FORM                           
         LA    R7,7+L'CMTCM1(R2)                                                
         CLI   0(R7),C' '          REST OF LINE MUST BE BLANKS                  
         BH    *+8                                                              
         BCT   R7,*-8                                                           
         CR    R7,R9                                                            
         BNL   INE24                                                            
         B     INE30               END OF STD COMM EDITING                      
INE24    DS    0H                                                               
         LA    R3,COMERR                                                        
         B     ERROR                                                            
*                                                                               
INE26    DS    0H                                                               
         CLC   0(4,R9),=C'COM='    IMBEDDED 'COM=' IS ERROR                     
         BE    INE24                                                            
         LA    R9,1(R9)                                                         
         CR    R9,R5               CHECK EOL                                    
         BNL   INE30                                                            
         B     INE26                                                            
*                                                                               
INE30    DS    0H                  END OF STD COMMENT EDITING                   
*                                                                               
         LA    R5,L'CMTCM1-1+8(R2) POINT TO LAST BYTE OF LINE                   
         LA    RF,L'CMTCM1         COMMENT LENGTH                               
*                                                                               
         CLI   0(R5),C' '          FIND END OF COMMENT                          
         BH    ADD04A                                                           
         BCTR  R5,0                BACK UP A POSITION                           
         BCT   RF,*-10                                                          
*                                  NO DATA ON LINE                              
         LA    RF,1                FORCE AT LEAST 1 BYTE OF COMMENT             
         MVI   8(R2),0                THAT IS FORCED TO NULLS                   
*                                                                               
ADD04A   DS    0H                                                               
*                                                                               
         LA    R5,2(RF)            ADD ID AND LENGTH TO COMMENT LENGTH          
         STC   R5,ELEM+1           SET COMMENT ELEMENT LENGTH                   
*                                                                               
         MVI   ELEM,X'40'          SET COMMENT ELEMENT ID                       
*                                                                               
         SH    R5,=H'3'            DECREMENT FOR EXECUTE                        
*                                                                               
         EX    R5,MVECM            MOVE COMMENT TO ELEMENT                      
*                                                                               
         CLC   ELEM+2(7),=C'++START'                                            
         BE    ADD04E                                                           
         CLC   ELEM+2(5),=C'++END'                                              
         BE    ADD04E                                                           
         CLI   ELEM+2,C'+'                                                      
         BNE   ADD04E                                                           
         CLI   ELEM+3,C'1'                                                      
         BL    COMMERR                                                          
         CLI   ELEM+3,C'3'                                                      
         BH    COMMERR                                                          
*                                                                               
ADD04E   MVC   HALF,PCOMLEN                                                     
         LH    RE,HALF                                                          
         LA    RE,3(R5,RE)         NEW RECORD LENGTH                            
         CH    RE,=H'975'                                                       
         BL    ADD05                                                            
         LA    R3,RSIZERR                                                       
         B     ERROR                                                            
ADD05    DS    0H                                                               
         GOTO1 VRECUP,DMCB,(1,PCOMREC),ELEM,(R4)                                
         LA    R4,3(R4,R5)                                                      
         BAS   R9,BMPFLD                                                        
         BNE   ADD02                                                            
*                                  TEST IF ANY ELEMS ADDED                      
ADD06    LA    R2,CMTCM1H                                                       
         CLI   ADDSW,1                                                          
         BNE   ERROR                                                            
         MVI   ADDSW,0                                                          
         CLI   CHSW,1                                                           
         BE    CHNG4                                                            
*                                  ADD REC                                      
         MVC   KEY(25),PCOMKEY                                                  
         BAS   RE,ADDREC                                                        
         FOUT  CMTMSGH,=C'COMMENT ADDED SUCCESSFULLY'                           
         LA    R2,CMTACH                                                        
         B     REQ                                                              
         SPACE 3                                                                
MVECM    MVC   ELEM+2(0),8(R2)                                                  
*                                                                               
COMMERR  FOUT  CMTMSGH,=C'COMMENT LINE STARTING WITH ''+'' MUST BE FOLLX        
               OWED BY 1,2,OR 3',59                                             
         MVI   ERRAREA,X'FF'                                                    
         B     EXIT                                                             
*                                                                               
COMERM   DC    C'RECORD NOT FOUND, COMMENT NUMBER'                              
*                                                                               
         EJECT                                                                  
*                                                                               
*                                                                               
*                                  ROUTINE TO GET NEXT ELEM                     
NEXTEL   SR    R0,R0                                                            
         IC    R0,1(R5)                                                         
         AR    R5,R0                                                            
         CLC   ELCODE,0(R5)                                                     
         BCR   8,R9                                                             
         CLI   0(R5),0                                                          
         BNE   *-18                                                             
         LTR   R5,R5                                                            
         BR    R9                                                               
*                                                                               
*                                                                               
*                                  ROUTINE TO GET NEXT INPUT FIELD              
BMPFLD   SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),9                                                          
         BCR   8,R9                                                             
         CLI   0(R2),0                                                          
         BR    R9                                                               
         SPACE 2                                                                
CLRSCRN  DS    0H                                                               
         CLI   0(R2),9             CHEACK EOS                                   
         BNH   CSX                                                              
*                                                                               
         TM    1(R2),X'20'         SKIP IF PROTECTED                            
         BNO   *+12                                                             
         BAS   R9,BMPFLD           BUMP TO NEXT FIELD                           
         B     CLRSCRN                                                          
*                                                                               
         OC    8(L'CMTCM1,R2),BLANKS                                            
         CLC   8(L'CMTCM1,R2),BLANKS                                            
         BE    CS4                                                              
         MVC   8(L'CMTCM1,R2),BLANKS                                            
         FOUT  (R2)                                                             
CS4      DS    0H                                                               
         LA    R2,78(R2)                                                        
         B     CLRSCRN                                                          
CSX      DS    0H                                                               
         BR    RE                                                               
         SPACE 3                                                                
REQ      DS    0H                                                               
*                                                                               
         XC    QCTL,QCTL                                                        
         MVI   QAREA,C' '                                                       
         MVC   QAREA+1(79),QAREA                                                
         MVC   QAREA(2),=C'47'                                                  
         MVC   QAREA+2(2),AGYALPHA                                              
         MVC   QAREA+4(1),CMTMD                                                 
         MVC   QAREA+52(6),WKNO                                                 
         MVC   QAREA+68(7),=C'AUTOREQ'                                          
*                                                                               
         MVI   QCTL+10,47                                                       
         MVI   QCTL+14,106                                                      
         CLC   CMTNO(6),=C'NVTEXT'                                              
         BNE   REQ10                                                            
         MVC   QAREA(2),=C'NT'                                                  
         MVC   QAREA+52(3),KEY+4                                                
         MVC   QAREA+55(3),=3C' '                                               
         MVI   QCTL+10,122        FROM REQTAB IN PRREQ00 (ENTRY FOR NT)         
         B     REQ50                                                            
*                                                                               
REQ10    DS    0H                                                               
         CLC   CMTNO(6),=C'I/OCOM'                                              
         BNE   REQ20                                                            
         MVC   QAREA(2),=C'IC'                                                  
         MVC   QAREA+52(3),KEY+4                                                
         MVC   QAREA+55(3),=3C' '                                               
         MVI   QAREA+61,C'I'                                                    
         MVI   QCTL+10,125        FROM REQTAB IN PRREQ00 (ENTRY FOR IC)         
         B     REQ50                                                            
*                                                                               
REQ20    DS    0H                                                               
         CLC   CMTNO(6),=C'CONCOM'                                              
         BNE   REQ50                                                            
         MVC   QAREA(2),=C'CC'                                                  
         MVC   QAREA+52(3),KEY+4                                                
         MVC   QAREA+55(3),=3C' '                                               
         MVI   QAREA+61,C'C'                                                    
         MVI   QCTL+10,126        FROM REQTAB IN PRREQ00 (ENTRY FOR CC)         
*                                                                               
REQ50    GOTO1 VDATAMGR,DMCB,=C'DMADD',=C'PREQUEST',QCTL,QCTL                   
*                                                                               
         TM    DMCB+8,X'FF'                                                     
         BZ    EXIT                                                             
         SR    R3,R3                                                            
         B     ERROR                                                            
         EJECT                                                                  
*                  INITIALISATION CODE                                          
         SPACE 3                                                                
INITL    LR    R4,RC               SET UP TO CLEAR WORK SPACE                   
         LR    R5,RD                                                            
         SR    R5,R4                                                            
         LR    R0,RE                                                            
         BAS   RE,CLEARWRK                                                      
         LM    R2,R4,0(R1)                                                      
         PACK  AGYNUM,0(1,R1)      AGENCY NUMBER                                
         LH    R5,0(R2)                                                         
         AR    R5,R3                                                            
         ST    R5,FRSTFLD .        A(FIRST INPUT FIELD HEADER)                  
         LH    R5,2(R2)                                                         
         AR    R5,R3                                                            
         ST    R5,LASTFLD .        A(LAST INPUT FIELD)                          
         MVC   NUMFLD,4(R2) .      NUMBER OF FIELDS                             
         ST    R3,VTWA .           A(TWA)                                       
         MVC   VDATAMGR(36),0(R4)  FACILITY LIST                                
         LR    RA,R3                                                            
         STCM  R2,15,ATIOB         SAVE A(TIOB)                                 
         MVC   TERMNAL,0(RA) .     TERMINAL NUMBER                              
         MVC   AGYALPHA,14(RA)     ALPHA AGENCY CODE                            
         LA    R3,64(R3)                                                        
         ST    R3,ERRAREA          PRESET ERRAREA TO A(FIRST HEADER)            
         MVI   DMINBTS,X'C0'       PRESET DATAMGR CONTROL BITS                  
         MVI   DMOUTBTS,X'FD'      PRESET DATAMGR ERROR CHECK BITS              
         LR    RE,R0                                                            
         BR    RE                                                               
         SPACE 2                                                                
CLEARWRK LTR   R5,R5               CLEAR STORAGE TO ZEROS                       
         BCR   8,RE                                                             
         CH    R5,=H'250'                                                       
         BNH   CLEAREST                                                         
         XC    0(250,R4),0(R4)                                                  
         LA    R4,250(R4)                                                       
         SH    R5,=H'250'                                                       
         B     CLEARWRK                                                         
         SPACE 2                                                                
CLEAREST BCTR  R5,R0                                                            
         EX    R5,VARCLEAR                                                      
         BR    RE                                                               
         SPACE 2                                                                
VARCLEAR XC    0(0,R4),0(R4)                                                    
         EJECT                                                                  
*                  FARMABLE CODE                                                
         SPACE 3                                                                
ANY      CLI   5(R2),0                                                          
         BNE   ANY2                                                             
         LA    R3,1                                                             
         B     ERROR                                                            
         SPACE 2                                                                
ANY2     TM    4(R2),X'10' .       IS IT VALID NUMERIC                          
         BCR   8,RE .              IF APPLICABLE                                
         LA    R3,3                                                             
         B     ERROR                                                            
         EJECT                                                                  
*                  COMMUNICATION WITH DATA MANAGER (DIRECTORY)                  
         SPACE 3                                                                
READ     MVC   COMMAND,=C'DMREAD'                                               
         MVC   KEYSAVE,KEY                                                      
         B     DIRCTRY                                                          
         SPACE 2                                                                
SEQ      MVC   COMMAND,=C'DMRSEQ'                                               
         B     DIRCTRY                                                          
         SPACE 2                                                                
HIGH     MVC   COMMAND,=C'DMRDHI'                                               
         MVC   KEYSAVE,KEY                                                      
         B     DIRCTRY                                                          
         SPACE 2                                                                
ADD      MVC   COMMAND,=C'DMADD '                                               
         B     DIRCTRY                                                          
         SPACE 2                                                                
WRITE    MVC   COMMAND,=C'DMWRT '                                               
         B     DIRCTRY                                                          
         SPACE 2                                                                
DIRCTRY  NTR                                                                    
         GOTO1 VDATAMGR,DMCB,(DMINBTS,COMMAND),=C'PRTDIR',             X        
               KEY,KEY,(TERMNAL,0)                                              
         B     DMCHECK                                                          
         EJECT                                                                  
*                  COMMUNICATION WITH DATA MANAGER (FILE)                       
         SPACE 3                                                                
*                                                                               
GETREC   MVC   COMMAND,=C'GETREC'                                               
         B     FILE                                                             
         SPACE 2                                                                
PUTREC   MVC   COMMAND,=C'PUTREC'                                               
         B     FILE                                                             
         SPACE 2                                                                
ADDREC   MVC   COMMAND,=C'ADDREC'                                               
         B     FILE                                                             
         SPACE 2                                                                
FILE     NTR                                                                    
         LA    R2,KEY+27                                                        
         CLC   COMMAND(5),=C'DMDEL'                                             
         BE    *+12                                                             
         CLI   COMMAND,C'A'                                                     
         BNE   *+8                                                              
         LA    R2,KEY                                                           
         GOTO1 VDATAMGR,DMCB,(DMINBTS,COMMAND),=C'PRTFILE',            X        
               (R2),IOAREA,(TERMNAL,DMWORK)                                     
         B     DMCHECK                                                          
         EJECT                                                                  
*                  DATA MANAGER ERRORS AND EXIT                                 
         SPACE 3                                                                
DMCHECK  MVC   BYTE,DMCB+8                                                      
         NC    BYTE,DMOUTBTS                                                    
         BNZ   DMERRS                                                           
         XIT                                                                    
         SPACE 2                                                                
DMERRS   L     RD,4(RD) .          UNWIND WITHOUT XIT                           
         LM    RE,RC,12(RD)                                                     
         SR    R3,R3 .             LET GETMSG SORT IT OUT                       
         B     ERROR                                                            
         EJECT                                                                  
*                  EXITS FROM PROGRAM                                           
         SPACE 3                                                                
LOCK     OI    6(R2),X'02'         LOCK SCREEN                                  
         SPACE 2                                                                
ERROR    L     R4,ERRAREA                                                       
         MVI   ERRAREA,X'FF'                                                    
         MVC   DMCB+20(4),VDATAMGR                                              
         MVC   DMCB+20(1),TERMNAL                                               
         GOTO1 VGETMSG,DMCB+12,((R3),8(R4)),(4,DMCB)                            
         SPACE 2                                                                
EXIT     DS    0H                                                               
*                                                                               
         ICM   RF,15,ATIOB         POINT TO TIOB                                
*                                                                               
         CLI   TIOBAID-TIOBD(RF),0 SKIP IF ENTER HIT                            
         BE    *+12                                                             
         OI    TIOBINDS-TIOBD(RF),TIOBSETC LEAVES CURSOR WHERE IT WAS           
         B     *+8                                                              
         OI    6(R2),OI1C .        INSERT CURSOR                                
*                                                                               
         L     R4,ERRAREA                                                       
         FOUT  (R4)                                                             
*                                                                               
EXXMOD   MVC   SVKEY(10),KEYSAVE                                                
         MVI   CHSW,0                                                           
         XMOD1 1                                                                
         TITLE 'PPCOM00 - COMMENT FILE MAINTENANCE - PFKEYS'                    
***********************************************************************         
*                                                                     *         
*        ANALYZE PFKEYS                                               *         
*              PF4  - INSERT LINE BEFORE CURRENT ONE                  *         
*              PF5  - SPLIT LINE DOWN TO NEXT ONE                     *         
*              PF6 -  JOIN LINE WITH NEXT                             *         
*              PF9  - DELETE LINE                                     *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
PFKEYS   NTR1                                                                   
         ICM   R5,15,ATIOB          ESTABLISH TIOB                              
         USING TIOBD,R5                                                         
*                                                                               
         CLC   CMTAC(3),=C'ADD'    IF ACTION 'ADD'                              
         BNE   PFKEYS1                                                          
*                                                                               
         FOUT  CMTACH,=C'CHANGE '         CHANGE TO 'CHANGE'                    
*                                                                               
PFKEYS1  DS    0H                                                               
*                                                                               
         LA    R6,LNTBL            POINT TO START OF TABLE                      
*                                                                               
         CLC   0(2,R6),TIOBCURD    FIND LINE WITH CURSOR                        
         BE    *+16                                                             
         BH    PFKEYSX             IGNORE IF CURSOR NOT ON A LINE               
         LA    R6,2(R6)            BUMP TO NEXT TABLE ENTRY                     
         B     *-18                                                             
*                                                                               
         MVC   ALINCUR,0(R6)       SAVE A(LINE WITH CURSOR)                     
*                                                                               
         CLI   TIOBAID,4           PF4 OR PF16                                  
         BE    *+8                                                              
         CLI   TIOBAID,16                                                       
         BE    LNADD                  ADD LINE                                  
*                                                                               
         CLI   TIOBAID,5           PF5 OR PF17                                  
         BE    *+8                                                              
         CLI   TIOBAID,17                                                       
         BE    LNADD                  SPLIT LLINE                               
*                                                                               
         CLI   TIOBAID,6           PF6 OR PF18                                  
         BE    *+8                                                              
         CLI   TIOBAID,18                                                       
         BE    LNJOIN                 JOIN LINE                                 
*                                                                               
         CLI   TIOBAID,9           PF9 OR PF21                                  
         BE    *+8                                                              
         CLI   TIOBAID,21                                                       
         BE    LNDEL                  DELETE LINE                               
*                                                                               
         B     PFKEYSX             IGNORE ALL OTHER KEYS                        
*                                                                               
         TITLE 'PPCOM00 - COMMENT FILE MAINTENANCE - LNADD'                     
***********************************************************************         
*                                                                     *         
*        ADD/CHANGE A LINE                                            *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
LNADD    DS    0H                  ADD LINE BEFORE CURSOR LINE                  
*                                                                               
         CLC   CMTCML,BLANKS       LAST LINE MUST BE EMPTY                      
         BNH   LNADD1                                                           
*                                                                               
         LA    R3,LNADDERR         NO ROOM ON SCREEN                            
         B     PFKEYERX                                                         
*                                                                               
LNADD1   DS    0H                                                               
*                                                                               
         LA    R6,LNTBLLS          POINT TO LAST ENTRY IN TABLE                 
         LR    R7,R6                                                            
         SH    R7,=H'2'            BACK UP A TABLE ENTRY                        
*                                                                               
LNADDLP  DS    0H                                                               
*                                                                               
         LH    RF,0(R6)                                                         
         LA    RF,0(RF,RA)         POINT TO LINES IN TWA                        
         LH    RE,0(R7)                                                         
         LA    RE,0(RE,RA)                                                      
*                                                                               
         MVC   8(L'CMTCM1,RF),8(RE)   COPY LINE DOWN ONE                        
         MVC   4(2,RF),4(RE)       COPY INPUT INDICATORS AND LENGTH             
         OI    6(RF),X'80'         TRANSMIT FIELD                               
         MVC   7(1,RF),5(RF)       SET OUTPUT LENGTH                            
*                                                                               
LNADDCN  DS    0H                                                               
*                                                                               
         LR    R6,R7               BACK UP ENTRIES IN TABLE                     
         SH    R7,=H'2'                                                         
*                                                                               
         CLC   0(2,R7),ALINCUR     STOP IF PASSED LINE WITH CURSOR              
         BNL   LNADDLP                                                          
*                                                                               
LNADDDN  DS    0H                                                               
*                                                                               
         SR    R6,R6                                                            
         ICM   R6,3,ALINCUR        POINT TO LINE WITH CURSOR                    
         AR    R6,RA               POINT TO FIELD HEADER                        
*                                                                               
         CLI   TIOBAID,5           SKIP IF SPLITTING LINE                       
         BE    *+8                                                              
         CLI   TIOBAID,17                                                       
         BE    LNADDSP                                                          
*                                                                               
         MVC   8(L'CMTCM1,R6),BLANKS    CLEAR FIELD                             
         MVI   5(R6),0             INDICATE NO INPUT                            
         OI    6(R6),X'80'         TRANSMIT FIELD                               
         MVI   7(R6),0             INDICATE NO OUTPUT                           
*                                                                               
         B     PFKEYSX                                                          
*                                                                               
LNADDSP  DS    0H                                                               
*                                                                               
         LA    RF,L'CMTCM1-1+8(R6) POINT TO LAST BYTE OF FIELD                  
*                                                                               
         SR    RE,RE                                                            
         IC    RE,TIOBCURI         DISPLACEMENT OF CURSOR INTO FLD              
         LA    RE,8(RE,R6)         CURSOR ADDRESS                               
*                                                                               
         CR    RF,RE               DONE IF CURSOR PASSED                        
         BL    *+12                                                             
         MVI   0(RF),C' '          CLEAR OUT END OF FIELD                       
         BCT   RF,*-10                                                          
*                                                                               
         LA    RE,CMTCM2H-CMTCM1H(RE)      POINT TO CURSOR IN NEXT LINE         
         LA    RF,8+CMTCM2H-CMTCM1H(R6)    START OF NEXT FIELD                  
*                                                                               
         CR    RF,RE               CLEAR OUT TO CURSOR                          
         BNL   *+16                                                             
         MVI   0(RF),C' '                                                       
         LA    RF,1(RF)            NEXT POSITION                                
         B     *-14                                                             
*                                                                               
         OI    6(R6),X'80'         TRANSMIT SPLIT LINE - OTHERS DONE            
*                                                                               
         B     PFKEYSX                                                          
*                                                                               
         TITLE 'PPCOM00 - COMMENT FILE MAINTENANCE - LNDEL'                     
***********************************************************************         
*                                                                     *         
*        DELETE A LINE                                                *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
LNDEL    DS    0H                  ADD LINE BEFORE CURSOR LINE                  
*                                                                               
*        ON ENTRY R6==> TABLE ENTRY FOR LINE WITH CURSOR                        
*                                                                               
         LR    R7,R6                                                            
         AH    R7,=H'2'            POINT TO NEXT TABLE ENTRY                    
*                                                                               
LNDELLP  DS    0H                                                               
*                                                                               
         CLC   0(2,R7),LNTBLLS     STOP IF PASSED END OF TABLE                  
         BH    LNDELDN                                                          
*                                                                               
         LH    RF,0(R6)                                                         
         LA    RF,0(RF,RA)         POINT TO LINES IN TWA                        
         LH    RE,0(R7)                                                         
         LA    RE,0(RE,RA)                                                      
*                                                                               
         MVC   8(L'CMTCM1,RF),8(RE)   COPY LINE UP ONE                          
         MVC   4(2,RF),4(RE)       COPY INPUT INDICATORS AND LENGTH             
         OI    6(RF),X'80'         TRANSMIT FIELD                               
         MVC   7(1,RF),5(RF)       SET OUTPUT LENGTH                            
*                                                                               
LNDELCN  DS    0H                                                               
*                                                                               
         LR    R6,R7               ADVANCE ENTRIES IN TABLE                     
         AH    R7,=H'2'                                                         
*                                                                               
         B     LNDELLP                                                          
*                                                                               
LNDELDN  DS    0H                                                               
*                                                                               
         SR    R6,R6                                                            
         ICM   R6,3,LNTBLLS        POINT TO LAST LINE IN TABLE                  
         AR    R6,RA               POINT TO FIELD HEADER                        
*                                                                               
         MVC   8(L'CMTCM1,R6),BLANKS    CLEAR LAST LINE                         
         MVI   5(R6),0             INDICATE NO INPUT                            
         OI    6(R6),X'80'         TRANSMIT FIELD                               
         MVI   7(R6),0             INDICATE NO OUTPUT                           
*                                                                               
         B     PFKEYSX                                                          
*                                                                               
         TITLE 'PPCOM00 - COMMENT FILE MAINTENANCE - LNJOIN'                    
***********************************************************************         
*                                                                     *         
*        JOIN LINE WITH CURSOR TO ONE BELOW                           *         
*              FIND LAST NON-BLANK AND FILL REMAINDER OF LINE WITH    *         
*              DATA FROM NEXT LINE :*                                           
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
LNJOIN   DS    0H                                                               
*                                                                               
*        ON ENTRY R6==> TABLE ENTRY FOR LINE WITH CURSOR                        
*                                                                               
         LH    RF,0(R6)            POINT TO CURSOR LINE                         
         LA    RF,0(RF,RA)                                                      
         OI    6(RF),X'80'         FORCE RE-DISPLAY OF LINE                     
*                                                                               
*        MOVE RIGHT PART OF LINE OVER TO CURSOR                                 
*                                                                               
         ICM   RE,15,ATIOB         GET DISPLACEMENT OF CURSOR INTO LINE         
         SR    R1,R1                                                            
         ICM   R1,1,TIOBCURI-TIOBD(RE)                                          
*                                                                               
         LA    R1,8(R1,RF)         POINT TO CURSOR POSITION                     
*                                                                               
         LA    RF,L'CMTCM1-1+8(RF) POINT TO LAST BYTE OF CURSOR LINE            
*                                                                               
         CLI   0(R1),C' '          FIND FIRST BLANK AT OR AFTER CURSOR          
         BNH   *+18                                                             
         LA    R1,1(R1)                                                         
         CR    R1,RF               CHECK FOR END OF LINE                        
         BL    *-14                                                             
         B     LNJOIN0             LINE FULL                                    
*                                                                               
         LA    R1,1(R1)            MOVE EVERYTHING UP TO NEXT POSITION          
*                                                                               
         CR    R1,RF               DONE IF END OF LINE REACHED                  
         BH    LNJOIN0                                                          
*                                                                               
         CLI   0(R1),C' '          DONE IF NON-BLANK                            
         BH    LNJOIN0                                                          
*                                                                               
         LA    RE,1(R1)            SEARCH FOR NEXT NON-BLANK                    
*                                                                               
         CR    RE,RF               DONE IF END OF LINE REACHED                  
         BH    LNJOIN0                                                          
         CLI   0(RE),C' '                                                       
         BH    *+12                                                             
         LA    RE,1(RE)            BUMP POINTER                                 
         B     *-18                                                             
*                                                                               
         MVC   0(1,R1),0(RE)       MOVE RIGHT END OVER TO THE LEFT              
         LA    R1,1(R1)                                                         
         LA    RE,1(RE)                                                         
         CR    RE,RF                                                            
         BNH   *-16                                                             
*                                  END OF LINE REACHED                          
         MVI   0(R1),C' '          FILL OUT REST OF LINE WITH BLANKS            
         LA    R1,1(R1)                                                         
         CR    R1,RF                                                            
         BNH   *-10                                                             
*                                                                               
LNJOIN0  DS    0H                                                               
*                                  RF POINTS TO LAST BYTE OF LINE               
         LA    R0,L'CMTCM1         NUMBER OF BYTES ON LINE                      
*                                                                               
         CLI   0(RF),C' '          IF LINE FULL NOTHING TO DO                   
         BH    LNJOIN1                                                          
*                                                                               
         CLI   0(RF),C' '          FIND LAST NON-BLANK                          
         BH    *+14                                                             
         BCTR  RF,0                BACK UP A BYTE                               
         BCT   R0,*-10                                                          
         B     LNJOIN1             EMPTY LINE NO SPACE TO START                 
*                                                                               
         LA    RF,1(RF)            BUMP TO FIRST AVAILABLE SPOT                 
         MVI   1(RF),C' '          FORCE SPACE SEPARATOR                        
         AH    R0,=H'1'            RECTIFY BYTE COUNT                           
*                                                                               
LNJOIN1  DS    0H                                                               
*                                                                               
         LA    RF,1(RF)            BUMP TO FIRST AVAILABLE SPOT                 
*                                                                               
         CLC   2(2,R6),=X'FFFF'    SKIP IF AT END OF TABLE                      
         BE    LNJOINX                                                          
*                                                                               
         LH    RE,2(R6)            DISPLACEMENT OF NEXT LINE                    
         LA    RE,0(RE,RA)         START OF NEXT LINE                           
         OI    6(RE),X'80'         FORCE RE-DISPLAY OF LINE                     
         LA    RE,8(RE)            FIRST BYTE OF LINE                           
*                                                                               
         LA    R2,L'CMTCM1         LENGTH OF LINE                               
*                                                                               
         CLI   0(RE),C' '          FIND FIRST NON-SPACE                         
         BH    *+16                                                             
         LA    RE,1(RE)                                                         
         BCT   R2,*-12                                                          
         B     LNJOINX              LINE IS BLANK                               
*                                                                               
         LR    R7,RE               SAVE DATA START ADDRESS                      
*                                                                               
         LA    R1,L'CMTCM1         CALCULATE SPACE LEFT ON CURSOR LINE          
*                                                                               
         SR    R1,R0                                                            
         BNP   LNJOIN3             NO ROOM ON CURSOR LINE                       
*                                                                               
         CR    R2,R1                                                            
         BH    *+10                                                             
         LR    R1,R2               USE SMALLER LENGTH                           
         B     LNJOIN2             GO MOVE EVERYTHING                           
*                                                                               
         LA    RE,0(R1,RE)         POINT TO LAST BYTE PLUS ONE                  
*                                    TO BE MOVED TO CURSOR LINE                 
*                                                                               
         CLI   0(RE),C' '          MUST BE A SPACE                              
         BNH   *+14                                                             
         BCTR  RE,0                BACK UP A BYTE                               
         BCT   R1,*-10                                                          
         B     LNJOIN3             NO ROOM TO MOVE ANYTHING                     
*                                                                               
LNJOIN2  DS    0H                                                               
*                                  R1 HAS NUMBER OF BYTES TO MOVE               
         BCTR  R1,0                DECREMENT FOR EXECUTE                        
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),0(R7)       MOVES NEXT LINE TO CURSOR LINE               
*                                                                               
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R7),BLANKS      CLEARS START OF NEXT LINE                    
*                                                                               
         LA    R1,1(R1)            RESTORE BYTE COUNTER                         
*                                                                               
*                                  R2 HAS REMAINING BYTES ON LINE               
*                                                                               
LNJOIN3  DS    0H                                                               
*                                                                               
         SR    R2,R1               NUMBER OF BYTES TO BE JUSTIFIED              
         BNP   LNJOINX             NONE - ALL MOVED                             
*                                                                               
         CLI   0(RE),C' '          FIND NEXT NON-BLANK                          
         BH    *+16                                                             
         LA    RE,1(RE)            BUMP POINTER                                 
         BCT   R2,*-12             CONTINUE IF MORE ON LINE                     
         B     LNJOINX             NOTHING LEFT ON LINE TO MOVE                 
*                                                                               
         LH    RF,2(R6)            DISPLACEMENT OF NEXT LINE                    
         LA    RF,8(RF,RA)         START OF NEXT LINE                           
*                                                                               
         BCTR  R2,0                DECREMENT FOR EXECUTE                        
         EX    R2,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),0(RE)       LEFT JUSTIFIES 'NEXT' LINE                   
*                                                                               
         LA    RF,1(R2,RF)         END OF WHAT WAS JUSTIFIED                    
*                                                                               
         LA    R2,1(R2)            RESTORE BYTE COUNTER                         
*                                                                               
         LA    R1,L'CMTCM1                                                      
         SR    R1,R2               AMOUNT TO BE CLEARED AFTERWARDS              
         BNP   LNJOINX             NOTHING                                      
*                                                                               
         BCTR  R1,0                DECREMENT FOR EXECUTE                        
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),BLANKS      CLEAR OUT REST OF 'NEXT' LINE                
*                                                                               
LNJOINX  DS    0H                                                               
*                                                                               
         EJECT                                                                  
         TITLE 'PPCOM00 - COMMENT FILE MAINTENANCE - EXITS'                     
***********************************************************************         
*                                                                     *         
*        EXIT ROUTINES                                                *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
PFKEYSX  DS    0H                  NORMAL EXIT                                  
         CR    RB,RB               FORCE EQ CC                                  
         XIT1                                                                   
*                                                                               
PFKEYERX DS    0H                  ERROR EXIT                                   
*                                                                               
         LTR   RB,RB               FORCE NE CC                                  
         XIT1  REGS=(R3)           RETURN R3                                    
*                                                                               
         TITLE 'PPCOM00 - COMMENT FILE MAINTENANCE - LNTBL'                     
***********************************************************************         
*                                                                     *         
*        TABLE OF LINE DISPLACEMENTS INTO SCREEN                      *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
LNTBL    DS    0D                                                               
         DC    Y(CMTCM1H-T408FFD)   COMMENT 1                                   
         DC    Y(CMTCM2H-T408FFD)   COMMENT 2                                   
         DC    Y(CMTCM3H-T408FFD)   COMMENT 3                                   
         DC    Y(CMTCM4H-T408FFD)   COMMENT 4                                   
         DC    Y(CMTCM5H-T408FFD)   COMMENT 5                                   
         DC    Y(CMTCM6H-T408FFD)   COMMENT 6                                   
         DC    Y(CMTCM7H-T408FFD)   COMMENT 7                                   
         DC    Y(CMTCM8H-T408FFD)   COMMENT 8                                   
         DC    Y(CMTCM9H-T408FFD)   COMMENT 9                                   
         DC    Y(CMTCM10H-T408FFD)  COMMENT 10                                  
         DC    Y(CMTCM11H-T408FFD)  COMMENT 11                                  
         DC    Y(CMTCM12H-T408FFD)  COMMENT 12                                  
         DC    Y(CMTCM13H-T408FFD)  COMMENT 13                                  
         DC    Y(CMTCM14H-T408FFD)  COMMENT 14                                  
         DC    Y(CMTCM15H-T408FFD)  COMMENT 15                                  
LNTBLLS  DC    Y(CMTCMLH-T408FFD)   COMMENT LAST                                
         DC    4X'FF'               END OF TABLE                                
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*       *************************                                               
******  TEST OFFICE LIST SECURITY  ******                                       
*       *************************                                               
         SPACE 2                                                                
*                                                                               
PPCLIVER NTR1                   *****  LIMIT ACCESS TESTING   *****             
         XC    WORK,WORK          WORK MUST BE AT LEAST 48 BYTES                
         LA    R1,WORK            (LENGTH OF OFFICED IS 48 BYTES)               
         USING OFFICED,R1                                                       
*                                                                               
         MVI   OFCSYS,C'P'                                                      
         L     RF,VTWA                                                          
         MVC   OFCAUTH,6(RF)                                                    
         MVC   OFCAGY,AGYALPHA                                                  
         MVC   OFCOFC,TSTOFF       TSTOFF JUST HOLDING OFFICE HERE              
         MVC   OFCOFC2,TSTOFF2     TSTOFF2 JUST HOLDING OFFICE HERE             
         MVC   OFCCLT,PCLTKCLT                                                  
         OC    OFCCLT,=3C' '                                                    
         MVC   OFCPMED,PCLTKMED                                                 
         MVC   OFCLMT(4),6(RF)                                                  
         LA    R0,SECBLK                                                        
         ST    R0,OFCSECD         A("SECRET BLOCK")                             
         DROP  R1                                                               
*                                                                               
         GOTOR VOFFICER,DMCB,(C'2',WORK),(0,ACOMFACS),0                         
         CLI   0(R1),0                                                          
         XIT1                                                                   
*                                                                               
         SPACE 2                                                                
BLANKS   DC    70C' '                                                           
ACOMFACS DS    A                   COMFACS                                      
VOFFICER DS    V                   A(OFFICER)                                   
*                                                                               
         SPACE 2                                                                
OFCCHG   NTR1                   **** CONVERT 2-CHAR OFFICE TO 1 *****           
         XC    WORK,WORK          WORK MUST BE AT LEAST 48 BYTES                
         LA    R1,WORK            (LENGTH OF OFFICED IS 48 BYTES)               
         USING OFFICED,R1                                                       
*                                                                               
         MVI   OFCSYS,C'P'                                                      
         MVC   OFCAGY,AGYALPHA                                                  
         MVC   OFCOFC2,TSTOFF2     TSTOFF JUST HOLDING OFFICE HERE              
         DROP  R1                                                               
*                                                                               
         GOTOR VOFFICER,DMCB,(C'2',WORK),(0,ACOMFACS),0                         
         CLI   0(R1),0                                                          
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
CKTRAFID NTR1  BASE=*,LABEL=*      CHECKING FOR TRAFFIC ID SIGN-ON              
*                                                                               
         LA    R0,IOAREA                                                        
         LHI   R1,1600                                                          
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         LA    R4,IOAREA                                                        
         USING CTIREC,R4                                                        
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKEY,C'I'                                                      
         MVC   CTIKNUM,10(RA)      ID NUMBER                                    
*                                                                               
         GOTO1 VDATAMGR,DMCB,(0,=C'DMREAD'),=C'CTFILE',(R4),(R4)                
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    RE,CTIDATA                                                       
CKTRA10  CLI   0(RE),0                                                          
         BNE   *+6                                                              
         DC    H'0'                ELEM MUST BE PRESENT                         
         CLI   0(RE),CTAGYELQ      AGENCY ALPHA ID ELEMENT (X'06')              
         BE    CKTRA20                                                          
         SR    R1,R1                                                            
         IC    R1,1(RE)                                                         
         AR    RE,R1                                                            
         B     CKTRA10                                                          
*                                                                               
CKTRA20  DS    0H                                                               
         USING CTAGYD,RE                                                        
         CLI   CTAGYIDT,CTAGYTTQ   TRAFFIC ID (C'T')?                           
         BNE   CKTRIDER                                                         
         DROP  R4,RE                                                            
*                                                                               
CKTRIDX  DS    0H                                                               
         CR    RB,RB               EQUAL                                        
         B     *+6                                                              
CKTRIDER LTR   RB,RB               NOT EQUAL (SIGN-ON IS NOT TRAFFIC)           
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE FLDIND                                                         
*                                                                               
       ++INCLUDE GENOLD                                                         
         EJECT                                                                  
         ORG   IOAREA                                                           
       ++INCLUDE PCOMREC                                                        
         EJECT                                                                  
         ORG   IOAREA                                                           
       ++INCLUDE PNVTREC                                                        
         EJECT                                                                  
         ORG   IOAREA                                                           
       ++INCLUDE PIOCREC                                                        
         EJECT                                                                  
         ORG   IOAREA                                                           
       ++INCLUDE PCONCREC                                                       
         EJECT                                                                  
         PRINT OFF                                                              
         ORG   IOAREA                                                           
       ++INCLUDE PAGYREC                                                        
         EJECT                                                                  
         ORG   IOAREA                                                           
       ++INCLUDE PCLTREC                                                        
         ORG   IOAREA                                                           
       ++INCLUDE CTGENFILE                                                      
         EJECT                                                                  
*                                                                               
         PRINT ON                                                               
       ++INCLUDE PPCOMFFD                                                       
*                                                                               
         EJECT                                                                  
       ++INCLUDE PCMTTWA                                                        
         PRINT OFF                                                              
         EJECT                                                                  
       ++INCLUDE FATIOB                                                         
         EJECT                                                                  
       ++INCLUDE DDOFFICED                                                      
         EJECT                                                                  
       ++INCLUDE FASECRETD                                                      
         EJECT                                                                  
       ++INCLUDE DDCOMFACS                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'099PPCOM00   12/21/11'                                      
         END                                                                    
