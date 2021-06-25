*          DATA SET PPSR210    AT LEVEL 007 AS OF 03/25/11                      
*PHASE T42110A                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         TITLE 'T42110 - PRINTPAK ENHANCE SPACE RESERVATION PROCESSOR'          
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* KWAN 03/22/11 ESR BY ESTIMATE PERIOD                                          
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
T42110   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 PPSR210X-PPSR210D,T42110,RR=RE,CLEAR=YES                         
*                                                                               
         LR    R6,RC                                                            
         USING PPSR210D,R6                                                      
*                                                                               
         ST    RE,RELO10                                                        
         L     RC,0(R1)                                                         
         USING POLWRKD,RC                                                       
         USING T421FFD,RA                                                       
         USING ESRWORKD,R8                                                      
         L     R9,PPFILEC                                                       
         LA    R7,1(R9)                                                         
         LA    R7,4095(R7)                                                      
         USING POLFILED,R9,R7                                                   
*                                                                               
         CLI   8(R1),RPYESRHQ      REPLY ESR HEADER?                            
         BE    REPLYHDR                                                         
         CLI   8(R1),MRKBUYRQ      MARK BUY RECORD?                             
         BE    WRITBUYR                                                         
*                                                                               
         DC    H'0'                INVALID CALL                                 
*                                                                               
REPLYHDR BRAS  RE,RPYESRHD         REPLY ESR HEADER                             
         BRAS  RE,ESRHCOMM         REPLY ESR HEADER COMMENT                     
         B     EXXMOD                                                           
*                                                                               
WRITBUYR BRAS  RE,MARKBUYR         MARK BUY RECORD                              
         B     EXXMOD                                                           
*                                                                               
EXXMOD   XMOD1 1                                                                
*                                                                               
SETCCEQ  CR    RB,RB               EQUAL                                        
         J     *+6                                                              
SETCCNEQ LTR   RB,RB               NOT EQUAL                                    
EXIT     XIT1                                                                   
*                                                                               
NXTEL    SR    R0,R0                                                            
         IC    R0,1(R2)                                                         
         AR    R2,R0                                                            
         CLC   ELCODE,0(R2)                                                     
         JE    NXTELX              CC IS EQUAL                                  
         CLI   0(R2),0                                                          
         JNE   NXTEL                                                            
         LTR   R2,R2               CC IS NOT EQUAL                              
NXTELX   BR    RE                                                               
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
MARKBUYR NTR1  BASE=*,LABEL=*      MARK BUY RECORD                              
*                                                                               
         CLI   RCWRITE,C'Y'                                                     
         JNE   EXIT                                                             
*                                                                               
         XC    WKINSCNT,WKINSCNT                                                
         LA    R5,BUYDALST                                                      
         MVC   TMPFULL1,AREC       SAVE ORIGINAL AIO POINTER                    
         LA    RE,PBUYREC                                                       
         ST    RE,AREC                                                          
*                                                                               
M_BUY20  OC    0(4,R5),0(R5)       NO MORE INSERTIONS?                          
         BZ    M_BUY_X                                                          
         MVC   KEY+27(4),0(R5)     INSERTION DISK ADDRESS                       
         GOTOR GETPRT                                                           
*                                                                               
         ICM   RE,15,WKINSCNT                                                   
         AHI   RE,1                                                             
         STCM  RE,15,WKINSCNT                                                   
         CHI   RE,BUYDAMXQ                                                      
         BL    *+6                                                              
         DC    H'0'                TABLE MAX ERROR ENCOUNTERED                  
*                                                                               
         BRAS  RE,BLDESREL         BUILD ESR ELEM                               
         L     RE,AREC                                                          
         CLI   33(RE),X'20'                                                     
         BE    *+6                                                              
         DC    H'0'                INVALID BUY RECORD                           
         GOTOR PUTPRT                                                           
*                                                                               
         LA    R5,4(R5)            NEXT DISK ADDRESS                            
         B     M_BUY20                                                          
*                                                                               
M_BUY_X  MVC   AREC,TMPFULL1       RESTORE ORIGINAL AIO POINTER                 
         J     EXIT                                                             
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
BLDESREL NTR1  BASE=*,LABEL=*      BUILD ESR ELEM                               
*                                                                               
         LA    R2,PBUYREC+33                                                    
         CLI   0(R2),X'20'                                                      
         BE    *+6                                                              
         DC    H'0'                INVALID BUY RECORD                           
*                                                                               
         NI    WKESRFLG,X'FF'-(E_SRBMFQ+E_SRBMTQ)                               
         LA    R2,PBUYREC+33                                                    
         USING PBYMELEM,R2                                                      
         MVI   ELCODE,PBYMELCQ     BUY MOVE ELEM CODE                           
ESREL02  BRAS  RE,NXTEL                                                         
         BNE   ESREL03                                                          
         TM    PBYMSTAT,PBYMFROQ   MOVED "FROM"?                                
         BZ    *+8                                                              
         OI    WKESRFLG,E_SRBMFQ   CONTAIN BUY MOVE "FROM" ELEM                 
         TM    PBYMSTAT,PBYMTO_Q   MOVED "TO"?                                  
         BZ    *+8                                                              
         OI    WKESRFLG,E_SRBMTQ   CONTAIN BUY MOVE "TO"   ELEM                 
         B     ESREL02                                                          
*                                                                               
ESREL03  LA    R2,PBUYREC+33                                                    
*                                                                               
         MVI   ELCODE,PESRELCQ                                                  
         BRAS  RE,NXTEL                                                         
         BNE   *+8                                                              
         B     *-12                PASS LAST ESR ELEM                           
*                                                                               
         XC    TMPELEM,TMPELEM                                                  
         LA    R3,TMPELEM                                                       
         USING PESRELEM,R3                                                      
         MVI   PESRELCO,PESRELCQ ELEM CODE                                      
         MVI   PESRELLN,PESRELLQ ELEM LENGTH                                    
         MVC   PESRDATE,BTODAY                                                  
         MVC   PESR#YER,SVESR_YR                                                
         MVC   PESR#SQ#,SVESR_SQ                                                
         MVC   PESR#REV,SVESR_R#                                                
*                                                                               
         BRAS  RE,GETMODCD         GET MODIFICATION CODE FROM TABLE             
         MVC   PESRMODC,TMPWK1                                                  
*                                                                               
         MVI   PESRMIND,C'A'       ADBUYER METHOD                               
*                                                                               
         MVC   PESRINDT,PBUYKDAT   INSERTION DATE                               
         CLI   PBUYKMED,C'N'                                                    
         BE    *+14                                                             
         MVC   PESRSPAC,PBDSPACE                                                
         B     ESREL30                                                          
*                                                                               
         ZAP   PESRUNIT,PBDUNITS                                                
         ZAP   PESRCOLM,PBDCLMS                                                 
         MVC   PESRUIND,PBDUIND                                                 
         MVC   PESR#COL,PBDCL                                                   
         MVC   PESRSAUN,PBDSPACE                                                
*                                                                               
ESREL30  MVC   PESRADCD,PBDJOB                                                  
         CLI   ZEOPT,C'Y'          MULTIPLE ZONE/EDITION?                       
         BNE   *+8                                                              
         OI    PESRSTAT,X'80'                                                   
*                                                                               
         TM    WKESRFLG,E_SRBMTQ   BUY MOVE "TO" ELEM PRESENT?                  
         BZ    *+8                                                              
         OI    PESRSTAT,PESRDBMQ   DELETED BUY MOVE CHANGE RESV ISSUED          
*                                                                               
         TM    WKESRFLG,E_SRBMFQ   BUY MOVE "FROM" ELEM PRESENT?                
         BZ    *+8                                                              
         OI    PESRSTAT,PESRBMOQ   BUY MOVE INSERTION RESV ISSUED               
*                                                                               
         CLI   SRESVTYP,SRT1ESTQ   SPACE RESERVATION BY ESTIMATE?               
         BE    *+12                                                             
         CLI   SRESVTYP,SRTSTWEQ   STEWARD SPACE RESERVATION BY EST?            
         BNE   *+8                                                              
         OI    PESRSTAT,PESRESTQ   SPACE RESERVATION BY EST                     
*                                                                               
         CLI   SRESVTYP,SRT1ESPQ   ESR BY ESTIMATE PERIOD?                      
         JE    *+12                                                             
         CLI   SRESVTYP,SRT1SWPQ   STEWARDSHIP ESR BY ESTIMATE PERIOD?          
         JNE   *+8                                                              
         OI    PESRSTAT,PESREPRQ   ESR BY ESTIMATE PERIOD                       
*                                                                               
         GOTOR VRECUP,DMCB,(1,PBUYREC),TMPELEM,(R2)                             
*                                                                               
         BRAS  RE,NXTEL            BY PASS NEW ESR ELEM                         
*                                                                               
         GOTOR DATCON,DMCB,(3,BTODAY),(2,HALF2)                                 
*                                                                               
         LA    RE,TMPWK1           CREATE A CHG ELEM RIGHT AFTER                
         USING PCHGELEM,RE                                                      
         XC    TMPWK1,TMPWK1                                                    
         MVI   PCHGELCD,PCHGELEQ                                                
         MVI   PCHGLEN,PCHGNEWS                                                 
         MVC   PCHGDAT,HALF2       COMPRESSED TODAY'S DATE                      
         LA    RF,PCHG_XSS         START OF EXTENSION FOR SHORT ELEM            
         USING PCHGEXT,RF                                                       
         MVC   PCHGPID,SVSR2PID    PID                                          
         OI    PCHGIND5,X'80'      ESR IO IS GENERATED                          
*                                                                               
         GOTOR VRECUP,DMCB,(1,PBUYREC),TMPWK1,(R2)                              
*                                                                               
WIOELX   J     EXIT                                                             
*                                                                               
         LTORG                                                                  
         DROP  RB,R3,RE,RF                                                      
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
GETMODCD NTR1  BASE=*,LABEL=*      GET MODIFICATION CODE FROM TABLE             
*                                                                               
         XC    TMPWK1,TMPWK1       INIT RETURN VALUE                            
         LA    R2,PBUYREC+33                                                    
         MVI   ELCODE,X'99'                                                     
         BRAS  RE,NXTEL                                                         
         BE    *+6                                                              
         DC    H'0'                INSERTION SHOULD HAVE SERIAL#                
         USING PSERELEM,R2                                                      
         L     R4,ASER#TAB         POINT TO TABLE OF SERIAL#S                   
         USING SER#TABD,R4                                                      
         SR    RE,RE                                                            
         ICM   RE,3,NUMSER#S                                                    
GTMC20   CP    PSERNUM,S#SERIAL    FOUND IN TABLE?                              
         BE    GTMC30                                                           
         LA    R4,SER#TBLQ(R4)                                                  
         BCT   RE,GTMC20                                                        
         DC    H'0'                SERIAL# HAS TO BE IN TABLE!                  
*                                                                               
GTMC30   MVC   TMPWK1(L'S#MODCOD),S#MODCOD                                      
         OC    TMPWK1,TMPWK1                                                    
         BNZ   *+6                                                              
         DC    H'0'                MODIFICATION NOT IN TABLE!                   
         J     EXIT                                                             
*                                                                               
         LTORG                                                                  
         DROP  RB,R2,R4                                                         
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* TMPWK1 = FAX CODE TO BE LOOKED UP, WILL RETURN NUMBER IF FOUND                
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
TFAXCODE NTR1  BASE=*,LABEL=*      TRANSLATE FAX CODE TO NUMBER                 
*                                                                               
         OC    TMPWK1+3(7),TMPWK1+3                                             
         BNZ   *+14                                                             
         MVC   TMPWK1(L'CTFX1NUM),SPACES                                        
         B     TFXC_X              NO VALID FAX CODE IS FOUND                   
         XC    KEY,KEY                                                          
         LA    RE,KEY                                                           
         USING CTFXKEY,RE                                                       
         MVI   CTFXKTYP,CTFXEQU    RECORD TYPE                                  
         MVC   CTFXAGY,QAGENCY                                                  
         MVC   CTFXCODE,TMPWK1+3                                                
         OC    CTFXCODE,SPACES     LEFT JUST AND SPACE PADDED                   
         DROP  RE                                                               
*                                                                               
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,(0,=C'DMREAD'),=C'CTFILE',KEY,TMPWKAIO              
*                                                                               
         MVC   TMPWK1(L'CTFX1NUM),SPACES                                        
         CLC   KEY(18),KEYSAVE     COMPARE UP TO FAX CODE                       
         BNE   TFXC_X              FAX CODE IS NOT FOUND IN CNTL FILE           
         LA    R2,TMPWKAIO+28      POINT TO FIRST ELEM IN CNTL FILE             
         MVI   ELCODE,CTFX1ELQ                                                  
         CLC   ELCODE,0(R2)        FIRST ELEM IS CNTL FILE FAX NUM?             
         BE    *+12                                                             
         BRAS  RE,NXTEL                                                         
         BNE   TFXC_X                                                           
         ZIC   RE,1(R2)                                                         
         AHI   RE,-3               2 FOR ELEM OVERHEAD AND 1 FOR EX             
         CHI   RE,0                                                             
         BNL   *+6                                                              
         DC    H'0'                BAD ELEM LENGTH IN CNTL FILE                 
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   TMPWK1(0),2(R2)     GET FAX NUMBER IN CNTL FILE                  
*                                                                               
TFXC_X   J     EXIT                                                             
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
RPYESRHD NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         TM    ADBSW,AS_ESRUQ      ESR UPLOAD?                                  
         JZ    EXIT                                                             
         TM    ADRPYREC,RPYHEADQ   HEADER ALREADY REPLIED?                      
         JNZ   EXIT                                                             
*                                                                               
         L     R4,AESRDSST         POINT TO ESR STORAGE BLOCK                   
         USING ESRDSD,R4           ESR STORAGE BLOCK                            
         BRAS  RE,SETESRHD         SET ESR HEADER VALUES                        
*                                                                               
         L     R3,AWRKREC                                                       
         USING LIOBD,R3            LINKIO INTERFACE BLOCK                       
*                                                                               
         GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTMAP',E#SR2HDR)              
         OI    ADRPYREC,RPYHEADQ                                                
*                                                                               
         GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#ESRLKY),    +        
               ('LD_CHARQ',H_SRLKEY),('H_SRLKYL',0)                             
*                                                                               
         GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#YEARYY),    +        
               ('LD_CHARQ',SVYEARYY),(L'SVYEARYY,0)                             
*                                                                               
         CLC   LIOBPCV1,=AL1(03,04,00,05)                                       
         BL    ESRH140                                                          
         CLI   SRESVTYP,C' '                                                    
         BNH   ESRH140                                                          
         MVC   TMPBYTE1,SRESVTYP                                                
         CLI   TMPBYTE1,SRTSTWEQ                                                
         BNE   *+8                                                              
         MVI   TMPBYTE1,SRTSTEWQ                                                
         GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#INSCLA),    +        
               ('LD_CHARQ',TMPBYTE1),(L'SRESVTYP,0)                             
*                                                                               
ESRH140  GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#IOSEQ#),    +        
               ('LD_UBINQ',SVESR_SQ),(L'SVESR_SQ,0)                             
*                                                                               
         GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#REVNUM),    +        
               ('LD_UBINQ',SVESR_R#),(L'SVESR_R#,0)                             
*                                                                               
         GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#AGYNAM),    +        
               ('LD_CHARQ',PAGYNAME),(L'PAGYNAME,0)                             
         GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#AGYADR),    +        
               ('LD_CHARQ',PAGYADDR),(L'PAGYADDR,0)                             
*                                                                               
         GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#ALPHID),    +        
               ('LD_CHARQ',QAGENCY),(L'QAGENCY,0)                               
*                                                                               
         GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#MEDCOD),    +        
               ('LD_CHARQ',QMEDIA),(L'QMEDIA,0)                                 
*                                                                               
         MVC   TMPWK1(17),SPACES                                                
         GOTOR DATCON,DMCB,(0,QSTART),(10,TMPWK1)                               
         MVI   TMPWK1+8,C'-'                                                    
         GOTOR DATCON,DMCB,(0,QEND),(10,TMPWK1+9)                               
         GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#STEND),     +        
               ('LD_CHARQ',TMPWK1),(17,0)                                       
*                                                                               
         GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#PERTYP),    +        
               ('LD_CHARQ',H_PERTYP),(L'H_PERTYP,0)                             
*                                                                               
ESRH160  GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#PUBCOD),    +        
               ('LD_CHARQ',QPUB),(L'QPUB,0)                                     
*                                                                               
         GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#PUBNAM),    +        
               ('LD_CHARQ',PUBNAME),(L'PUBNAME,0)                               
*                                                                               
ESRH172  OC    BPUB+4(1),BPUB+4    PUB ZONE PRESENT?                            
         BZ    ESRH172D                                                         
         XC    TMPWK1,TMPWK1                                                    
         GOTOR APUBEDIT,DMCB,(X'08',BPUB),(C'S',TMPWK1)                         
         GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#PUBZON),    +        
               ('LD_CHARQ',TMPWK1+8+1),(2,0)                                    
         MVC   TMPWK2(L'PUBZNAME),PUBZNAME                                      
         OC    TMPWK2(L'PUBZNAME),SPACES                                        
         CLC   TMPWK2(L'PUBZNAME),SPACES                                        
         BE    ESRH172D                                                         
         GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#PZNAME),    +        
               ('LD_CHARQ',TMPWK2),(L'PUBZNAME,0)                               
*                                                                               
ESRH172D OC    BPUB+5(1),BPUB+5    PUB EDITION PRESENT?                         
         BZ    ESRH178                                                          
         OC    BPUB+4(1),BPUB+4    PUB ZONE PRESENT?                            
         BNZ   ESRH172H                                                         
         XC    TMPWK1,TMPWK1                                                    
         GOTOR APUBEDIT,DMCB,(X'08',BPUB),(C'S',TMPWK1)                         
         LA    RF,TMPWK1+8+1                                                    
         B     ESRH172M                                                         
*                                                                               
ESRH172H LA    RF,TMPWK1+8+1+2+1                                                
*                                                                               
ESRH172M GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#PUBEDT),    +        
               ('LD_CHARQ',0(RF)),(3,0)                                         
         GOTOR APUBEDIT,DMCB,(X'08',BPUB),(C'E',TMPWK1)                         
         GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#EDTEXP),    +        
               ('LD_CHARQ',TMPWK1),(11,0)                                       
*                                                                               
ESRH178  GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#IORDAT),    +        
               ('LD_BDATQ',BTODAY),(L'BTODAY,0)                                 
*                                                                               
         GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#CLTCOD),    +        
               ('LD_CHARQ',PCLTKCLT),(L'PCLTKCLT,0)                             
*                                                                               
         GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#CLTNAM),    +        
               ('LD_CHARQ',PCLTNAME),(L'PCLTNAME,0)                             
*                                                                               
         CLI   PCLTPROF+5,C'2'     SUB CLIENT?                                  
         BNE   ESRH186                                                          
*                                                                               
         GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#MCLTCD),    +        
               ('LD_CHARQ',PCLTPROF+6),(L'PCLTKCLT,0)                           
*                                                                               
         GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#MCLTNM),    +        
               ('LD_CHARQ',SVMCCNAM),(L'SVMCCNAM,0)                             
*                                                                               
ESRH186  GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#PRDCOD),    +        
               ('LD_CHARQ',PPRDKPRD),(L'PPRDKPRD,0)                             
*                                                                               
         GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#PRDNAM),    +        
               ('LD_CHARQ',PPRDNAME),(L'PPRDNAME,0)                             
*                                                                               
         CLI   SRESVTYP,SRT1ESTQ   ESR BY ESTIMATE?                             
         JE    ESRH187H                                                         
         CLI   SRESVTYP,SRTSTWEQ   ESR BY STEWARD ESTIMATE?                     
         JE    ESRH187H                                                         
         CLI   SRESVTYP,SRT1ESPQ   ESR BY ESTIMATE PERIOD?                      
         JE    ESRH187H                                                         
         CLI   SRESVTYP,SRT1SWPQ   STEWARDSHIP ESR BY ESTIMATE PERIOD?          
         JE    ESRH187H                                                         
         J     ESRH188                                                          
ESRH187H LA    R2,PESTREC                                                       
         USING PESTREC,R2                                                       
         GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#ESTNUM),    +        
               ('LD_UBINQ',PESTKEST),(L'PESTKEST,0)                             
         GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#ESTNAM),    +        
               ('LD_CHARQ',PESTNAME),(L'PESTNAME,0)                             
         CLC   PESTNAM2,SPACES                                                  
         BNH   ESRH188                                                          
         GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#ESTNM2),    +        
               ('LD_CHARQ',PESTNAM2),(L'PESTNAM2,0)                             
         DROP  R2                                                               
*                                                                               
ESRH188  GOTOR VT42105,DMCB,(RC),(RA),('SETUPRCQ',0)                            
*                                                                               
         MVC   TMPBYTE3,SHWINSCO                                                
         CLI   TMPBYTE3,0                                                       
         BNE   *+8                                                              
         MVI   TMPBYTE3,C'G'       SET TO GROSS                                 
         GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#SHWCOS),    +        
               ('LD_CHARQ',TMPBYTE3),(L'TMPBYTE3,0)                             
*                                                                               
         BRAS  RE,ESRH_PUB         REPLY PUBLICATION INFORMATION                
*                                                                               
         GOTOR VT42105,DMCB,(RC),(RA),('VCCLISTQ',0)                            
*                                                                               
         GOTOR VT42105,DMCB,(RC),(RA),('ACCLISTQ',0)                            
*                                                                               
         J     EXIT                                                             
*                                                                               
         LTORG                                                                  
         DROP  RB,R3,R4                                                         
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
SETESRHD NTR1  BASE=*,LABEL=*      SET ESR HEADER VALUES                        
*                                                                               
         USING ESRDSD,R4           ESR STORAGE BLOCK                            
*                                                                               
         MVI   H_SRLKEY,C'-'                                                    
         MVC   H_SRLKEY+1(H_SRLKYL-1),H_SRLKEY                                  
         MVC   H_LKSRTX,=C'SR'                                                  
         MVC   H_LKYMED,QMEDIA                                                  
         MVC   FULL(L'BTODAY),BTODAY                                            
         MVC   FULL(L'SVESR_YR),SVESR_YR                                        
         GOTOR DATCON,DMCB,(3,FULL),(10,WORK)                                   
         MVC   SVYEARYY,WORK+6                                                  
         MVC   H_LKYYER,WORK+6                                                  
         MVC   H_LKYCLT,QCLIENT                                                 
         CLI   H_LKYCLT+2,C' '     TWO CHARACTER CLIENT CODE?                   
         BNE   *+8                                                              
         MVI   H_LKYCLT+2,C'-'                                                  
         EDIT  (B3,SVESR_SQ),H_LKYRNO,0,FILL=0                                  
         CLI   SVESR_R#,0                                                       
         BE    SESRH12             NO REVISION #                                
         MVC   H_LKYRTX,=C'REV'                                                 
         EDIT  (B1,SVESR_R#),H_LKYRE#,0,FILL=0                                  
         B     SESRH14                                                          
*                                                                               
SESRH12  MVI   H_LKYRNO+L'H_LKYRNO,0                                            
         XC    H_LKYRTX,H_LKYRTX                                                
         XC    H_LKYRE#,H_LKYRE#                                                
*                                                                               
SESRH14  SR    RE,RE                                                            
         ICM   RE,7,SVESR_SQ                                                    
         CHI   RE,9999                                                          
         BH    SESRH16                                                          
         MVC   H_SRLKEY+DISPER#Q(LNAERF#Q),H_SRLKEY+DISP_R#Q                    
         MVI   H_SRLKEY+(H_SRLKYL-L'H_LKYR#E),C' '                              
*                                                                               
SESRH16  MVI   H_PERTYP,C'M'       MONTHLY (NOT USED YET)                       
*                                                                               
         J     EXIT                                                             
*                                                                               
         LTORG                                                                  
         DROP  RB,R4                                                            
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
ESRH_PUB NTR1  BASE=*,LABEL=*      PUBLICATION INFORMATION                      
*                                                                               
         L     R3,AWRKREC                                                       
         USING LIOBD,R3            LINKIO INTERFACE BLOCK                       
*                                                                               
         MVC   SVPSCOM1,SPACES                                                  
         MVC   SVPSCOM2,SPACES                                                  
         MVI   TMPBYTE1,0          KEEP TRACK REPLIED ADDRESSES                 
         LA    RE,H_PUBINF         CLEAR REPLY INFORMATION BLOCK                
         LHI   RF,H_PUBINL                                                      
         XCEFL                                                                  
*                                                                               
         MVI   ADRTYP,C'C'         CONTRACT ADDRESS                             
         MVC   CLTAGY,QAGENCY      AGY                                          
         MVC   CLTMED,QMEDIA       MED                                          
         MVC   CLTCODE,PCLTKCLT    CLIENT CODE                                  
         MVC   CLTOFF,PCLTOFF      CLIENT OFFICE                                
*                                                                               
         GOTOR APPGETAD,DMCB,(ADRTYP,CLTDATA),PUBREC,VDATAMGR                   
         CLI   0(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R1),0             NO ADDRESS FOUND?                            
         BE    ESRH_P30                                                         
         CLI   0(R1),X'0A'         CONTRACT ADDRESS FOUND?                      
         BNE   ESRH_P30                                                         
*                                                                               
         L     RE,4(R1)            POINT TO ADDRESS                             
         USING PGETADRD,RE                                                      
         MVC   H_PUBTYP,ADRTYP     ADDRESS TYPE                                 
         MVC   H_PUBNAM(L'PGADNAME),PGADNAME                                    
         MVC   H_PUBAL1(L'PGADNAME),PGADLIN1                                    
         MVC   H_PUBAL2(L'PGADLIN1),PGADLIN2                                    
         MVC   H_PUBAL3(L'PGADLIN2),PGADLIN3                                    
         MVC   H_PUBATT(L'PGADATTN),PGADATTN                                    
         MVC   H_PUBPH#(L'PGADTEL),PGADTEL                                      
         MVC   H_PUBFX#(L'PGADFAX),PGADFAX                                      
         MVC   H_PUBEML(L'PGADEADD),PGADEADD                                    
         DROP  RE                                                               
*                                                                               
         BRAS  RE,WPUB_SET                                                      
         MVI   TMPBYTE1,C'C'       INDICATE CONTRACT ADDRESS REPLIED            
         CLI   SECNADDR,C'Y'       NEED TO CK FOR 2ND ADDRESS?                  
         BNE   ESRH_P90                                                         
         LA    RE,H_PUBINF         CLEAR REPLY INFORMATION BLOCK                
         LHI   RF,H_PUBINL                                                      
         XCEFL                                                                  
*                                                                               
ESRH_P30 LA    R2,PUBREC+33                                                     
         MVI   ELCODE,X'14'        REP ELEM CODE                                
ESRH_P34 BRAS  RE,NXTEL                                                         
         BNE   ESRH_P60                                                         
*                                                                               
         USING PUBREPEL,R2                                                      
         CLC   PUBRPOFF,=3X'FF'                                                 
         BE    ESRH_P40                                                         
         CLC   PUBRPOFF,PCLTKCLT                                                
         BE    ESRH_P40                                                         
         CLI   PUBRPOFF,X'FF'                                                   
         BNE   ESRH_P34                                                         
         CLC   PUBRPOFF+1(1),PCLTOFF                                            
         BNE   ESRH_P34                                                         
*                                                                               
ESRH_P40 CLI   PUBREPEL+1,X'2D'    BIG REP ELEM?                                
         BL    ESRH_P50                                                         
         TM    PUBCSCC1,X'40'      STD COMMENT ON CONTRACTS?                    
         BZ    *+10                                                             
         MVC   SVPSCOM1,PUBCSC1                                                 
         TM    PUBCSCC2,X'40'      STD COMMENT ON CONTRACTS?                    
         BZ    ESRH_P50                                                         
         MVC   SVPSCOM2,PUBCSC2                                                 
*                                                                               
ESRH_P50 OC    PUBCNREP,PUBCNREP   CONTRACT REP PRESENT?                        
         BZ    ESRH_P34                                                         
         MVC   SVTMPKEY,KEY                                                     
         XC    KEY,KEY                                                          
         MVC   KEY+00(02),QAGENCY                                               
         MVC   KEY+02(01),QMEDIA                                                
         MVI   KEY+03,X'11'        REP RECORD CODE                              
         MVC   KEY+04(04),PUBCNREP                                              
         GOTOR HIGH                                                             
         CLC   KEY(L'PREPKEY),KEYSAVE                                           
         BE    *+14                                                             
         MVC   KEY,SVTMPKEY        RESTORE KEY                                  
         B     ESRH_P60                                                         
         MVC   TMPFULL1,AREC       SAVE ORIGINAL AIO POINTER                    
         LA    RE,TMPWKAIO                                                      
         ST    RE,AREC                                                          
         GOTOR GETPRT                                                           
         MVC   AREC,TMPFULL1       RESTORE ORIGINAL AIO POINTER                 
         MVC   KEY,SVTMPKEY        RESTORE KEY                                  
*                                                                               
         LA    RE,TMPWKAIO+33                                                   
         USING PREPELEM,RE                                                      
         CLI   PREPELEM,X'11'                                                   
         BE    *+6                                                              
         DC    H'0'                INVALID REP RECORD                           
         MVI   H_PUBTYP,C'R'       ADDRESS TYPE IS REP                          
         MVC   H_PUBNAM(L'PREPNAME),PREPNAME                                    
         MVC   H_PUBAL1(L'PREPLIN1),PREPLIN1                                    
         MVC   H_PUBAL2(L'PREPLIN2),PREPLIN2                                    
         MVC   H_PUBATT(L'PREPATTN),PREPATTN                                    
         MVC   H_PUBPH#(L'PREPTEL),PREPTEL                                      
         MVC   H_PUBSTA(L'PREPSTAC),PREPSTAC                                    
         MVC   H_PUBAL3(L'PREPLIN3),PREPLIN3                                    
         MVC   H_PUBFX#(L'PREPFAX),PREPFAX                                      
         DROP  RE                                                               
*                                                                               
         BRAS  RE,WPUB_SET                                                      
         CLI   TMPBYTE1,C'C'       ALREADY REPLIED CONTRACT ADDRESS?            
         BE    ESRH_P90                                                         
         CLI   SECNADDR,C'Y'       NEED TO CK FOR 2ND ADDRESS?                  
         BNE   ESRH_P90                                                         
         LA    RE,H_PUBINF         CLEAR REPLY INFORMATION BLOCK                
         LHI   RF,H_PUBINL                                                      
         XCEFL                                                                  
*                                                                               
ESRH_P60 LA    R2,PUBREC+33                                                     
         CLI   0(R2),X'10'         MAIN NAME ELEM?                              
         BE    *+6                                                              
         DC    H'0'                                                             
         USING PUBNAMEL,R2                                                      
         MVI   H_PUBTYP,C'M'       ADDRESS TYPE IS MAIN                         
         MVC   H_PUBNAM(L'PUBNAME),PUBNAME                                      
         MVC   H_PUBAL1(L'PUBLINE1),PUBLINE1                                    
         MVC   H_PUBAL2(L'PUBLINE2),PUBLINE2                                    
         MVC   H_PUBAL3(L'PUBLINE3),PUBLINE3                                    
         MVC   H_PUBCIT(L'PUBCITY),PUBCITY                                      
         MVC   H_PUBSTA(L'PUBSTATE),PUBSTATE                                    
         MVC   H_PUBZIP(L'PUBNWZIP),PUBNWZIP                                    
*                                                                               
         MVI   ELCODE,X'11'        PUBLICATION SUPPLEMENT ELEM                  
         BRAS  RE,NXTEL                                                         
         BNE   ESRH_P66                                                         
         USING PUBSADEL,R2                                                      
         MVC   H_PUBPH#,PUBTEL                                                  
         MVC   H_PUBATT,PUBATTN                                                 
         MVC   H_PUBFX#,PUBSFAXN                                                
*                                                                               
ESRH_P66 MVI   ELCODE,X'70'        PUBLICATION WEB ADDRESS ELEM                 
         BRAS  RE,NXTEL                                                         
         BNE   ESRH_P72                                                         
         USING PUBWEBEL,R2                                                      
         SR    RE,RE                                                            
         IC    RE,1(R2)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   H_PUBWEB(0),PUBWEBS                                              
*                                                                               
ESRH_P72 BRAS  RE,WPUB_SET                                                      
*                                                                               
ESRH_P90 J     EXIT                                                             
*                                                                               
WPUB_SET ST    RE,TMPFULL1                                                      
*                                                                               
         GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTMAP',E#PUB)                 
*                                                                               
         CLI   H_PUBTYP,0                                                       
         BE    WPUB_S52                                                         
         GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#PBATYP),    +        
               ('LD_CHARQ',H_PUBTYP),(L'H_PUBTYP,0)                             
*                                                                               
WPUB_S52 OC    H_PUBNAM,H_PUBNAM                                                
         BZ    WPUB_S54                                                         
         GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#PUBNAM),    +        
               ('LD_CHARQ',H_PUBNAM),(L'H_PUBNAM,0)                             
*                                                                               
WPUB_S54 GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#ADDRL1),    +        
               ('LD_CHARQ',H_PUBAL1),(L'H_PUBAL1,0)                             
*                                                                               
         GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#ADDRL2),    +        
               ('LD_CHARQ',H_PUBAL2),(L'H_PUBAL2,0)                             
*                                                                               
         OC    H_PUBAL3,H_PUBAL3                                                
         BZ    WPUB_S64                                                         
         GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#ADDRL3),    +        
               ('LD_CHARQ',H_PUBAL3),(L'H_PUBAL3,0)                             
*                                                                               
WPUB_S64 OC    H_PUBCIT,H_PUBCIT                                                
         BZ    WPUB_S68                                                         
         GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#CITY),      +        
               ('LD_CHARQ',H_PUBCIT),(L'H_PUBCIT,0)                             
*                                                                               
WPUB_S68 OC    H_PUBSTA,H_PUBSTA                                                
         BZ    WPUB_S72                                                         
         GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#STATE),     +        
               ('LD_CHARQ',H_PUBSTA),(L'H_PUBSTA,0)                             
*                                                                               
WPUB_S72 OC    H_PUBZIP,H_PUBZIP                                                
         BZ    WPUB_S76                                                         
         GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#ZIPCOD),    +        
               ('LD_CHARQ',H_PUBZIP),(L'H_PUBZIP,0)                             
*                                                                               
WPUB_S76 CLI   TELFXOPT,C'B'       SHOW BOTH TELEPHONE AND FAX #S?              
         BE    *+12                                                             
         CLI   TELFXOPT,C'T'       SHOW TELEPHONE # ONLY?                       
         BNE   WPUB_S80                                                         
         OC    H_PUBPH#,H_PUBPH#                                                
         BZ    WPUB_S80                                                         
         GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#TELNUM),    +        
               ('LD_CHARQ',H_PUBPH#),(L'H_PUBPH#,0)                             
*                                                                               
WPUB_S80 OC    H_PUBATT,H_PUBATT                                                
         BZ    WPUB_S84                                                         
         GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#ATTENT),    +        
               ('LD_CHARQ',H_PUBATT),(L'H_PUBATT,0)                             
*                                                                               
WPUB_S84 CLI   TELFXOPT,C'B'       SHOW BOTH TELEPHONE AND FAX #S?              
         BE    *+12                                                             
         CLI   TELFXOPT,C'F'       SHOW FAX # ONLY?                             
         BNE   WPUB_S86                                                         
         OC    H_PUBFX#,H_PUBFX#                                                
         BZ    WPUB_S86                                                         
         MVC   TMPWK2(L'SPACES),SPACES                                          
         MVC   TMPWK2(L'H_PUBFX#),H_PUBFX#                                      
         CLC   H_PUBFX#(3),=C'FX=' NEED TO READ CONTROL FILE?                   
         BNE   *+20                                                             
         MVC   TMPWK1(L'H_PUBFX#),H_PUBFX#                                      
         BRAS  RE,TFAXCODE                                                      
         MVC   TMPWK2,TMPWK1                                                    
         GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#FAXNUM),    +        
               ('LD_CHARQ',TMPWK2),(L'CTFX1NUM,0)                               
*                                                                               
WPUB_S86 OC    H_PUBWEB,H_PUBWEB                                                
         BZ    WPUB_S88                                                         
         GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#WEDADR),    +        
               ('LD_CHARQ',H_PUBWEB),(L'H_PUBWEB,0)                             
*                                                                               
WPUB_S88 OC    H_PUBEML,H_PUBEML                                                
         BZ    WPUB_S90                                                         
         GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#E_MAIL),    +        
               ('LD_CHARQ',H_PUBEML),(L'H_PUBEML,0)                             
*                                                                               
WPUB_S90 OC    H_PUBCM1,H_PUBCM1                                                
         BZ    WPUB_S92                                                         
         GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#COMMNT),    +        
               ('LD_CHARQ',H_PUBCM1),(L'H_PUBCM1,0)                             
*                                                                               
WPUB_S92 DS    0H                                                               
*                                                                               
WPUB_SX  L     RE,TMPFULL1                                                      
         BR    RE                                                               
*                                                                               
         LTORG                                                                  
         DROP  RB,R2,R3                                                         
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
ESRHCOMM NTR1  BASE=*,LABEL=*      REPLY ESR HEADER COMMENT                     
*                                                                               
         TM    ADBSW,AS_ESRUQ      WEB IO?                                      
         JZ    EXIT                                                             
         TM    ADRPYREC,RPYHEADQ   HEADER ALREADY REPLIED?                      
         JNZ   *+6                                                              
         DC    H'0'                REPLY RECORD OUT OF SYNC                     
*                                                                               
         L     R4,AESRDSST         POINT TO ESR STORAGE BLOCK                   
         USING ESRDSD,R4           ESR STORAGE BLOCK                            
*                                                                               
         L     R3,AWRKREC                                                       
         USING LIOBD,R3            LINKIO INTERFACE BLOCK                       
*                                                                               
         LA    R2,ESR#RTAB                                                      
         USING ESR#RTBD,R2                                                      
         OC    0(E#R_LENQ,R2),0(R2)                                             
         BZ    ESRHC_X                                                          
*                                                                               
         GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTMAP',E#ESRHTC)              
         OI    ADRPYSW1,RPYHCONQ                                                
*                                                                               
         MVC   TMPWK1(L'SPACES),SPACES                                          
         MVC   TMPWK1(L'SRESTTX2),SRESTTX2                                      
*                                                                               
         LHI   R5,E#R_MAXQ                                                      
ESRHC24  OC    0(E#R_LENQ,R2),0(R2)                                             
         BZ    ESRHC30                                                          
         MVC   TMPWK2(L'SPACES),SPACES                                          
         MVC   TMPWK2(1),QMEDIA                                                 
         MVI   TMPWK2+1,C'-'                                                    
         MVC   FULL(L'BTODAY),BTODAY                                            
         MVC   FULL(L'E#R_YEAR),E#R_YEAR                                        
         GOTOR DATCON,DMCB,(3,FULL),(10,WORK)                                   
         MVC   TMPWK2+2(2),WORK+6                                               
         MVC   TMPWK2+2+2(3),QCLIENT                                            
         CLI   TMPWK2+2+2+2,C' '     TWO CHARACTER CLIENT CODE?                 
         BNE   *+8                                                              
         MVI   TMPWK2+2+2+2,C'-'                                                
         EDITR (B3,E#R_NUMB),(4,TMPWK2+2+2+3),0,FILL=0                          
         CLI   E#R_REV#,0                                                       
         BE    ESRHC28                                                          
         MVI   TMPWK2+2+2+3+4,C'-'                                              
         MVC   TMPWK2+2+2+3+4+1(3),=C'REV'                                      
         EDITR (B1,E#R_REV#),(3,TMPWK2+2+2+3+4+1+3),0,FILL=0                    
*                                                                               
ESRHC28  CHI   R5,E#R_MAXQ                                                      
         BE    ESRHC28H                                                         
         MVC   TMPWK1(L'SPACES),SPACES                                          
         MVC   TMPWK1+(H_SRLKYL),TMPWK2                                         
         B     ESRHC28K                                                         
ESRHC28H MVC   TMPWK1+L'SRESTTX2+1(H_SRLKYL),TMPWK2                             
ESRHC28K LA    RF,TMPWK1+L'SPACES-1                                             
         BRAS  RE,LAST_CHR                                                      
         LA    RF,2(RF)                                                         
         MVC   0(L'SRESTTX3,RF),SRESTTX3                                        
         LA    RF,(L'SRESTTX3+1)(RF)                                            
         GOTOR DATCON,DMCB,(3,E#R_DATE),(21,0(RF))                              
         BRAS  RE,RPYHTRCM                                                      
         LA    R2,E#R_LENQ(R2)                                                  
         BCT   R5,ESRHC24                                                       
*                                                                               
ESRHC30  MVC   TMPWK1(L'SPACES),SPACES                                          
         MVC   TMPWK1(L'SRESTTX4),SRESTTX4                                      
         LA    RF,TMPWK1+L'SRESTTX4+1                                           
         MVC   0(1,RF),PBUYREC+(PBUYKMED-PBUYKEY)                               
         MVI   1(RF),C'/'                                                       
         MVC   2(3,RF),PBUYREC+(PBUYKCLT-PBUYKEY)                               
         MVI   5(RF),C'/'                                                       
         MVC   6(3,RF),PBUYREC+(PBUYKPRD-PBUYKEY)                               
         CLI   SRESVTYP,SRT1ESTQ   ESR BY ESTIMATE?                             
         JE    ESRHC32                                                          
         CLI   SRESVTYP,SRTSTWEQ   ESR BY ESTIMATE?                             
         JE    ESRHC32                                                          
         CLI   SRESVTYP,SRT1ESPQ   ESR BY ESTIMATE PERIOD?                      
         JE    ESRHC32                                                          
         CLI   SRESVTYP,SRT1SWPQ   STEWARDSHIP ESR BY ESTIMATE PERIOD?          
         JE    ESRHC32                                                          
         MVC   TMPHALF1,E#R_ESTC   ESTIMATE NUMBER                              
         MVI   TMPWK1+L'SRESTTX4+10,C'/'                                        
         EDIT  (B2,TMPHALF1),(3,TMPWK1+L'SRESTTX4+11),                 +        
               0,ALIGN=RIGHT,ZERO=NOBLANK,FILL=0                                
ESRHC32  LA    RF,TMPWK1+L'SPACES-1                                             
         BRAS  RE,LAST_CHR                                                      
         MVI   1(RF),C'.'                                                       
         BRAS  RE,RPYHTRCM                                                      
*                                                                               
         MVC   TMPWK1(L'SPACES),SPACES                                          
         MVC   TMPWK1(L'SRESTTX5),SRESTTX5                                      
         BRAS  RE,RPYHTRCM                                                      
*                                                                               
ESRHC_X  XC    DMCB,DMCB                                                        
         J     EXIT                                                             
*                                                                               
RPYHTRCM LR    R0,RE               REPLY TRANSITION COMMENT                     
         GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#COMMNT),    +        
               ('LD_CHARQ',TMPWK1),(L'ESRCOMLN,0)                               
         LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
LAST_CHR CLI   0(RF),0                                                          
         JE    *+8                                                              
         CLI   0(RF),C' '                                                       
         JNE   *+10                                                             
         BCTR  RF,0                                                             
         J     *-18                                                             
         BR    RE                                                               
*                                                                               
SRESTTX2 DC    C'PLEASE BE ADVISED THAT'                                        
SRESTTX3 DC    C'SENT ON'                                                       
SRESTTX4 DC    C'MAY ALSO CONTAIN INSERTIONS FOR'                               
SRESTTX5 DC    C'CONTACT THE SENDER IF YOU HAVE ANY QUESTIONS ABOUT THO+        
               SE ORDERS.'                                                      
*                                                                               
         LTORG                                                                  
         DROP  RB,R3,R4,R2                                                      
*                                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
PPSR210D DSECT                                                                  
*                                                                               
RELO10   DS    F                                                                
*                                                                               
ADRTYP   DS    CL1                 TYPE OF ADDR REC-PAY,TRAFFIC,ETC.            
CLTDATA  DS    0CL7                PASS KEY INFO TO PPGETADR MODULE             
CLTAGY   DS    CL2                                                              
CLTMED   DS    CL1                                                              
CLTCODE  DS    CL3                                                              
CLTOFF   DS    CL1                                                              
*                                                                               
SVYEARYY DS    CL2                 SAVE YEAR (YY) FROM ESR LONG KEY             
SVTMPKEY DS    XL(L'KEY)                                                        
WKINSCNT DS    H                                                                
*                                                                               
H_PUBINF DS    0X                  PUBLICATION INFO                             
H_PUBTYP DS    C                   ADDRESS TYPE                                 
H_PUBNAM DS    CL(L'PREPNAME)      NAME                                         
H_PUBAL1 DS    CL(L'PUBLINE1)      ADDRESS LINE 1                               
H_PUBAL2 DS    CL(L'PUBLINE2)      ADDRESS LINE 2                               
H_PUBAL3 DS    CL(L'PUBLINE3)      ADDRESS LINE 3                               
H_PUBCIT DS    CL(L'PUBCITY)       CITY                                         
H_PUBSTA DS    CL(L'PUBSTATE)      STATE                                        
H_PUBZIP DS    CL(L'PUBNWZIP)      ZIP CODE                                     
H_PUBATT DS    CL(L'PUBATTN)       PHONE NUMBER                                 
H_PUBPH# DS    CL(L'PUBTEL)        ATTENTION                                    
H_PUBFX# DS    CL(L'PUBSFAXN)      FAX NUMBER                                   
H_PUBWEB DS    CL(L'PUBWEBS+255-6) WEB ADDRESS                                  
H_PUBEML DS    CL(L'PGADEADD)      E-MAIL                                       
H_PUBCM1 DS    CL(L'P)             COMMENT                                      
H_PUBINL EQU   *-H_PUBINF                                                       
*                                                                               
TMPFULL1 DS    F                                                                
TMPFULL2 DS    F                                                                
TMPHALF1 DS    H                                                                
TMPHALF2 DS    H                                                                
TMPBYTE1 DS    X                                                                
TMPBYTE2 DS    X                                                                
TMPBYTE3 DS    X                                                                
*                                                                               
TMPSVKEY DS    XL(L'KEY)           TEMP SAVE KEY STORAGE                        
TMPSVAIO DS    XL(L'AREC)                                                       
*                                                                               
TMPWK1   DS    XL256                                                            
TMPWK2   DS    XL256                                                            
*                                                                               
TMPELEM  DS    XL256                                                            
*                                                                               
WKESRFLG DS    X                   ESR WORK FLAG                                
*        EQU   X'80'                                                            
*        EQU   X'40'                                                            
E_SRBMFQ EQU   X'20'               CONTAIN BUY MOVE "FROM" INSERTION            
E_SRBMTQ EQU   X'10'               CONTAIN BUY MOVE "TO"   INSERTION            
*                                                                               
TMPWKAIO DS    XL4096              TEMP WORKING AIO                             
*                                                                               
PPSR210X EQU   *                                                                
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         PRINT OFF                                                              
*                                                                               
       ++INCLUDE PPSR2WRK1                                                      
         EJECT                                                                  
*                                                                               
       ++INCLUDE PPSR2WRK2                                                      
         EJECT                                                                  
*                                                                               
         PRINT ON                                                               
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'007PPSR210   03/25/11'                                      
         END                                                                    
