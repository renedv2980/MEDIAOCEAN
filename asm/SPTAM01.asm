*          DATA SET SPTAM01    AT LEVEL 009 AS OF 11/27/06                      
*PHASE T22901A                                                                  
*INCLUDE DPTRD                                                                  
*                                                                               
*===============================================================*               
*                                                               *               
* HISTORY                                                       *               
* -------                                                       *               
*                                                               *               
* WHEN    LEV WHAT                                              *               
* ----    --- ----                                              *               
* 08NOV06 009 SEND WEEKLY/DAILY FLAG                            *               
* 21JUN05 007 DON'T DIE ON CLT REC NOT FOUND, TREAT AS INVALID  *               
* 03APR03 006 GIVE ERROR FOR EST NOT FOUND ON CAMPAIGN          *               
* 20JUN02 005 CLIENT STRING SECURITY                            *               
*         --- BROWSE CAMPAIGNS BY NEW CMP2KEY FOR SECURITY      *               
* 03MAY02 004 SOME REGISTER OTHER THAN RE!                      *               
* 05SEP01 003 SEND 6 CHAR DEMO NAMES                            *               
* 22AUG01 002 DON'T TRANSMIT NULLS!                             *               
* 05JUN01 001 INITIAL DEVELOPMENT                               *               
*                                                               *               
*===============================================================*               
*                                                                               
T22901   TITLE 'SPTAM01 - TV AVAIL MANAGER - UPLOAD DATA TO PC'                 
T22901   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,SPTM01**                                                       
*                                                                               
         LR    RC,R1                                                            
         USING WORKD,RC                                                         
*                                                                               
         L     RA,ATWA                                                          
         USING TWAD,RA                                                          
*                                                                               
         MVC   V10000,=X'01000000' VERSION 1.0.0                                
*                                                                               
         OC    SVRCVEL,SVRCVEL     THIS WILL BE 0 ON RE-ENTRY                   
         BNZ   *+10                                                             
         MVC   SVRCVEL,SVOLDRCV                                                 
         MVC   SVOLDRCV,SVRCVEL    SAVE ORIGINAL RCVEL                          
*                                                                               
         CLI   SVRESUME,0          TEST RETURN FROM GLOBBER CALL                
         BE    TAM20                NO                                          
         BRAS  RE,RETURN                                                        
         B     EXIT                                                             
*                                                                               
TAM20    CLI   SVRCVEL+1,H02Q      CAMPAIGN BROWSE                              
         BNE   *+12                                                             
         BRAS  RE,BROWSE                                                        
         B     EXIT                                                             
*                                                                               
         CLI   SVRCVEL+1,H04Q      CAMPAIGN RECORD                              
         BNE   *+12                                                             
         BRAS  RE,CAMPAIGN                                                      
         B     EXIT                                                             
*                                                                               
         CLI   SVRCVEL+1,H08Q      WORK ADD                                     
         BNE   *+12                                                             
         BRAS  RE,WORK_ADD                                                      
         B     EXIT                                                             
*                                                                               
EQXIT    CR    RE,RE               EXIT CC EQ                                   
         J     EXIT                                                             
*                                                                               
NEQXIT   LTR   RE,RE               EXIT CC NEQ                                  
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*=================================================================*             
* CAMPAIGN BROWSE                                                 *             
*=================================================================*             
         SPACE 1                                                                
BROWSE   NTR1  BASE=*,LABEL=*                                                   
         BRAS  RE,BLDSECRT                                                      
         XC    KEY,KEY                                                          
         LA    R2,KEY                                                           
         USING CAMRECD,R2                                                       
         MVI   CMP2KTYP,CMP2KTYQ                                                
         MVI   CMP2KSUB,CMP2KSBQ                                                
         MVC   CMP2KAM,BAGYMD                                                   
         OC    CMP2KAM,BBYRMASK                                                 
         MVC   CMP2KBYR,BBUYER                                                  
*                                                                               
         L     R2,AIO1                                                          
         ST    R2,AIO                                                           
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         B     BR20                                                             
*                                                                               
BR10     GOTO1 SEQ                                                              
*                                                                               
BR20     CLC   KEY(CMP2KCLT-CAMRECD),KEYSAVE                                    
         BNE   BRX                                                              
*                                                                               
         GOTO1 GETREC                                                           
*                                                                               
         OC    CMPEDDT,CMPEDDT     TEST END DATE SET                            
         BZ    *+14                 NO - DON'T TEST IT                          
         CLC   CAMEND,CMPEDDT      TEST CAMPAIGN ENDS BEFORE FILT DATE          
         BL    BR10                 YES, SKIP IT                                
         BRAS  RE,AUTHCLT          AUTHORIZED TO THIS CLT?                      
         BNE   BR10                 NO, SKIP IT                                 
         BRAS  RE,SENDCAMP         ELSE SEND IT'S DETAILS                       
         B     BR10                                                             
*                                                                               
BRX      J     EXIT                                                             
         LTORG                                                                  
         DROP  R2                                                               
         EJECT                                                                  
*=================================================================*             
* CAMPAIGN RECORD                                                 *             
*=================================================================*             
         SPACE 1                                                                
CAMPAIGN NTR1  BASE=*,LABEL=*                                                   
         BRAS  RE,BLDSECRT                                                      
         XC    KEY,KEY                                                          
         LA    R2,KEY                                                           
         USING CAMRECD,R2                                                       
         MVI   CAMKTYP,CAMKTYPQ                                                 
         MVI   CAMKSUB,CAMKSUBQ                                                 
         MVC   CAMKAGMD,BAGYMD                                                  
         OC    CAMKAGMD,BBYRMASK                                                
         MVC   CAMKBYR,BBUYER                                                   
         MVC   CAMKCAM,BCAMP                                                    
*                                                                               
         L     R2,AIO1                                                          
         ST    R2,AIO                                                           
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(CAMKREST-CAMRECD),KEYSAVE                                    
         BE    CAMP2                                                            
         MVC   ERROR,=Y(BADCAMP)                                                
         GOTO1 SENDMSG                                                          
*                                                                               
CAMP2    GOTO1 GETREC                                                           
         BRAS  RE,AUTHCLT                                                       
         BE    CAMP3                                                            
         MVC   ERROR,=Y(SECLOCK)                                                
         GOTO1 SENDMSG                                                          
*                                                                               
CAMP3    BRAS  RE,SENDCAMP                                                      
*                                                                               
* SEND ESTIMATE DATA                                                            
         XC    KEY,KEY             GET THE ESTIMATE                             
         MVC   KEY+1(1),BAGYMD                                                  
         MVC   KEY+2(2),CAMCLT                                                  
         MVC   KEY+4(3),CAMPRDC                                                 
         MVC   KEY+7(1),CAMEST                                                  
         DROP  R2                                                               
*                                                                               
         USING ESTHDRD,R2                                                       
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    CAMP4                                                            
         MVC   ERROR,=Y(BADEST)                                                 
         GOTO1 SENDMSG                                                          
*                                                                               
CAMP4    GOTO1 GETREC                                                           
*                                                                               
         LHI   R1,H06Q                                                          
         BRAS  RE,SENDH                                                         
*                                                                               
* SEND DEMO NAMES                                                               
         BRAS  RE,GDEMCAT          GET DEMO CATEGORIES                          
         BNE   CAMP20                                                           
         LA    R4,BLOCK                                                         
*                                                                               
CAMP10   LA    R1,H06_01Q                                                       
         BRAS  RE,SENDD                                                         
         AHI   R4,6                                                             
         BCT   R7,CAMP10                                                        
*                                                                               
CAMP20   LA    R1,H06_02Q                                                       
         LA    R4,EDAYMENU                                                      
         BRAS  RE,SENDD                                                         
*                                                                               
CAMP30   GOTO1 VDATCON,DMCB,ESTART,(2,FULL)   EST START & END DATES             
         GOTO1 (RF),(R1),EEND,(2,FULL+2)                                        
         LA    R1,H06_04Q                                                       
         LA    R4,FULL                                                          
         BRAS  RE,SENDD                                                         
         AHI   R4,2                                                             
         LA    R1,H06_05Q                                                       
         BRAS  RE,SENDD                                                         
*                                                                               
         CLI   EOWSDAY,0           OUT OF WEEK START DAY                        
         BE    CAMP40                                                           
         LA    R4,EOWSDAY          OUT OF WEEK START DAY NUMBER                 
         LA    R1,H06_06Q          MAP CODE                                     
         BRAS  RE,SENDD                                                         
*                                                                               
CAMP40   XC    DMCB(24),DMCB       READ DAYPART RECORD                          
         MVC   DMCB(2),QAGY                                                     
         MVC   DMCB+2(1),QMED                                                   
         MVC   DMCB+3(1),EDAYMENU                                               
         GOTO1 =V(DPTRD),DMCB,,AIO3,VDATAMGR,RR=Y                               
         CLI   DMCB+8,X'FF'                                                     
         BE    CAMPX                                                            
         LA    R4,DUB                                                           
         L     R6,AIO3                                                          
         LHI   R3,36               MAX 36 DAYPARTS                              
*                                                                               
CAMP50   OC    0(5,R6),0(R6)                                                    
         BZ    CAMPX                                                            
         MVC   DUB(1),0(R6)                                                     
         GOTO1 VHEXOUT,DMCB,1(R6),DUB+1,1,=C'TOG'                               
         MVC   DUB+3(3),2(R6)                                                   
         LA    R1,H06_03Q                                                       
         BRAS  RE,SENDD                                                         
         AHI   R6,5                                                             
         BCT   R3,CAMP50                                                        
*                                                                               
CAMPX    J     EXIT                                                             
         LTORG                                                                  
         DROP  R2                                                               
         EJECT                                                                  
*=================================================================*             
* CALL SECRET FOR INIT                                            *             
*=================================================================*             
         SPACE 1                                                                
BLDSECRT NTR1  BASE=*,LABEL=*                                                   
         OC    TWASAGN,TWASAGN     ON NEW SECURITY                              
         BNZ   *+14                                                             
         OC    TWAACCS(2),TWAACCS  OR HAVE LIMIT ACCESS                         
         BZ    BLDSECX                                                          
         L     R0,AIO4                                                          
         LHI   R1,SECLENQ                                                       
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         L     RF,ACOMFACS         INITIALIZE SECURITY BLOCK                    
         L     RF,CSECRET-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,('SECPINIT',AIO4),0                                    
         BE    BLDSECX                                                          
         DCHO                                                                   
*                                                                               
BLDSECX  J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
*==============================================================*                
* MAKE SURE THIS USER ID IS AUTH TO THIS CLT                   *                
*==============================================================*                
         SPACE 1                                                                
         USING CAMRECD,R2                                                       
AUTHCLT  NTR1  BASE=*,LABEL=*                                                   
         GOTO1 VCLUNPK,DMCB,CAMCLT,QCLT                                         
         MVC   MYKEYSAV,KEY                                                     
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING CLTHDR,R6                                                        
         MVC   CKEYAM,BAGYMD                                                    
         MVC   CKEYCLT,CAMCLT                                                   
*                                                                               
         L     R6,AIO2                                                          
         ST    R6,AIO                                                           
         CLC   KEY(13),0(R6)       HAVE THIS REC ALREADY?                       
         BE    AC10                 YES                                         
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE     MAKE SURE FOUND IT!                          
         BNE   ACNO                JUST TREAT AS INVALID                        
         GOTO1 GETREC                                                           
*                                                                               
AC10     MVC   SVCPROF,CPROF                                                    
         CLI   SVCPROF+6,C'Y'                                                   
         BNE   AC15                                                             
         GOTO1 VCLUNPK,DMCB,(SVCPROF+6,CAMCLT),QCLT                             
*                                                                               
AC15     CLI   TWAACCS,C'+'        IF THEY USE MKT LIMIT ACCESS ONLY            
         BE    ACYES                DON'T BOTHER WITH OFFICER CALL              
**         CLI   TWAACCS+2,C'+'                                                 
**         BE    ACYES                                                          
*                                                                               
         XC    WORK,WORK                                                        
         LA    R1,WORK                                                          
         USING OFFICED,R1                                                       
         MVI   OFCSYS,C'S'                                                      
         MVC   OFCAUTH,TWAACCS     2 BYTES!                                     
         MVC   OFCLMT,TWAACCS      4 BYTES!                                     
         MVC   OFCAGY,QAGY                                                      
         MVC   OFCOFC,COFFICE                                                   
         MVC   OFCCLT,QCLT                                                      
         OI    OFCINDS,OFCI2CSC    PASSING 2 BYTE CLT CODE                      
         MVC   OFCCLT2,CAMCLT                                                   
         MVC   OFCSAGMD,BAGYMD                                                  
         MVC   OFCACCSC(3),CACCESS                                              
         MVI   OFCACCSM,X'FF'                                                   
         MVC   OFCSECD,AIO4        SECRET BLOCK                                 
         DROP  R1                                                               
*                                                                               
         GOTO1 VOFFICER,DMCB,(C'N',WORK),ACOMFACS                               
         CLI   0(R1),0                                                          
         BE    ACYES                                                            
         B     ACNO                                                             
*                                                                               
ACYES    BAS   RE,RESTORE                                                       
         J     EQXIT                                                            
*                                                                               
ACNO     BAS   RE,RESTORE                                                       
         J     NEQXIT                                                           
*                                                                               
         DROP  R2,R6                                                            
*                                                                               
* RESTORE SEQUENTIAL READ                                                       
RESTORE  LR    R0,RE                                                            
         MVC   AIO,AIO1                                                         
         MVC   KEY(L'CAMKEY),MYKEYSAV                                           
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE     MAKE SURE FOUND IT!                          
         BE    *+6                                                              
         DCHO                                                                   
         LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
         EJECT                                                                  
*=================================================================*             
* SEND CAMPAIGN DATA (RECORD AND LIST DATA)                       *             
*   PASS R2=A(CAMPAIGN RECORD)                                    *             
*=================================================================*             
         SPACE 1                                                                
         USING CAMRECD,R2                                                       
SENDCAMP NTR1  BASE=*,LABEL=*                                                   
         LHI   R1,H02Q                                                          
         BRAS  RE,SENDH                                                         
*                                                                               
         LA    R4,QMED                                                          
         LA    R1,H02_01Q                                                       
         BRAS  RE,SENDD                                                         
*                                                                               
         LA    R4,BUYERCD                                                       
         LA    R1,H02_02Q                                                       
         BRAS  RE,SENDD                                                         
*                                                                               
         MVC   HALF,CAMKCAM        SEND CAMPAIGN NUMBER                         
         XC    HALF,=X'FFFF'                                                    
         LA    R1,H02_03Q                                                       
         LA    R4,HALF                                                          
         BRAS  RE,SENDD                                                         
*                                                                               
         LA    R1,H02_04Q                                                       
         LA    R4,QCLT                                                          
         BRAS  RE,SENDD                                                         
*                                                                               
         CLC   CAMNAME,SPACES      DON'T SEND IF THERE IS NONE                  
         BNH   SC10                                                             
         LA    R5,CAMNAME+(L'CAMNAME-1)   END OF CAMPAIGN NAME                  
*                                                                               
         CLI   0(R5),C' '          GET LENGTH OF DATA TO SEND                   
         BH    *+8                                                              
         BCT   R5,*-8                                                           
         LA    RF,CAMNAME-1                                                     
         SR    R5,RF                                                            
         LA    R1,H02_05Q          SEND CAMPAIGN NAME                           
         LA    R4,CAMNAME                                                       
         BRAS  RE,SENDD                                                         
*                                                                               
SC10     LA    R1,H02_06Q          SEND PRODUCT CODE                            
         LA    R4,CAMPRDC                                                       
         BRAS  RE,SENDD                                                         
*                                                                               
         LA    R1,H02_07Q          SEND ESTIMATE NUMBER                         
         LA    R4,CAMEST                                                        
         BRAS  RE,SENDD                                                         
*                                                                               
         LA    R1,H02_08Q          SEND START DATE                              
         LA    R4,CAMSTRT                                                       
         BRAS  RE,SENDD                                                         
*                                                                               
         LA    R1,H02_09Q          SEND END DATE                                
         LA    R4,CAMEND                                                        
         BRAS  RE,SENDD                                                         
*                                                                               
         CLI   CAMSLN,0            DON'T SEND SPOT LENGTH ZERO                  
         BE    SC12                                                             
         LA    R1,H02_0AQ          SEND SPOT LENGTH                             
         LA    R4,CAMSLN                                                        
         BRAS  RE,SENDD                                                         
*                                                                               
SC12     CLI   SVRCVEL+1,H02Q      LIST RECS?                                   
         BE    SCX                  YES - THAT'S ALL PC GETS                    
*                                                                               
         CLC   BUYERNM,SPACES                                                   
         BNH   SC20                                                             
         LA    R5,BUYERNM+(L'BUYERNM-1) END OF BUYER NAME                       
*                                                                               
         CLI   0(R5),C' '          GET LENGTH OF DATA TO SEND                   
         BH    *+8                                                              
         BCT   R5,*-8                                                           
         LA    RF,BUYERNM-1                                                     
         SR    R5,RF                                                            
         LA    R1,H02_0BQ          SEND BUYER NAME                              
         LA    R4,BUYERNM                                                       
         BRAS  RE,SENDD                                                         
*                                                                               
SC20     LA    R6,CAMWKS           SEND NON-CONTINUOUS WEEKS                    
         LA    R3,14                                                            
*                                                                               
SC30     OC    0(4,R6),0(R6)                                                    
         BZ    SC40                                                             
         LA    R1,H02_0CQ          SEND FLIGHT WEEK START                       
         LR    R4,R6                                                            
         BRAS  RE,SENDD                                                         
         LA    R1,H02_0DQ          SEND FLIGHT WEEK END                         
         LA    R4,2(R6)                                                         
         BRAS  RE,SENDD                                                         
         AHI   R6,4                                                             
         BCT   R3,SC30                                                          
*                                                                               
SC40     OC    CAMUPGRD,CAMUPGRD                                                
         BNZ   SC50                                                             
         LA    R1,H02_0EQ          SEND UPGRADE FORMULA REQUIRED                
         LA    R4,=X'01'                                                        
         BRAS  RE,SENDD                                                         
*                                                                               
SC50     CLC   BUYPASS,SPACES                                                   
         BNH   SC60                                                             
         LA    R1,H02_0FQ          SEND PASSWORD REQUIRED                       
         LA    R4,=X'01'                                                        
         BRAS  RE,SENDD                                                         
*                                                                               
SC60     CLI   SVAPROF+7,C'C'      TEST CANADA                                  
         BNE   SC70                                                             
         LA    R1,H02_10Q          SEND CANADIAN                                
         LA    R4,=X'01'           SEND TRUE                                    
         BRAS  RE,SENDD                                                         
*                                                                               
SC70     LA    R1,H02_11Q          SEND BUFFER SIZE                             
         LA    R4,=AL4(DBUFFL-1024)                                             
         BRAS  RE,SENDD                                                         
*                                                                               
         LA    R1,H02_12Q          SEND WEEKLY OR DAILY                         
         LA    R4,=C'W'                                                         
         TM    CAMOPT,CAMODLY                                                   
         BZ    *+8                                                              
         LA    R4,=C'D'                                                         
         BRAS  RE,SENDD                                                         
*                                                                               
SCX      J     EXIT                                                             
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         EJECT                                                                  
*=================================================================*             
* GDEMCAT                                                         *             
*  PASS R2 = A(EST REC)                                           *             
*  EXIT CC NEQ IF NO DEMOS                                        *             
*  RETURN R7 = # DEMO CATEGORIES                                  *             
*=================================================================*             
         SPACE 1                                                                
GDEMCAT  NTR1  BASE=*,LABEL=*                                                   
         L     RE,AIO3             ** INITIALIZE DBLOCK                         
         LR    R6,RE                                                            
         LHI   RF,LENIO                                                         
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         USING DBLOCKD,R6                                                       
         MVC   DBCOMFCS,ACOMFACS                                                
         MVC   DBFILE(3),=C'TP '                                                
         MVI   DBSELMED,C'T'                                                    
         CLI   SVAPROF+7,C'C'      CANADIAN?                                    
         BNE   GDC10                NO                                          
         GOTO1 VALICLT              YES - GET CLT REC                           
         CLI   SVCXTRA,C'U'        TEST US DEMOS                                
         BE    GDC10                                                            
         MVI   DBSELMED,C'C'                                                    
         DROP  R6                                                               
*                                                                               
         USING ESTHDRD,R2                                                       
*                                                                               
GDC10    SR    R7,R7               COUNT NUMBER OF DEMOS                        
         LA    R0,20                                                            
         LA    R1,EDEMOS                                                        
*                                                                               
GDC20    OC    0(3,R1),0(R1)                                                    
         BZ    GDC30                                                            
         LA    R1,3(R1)                                                         
         BCTR  R7,0                DECREMENT COUNT                              
         BCT   R0,GDC20                                                         
*                                                                               
GDC30    XC    DMCB(8),DMCB                                                     
         GOTO1 VCALLOV,DMCB,,X'D9000AE0'    GET A(DEMOCON)                      
         L     RF,0(R1)                                                         
*                                                                               
         LPR   R7,R7               SET NUMBER OF DEMOS                          
         BZ    GDXNEQ                                                           
         GOTO1 (RF),DMCB,((R7),EDEMOS),(6,BLOCK),(C'S',AIO3),EUSRNMS            
*                                                                               
GDXEQ    CR    RE,RE               EXIT CC EQ                                   
         B     *+6                                                              
GDXNEQ   LTR   RE,RE               EXIT CC NEQ                                  
         XIT1  REGS=(R7)                                                        
         DROP  R2                                                               
         LTORG                                                                  
         EJECT                                                                  
*=================================================================*             
* PUT DATA IN TIA TO WSSVR AND PASS CONTROL TO SPOT NWS           *             
*=================================================================*             
         SPACE 1                                                                
WORK_ADD NTR1  BASE=*,LABEL=*                                                   
         XC    WORK,WORK           CALL WSSVR TO SAVE TIA                       
         LA    R1,WORK                                                          
         USING FAWSSVRD,R1                                                      
         MVC   FAWSTOKN,=CL4'$TAM'                                              
         MVI   FAWSACTN,FAWSUSVE                                                
         LHI   R0,DBUFFL           SAVE OFF 16K OF THE TIA                      
         STH   R0,FAWSLEN                                                       
         MVC   FAWSADR,ATIA                                                     
         L     RF,ACOMFACS                                                      
         L     RF,CWSSVR-COMFACSD(RF)                                           
         GOTO1 (RF),(R1)                                                        
         CLI   FAWSRTN,0                                                        
         BE    *+6                                                              
         DCHO                                                                   
         DROP  R1                                                               
*                                                                               
         MVI   SVXFROV,X'01'       RETURN CONTROL TO THIS OVLY                  
         GOTO1 VGLOBBER,DMCB,=C'CLEAR' MAKE SURE NOTHING THERE!                 
*                                                                               
         XC    WORK,WORK                                                        
         USING TAMDATAD,R1                                                      
         LA    R1,WORK                                                          
         MVC   TAMMED,QMED                                                      
         MVC   TAMBYR,BUYERCD                                                   
         MVC   HALF,BCAMP          BCAMP IS X'FF' COMPLEMENT                    
         XC    HALF,=X'FFFF'                                                    
         EDIT  HALF,(5,TAMCAMP),FILL=0,WRK=WORK2                                
         MVC   TAMPASS,BUYPASS                                                  
         MVC   TAMVERS,VERSION                                                  
         DROP  R1                                                               
*                                                                               
         GOTO1 VGLOBBER,DMCB,=C'PUTD',WORK,60,GLVSMSG                           
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DCHO                                                                   
*                                                                               
         XC    WORK,WORK                                                        
         LA    R1,WORK                                                          
         USING GLVXFRSY,R1                                                      
         MVC   GLVXFRSY,=C'SPO'                                                 
         MVC   GLVXFRPR,=C'TAM'                                                 
         MVC   GLVXTOSY,=C'SPO'                                                 
         MVC   GLVXTOPR,=C'NWS'                                                 
         OI    GLVXFLG1,GLV1SEPS                                                
         GOTO1 VGLOBBER,DMCB,=C'PUTD',WORK,24,GLVXCTL                           
         CLI   8(R1),0                                                          
         JE    EXIT                                                             
         DCHO                                                                   
         DROP  R1                                                               
*                                                                               
*                                                                               
* KEEP THIS IN SYNC WITH SAME DSECT IN SPNWS00                                  
*                                                                               
TAMDATAD DSECT                                                                  
TAMMED   DS    CL1                                                              
TAMBYR   DS    CL3                                                              
TAMCAMP  DS    CL5                                                              
TAMPASS  DS    CL10                                                             
TAMVERS  DS    XL4                                                              
TAMDATAL EQU   *-TAMDATAD                                                       
*                                                                               
T22901   CSECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*=================================================================*             
* RETRIEVE DATA FROM WSSVR TO TIA AND PASS BACK TO TAM/PC         *             
*=================================================================*             
         SPACE 1                                                                
RETURN   NTR1  BASE=*,LABEL=*                                                   
         XC    WORK,WORK                                                        
         LA    R1,WORK                                                          
         USING FAWSSVRD,R1                                                      
         MVC   FAWSTOKN,=CL4'$TAM'                                              
         MVI   FAWSACTN,FAWSURST                                                
         LHI   R0,DBUFFL                                                        
         STH   R0,FAWSLEN                                                       
         MVC   FAWSADR,ATIA                                                     
         L     RF,ACOMFACS                                                      
         L     RF,CWSSVR-COMFACSD(RF)                                           
         GOTO1 (RF),(R1)                                                        
         CLI   FAWSRTN,0                                                        
         BE    *+6                                                              
         DCHO                                                                   
         DROP  R1                                                               
*                                                                               
         LHI   R1,H09Q                                                          
         BRAS  RE,SENDH                                                         
*                                                                               
         L     R2,ATIA                                                          
         ST    R2,DBUFFP                                                        
*                                                                               
RTN10    CLI   0(R2),X'FF'                                                      
         BE    RETURNX                                                          
*                                                                               
         LA    R4,2(R2)            SEND KEY                                     
         LA    R1,H09_01Q                                                       
         BRAS  RE,SENDD                                                         
*                                                                               
         OC    4(4,R2),4(R2)       ANY ERROR CODE?                              
         BZ    RTN50                NO                                          
         LA    R4,4(R2)                                                         
         LA    R1,H09_02Q                                                       
         BRAS  RE,SENDD                                                         
*                                                                               
         LA    R4,6(R2)            SEND ERROR FIELD NUMBER                      
         LA    R1,H09_03Q                                                       
         BRAS  RE,SENDD                                                         
*                                                                               
         LA    R1,DMCB                                                          
         XC    DMCB(24),DMCB                                                    
         USING GETTXTD,R1                                                       
         MVC   GTMSGNO,4(R2)                                                    
         CLI   GTMSGNO,X'FF'       IF HOB=X'FF'                                 
         BNE   *+12                                                             
         MVI   GTMSGNO,X'00'       TURN IT OFF                                  
         MVI   GTMSYS,X'FF'        AND SET GENERAL MESSAGE SYSTEM               
         MVI   GTMAXL,L'WORK                                                    
         LA    RF,WORK                                                          
         STCM  RF,7,GTAOUT                                                      
         MVI   GTMTYP,GTMERR                                                    
         OI    GT1INDS,GT1OWRK                                                  
         GOTO1 VGETTXT                                                          
         DROP  R1                                                               
*                                                                               
         ZIC   R5,4(R1)            GET TEXT LENGTH                              
* MAKE SURE WE DONT TRANSMIT NULLS!                                             
         XR    R0,R0               LOB R0 = SEARCH CHARACTER (X'00')            
         LA    RF,WORK                                                          
*                                                                               
RTN20    LA    RE,0(R5,RF)                                                      
         SRST  RE,RF                                                            
         BC    1,*-4               INTERRUPTED!                                 
         BC    2,*+12              NOT FOUND                                    
         MVI   0(RE),C' '          CHANGE NULL TO SPACE                         
         B     RTN20               MAKE SURE NO MORE                            
*                                                                               
         LA    R4,WORK             SEND ERROR TEXT                              
         LA    R1,H09_04Q                                                       
         BRAS  RE,SENDD                                                         
*                                                                               
RTN50    XR    R0,R0                                                            
         ICM   R0,3,0(R2)                                                       
         AR    R2,R0                                                            
         B     RTN10                                                            
*                                                                               
RETURNX  J     EXIT                                                             
*                                                                               
         EJECT                                                                  
*=================================================================*             
* ON ENTRY R1 CONTAINS HEADER CODE                                *             
*=================================================================*             
         SPACE 1                                                                
SENDH    LR    R0,RE                                                            
         GOTO1 GETHDR              GET HEADER ADDRESS                           
         GOTO1 ASETELEM,DMCB,AFABLK,HDRADDR,0,0                                 
         SR    R5,R5               CLEAR LENGTH OVERRIDE                        
         LR    RE,R0                                                            
         BR    RE                                                               
         SPACE 1                                                                
*===============================================================*               
* PARMS ARE FABLK,MAP_TABLE_ENTRY,A(DATA),OVRD_LEN              *               
* ON ENTRY R1 CONTAINS DATA ITEM NUMBER WITHIN CURRENT ELEMENT  *               
*          R4 HAS ADDRESS OF DATA                               *               
*          R5 HAS LENGTH OF DATA (IF DIFFERENT FROM MAP TABLE)  *               
*===============================================================*               
         SPACE 1                                                                
SENDD    LR    R0,RE                                                            
         GOTO1 GETDATA             GET DATA ITEM                                
         GOTO1 AADDDATA,DMCB,AFABLK,DATADDR,(R4),(R5)                           
         SR    R5,R5               CLEAR OVERRIDE LENGTH                        
         LR    RE,R0                                                            
         BR    RE                                                               
         EJECT                                                                  
*                                                                               
       ++INCLUDE SPTAMWRK                                                       
WORKD    DSECT                                                                  
         ORG   OVWORK                                                           
V10000   DS    CL4                                                              
MYKEYSAV DS    CL(L'KEY)                                                        
*                                                                               
         ORG                                                                    
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE FAWSSVRD                                                       
       ++INCLUDE FAGETTXTD                                                      
       ++INCLUDE DDGLOBEQUS                                                     
       ++INCLUDE DDGLVXCTLD                                                     
       ++INCLUDE SPNWSCAM                                                       
       ++INCLUDE DDOFFICED                                                      
       ++INCLUDE FASECRETD                                                      
ESTHDRD  DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'009SPTAM01   11/27/06'                                      
         END                                                                    
