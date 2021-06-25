*          DATA SET ACLFM06    AT LEVEL 025 AS OF 05/01/02                      
*PHASE T60306A,+0                                                               
*INCLUDE CHOPPER                                                                
         TITLE 'T60306 - MODULE TO HANDLE MEDIA ELEMENTS'                       
T60306   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**LFM6**                                                       
         L     RA,0(R1)                                                         
         USING T603FFD,RA                                                       
         L     RC,4(R1)                                                         
         USING LOGWORKD,RC                                                      
         EJECT                                                                  
*              BUILD KEY                                                        
         SPACE 1                                                                
         LA    R2,LOGCODEH                                                      
         CLI   MODE,BUILDKEY                                                    
         BNE   ME02                                                             
         MVC   KEY,SPACES                                                       
         MVI   KEY,X'09'                                                        
         MVC   KEY+1(1),COMPANY                                                 
         GOTO1 ANY                                                              
         MVC   KEY+2(1),LOGCODE                                                 
         TM    LOGCODEH+4,X'20'                                                 
         BO    XIT                                                              
         MVI   ANYKEY,C'Y'                                                      
         OI    LOGCODEH+4,X'20'                                                 
         B     XIT                                                              
         EJECT                                                                  
*              DISPLAY RECORD                                                   
         SPACE 1                                                                
ME02     CLI   MODE,DSPLYREC                                                    
         BNE   ME30                                                             
         TWAXC LOGDESCH                                                         
         MVC   LOGCNAM,SPACES                                                   
         OI    LOGCNAMH+6,X'80'                                                 
         MVC   LOGCSDN,SPACES                                                   
         OI    LOGCSDNH+6,X'80'                                                 
         MVC   LOGPCDN,SPACES                                                   
         OI    LOGPCDNH+6,X'80'                                                 
         MVC   LOGTWON,SPACES                                                   
         OI    LOGTWONH+6,X'80'                                                 
         MVC   LOGTINN,SPACES                                                   
         OI    LOGTINNH+6,X'80'                                                 
         MVC   LOGVNAM,SPACES                                                   
         OI    LOGVNAMH+6,X'80'                                                 
         LA    R4,IO2                                                           
         AH    R4,DATADISP                                                      
         SPACE 1                                                                
ME04     CLI   0(R4),X'11'                                                      
         BE    ME06                                                             
         CLI   0(R4),0                                                          
         BE    XIT                                                              
         SR    R3,R3                                                            
         IC    R3,1(R4)                                                         
         AR    R4,R3                                                            
         B     ME04                                                             
         SPACE 1                                                                
ME06     DS    0H                                                               
         USING ACMEDIAD,R4                                                      
         MVC   LOGDESC,ACMDDESC+3                                               
         MVC   LOGCOMM,ACMDCOMM+1                                               
         MVC   LOGCSD,ACMDCSHD+1                                                
         MVC   LOGANAL,SPACES                                                   
         CLI   ACMDLEN,ACMDLENQ                                                 
         BL    ME08                                                             
         MVC   LOGANAL(1),ACMDANAL                                              
         CLI   ACMDLEN,ACMDLNQ2                                                 
         BL    ME08                                                             
         MVC   LOGANAL,ACMDCOST                                                 
         MVC   LOGPCD,ACMDPRCD+1                                                
         MVC   LOGTWO,ACMDTWO+1                                                 
         CLI   ACMDLEN,ACMDLNQ3                                                 
         BL    ME08                                                             
         MVC   LOGTIN,ACMDTIN+1                                                 
         SPACE 1                                                                
ME08     CLC   COUNTRY,=C'UK'                                                   
         BNE   ME10                                                             
         MVC   LOGVATO,ACMDVTAC+1                                               
         SPACE 1                                                                
ME10     LA    R2,LOGCOMMH                                                      
         MVC   KEY(1),COMPANY                                                   
         MVC   KEY+1(14),LOGCOMM                                                
         GOTO1 READ                                                             
         GOTO1 NAMOUT                                                           
         OI    4(R2),X'20'                                                      
*                                                                               
         CLI   LOGCSD,C' '                                                      
         BNH   ME12                                                             
         LA    R2,LOGCSDH                                                       
         MVC   KEY(1),COMPANY                                                   
         MVC   KEY+1(14),LOGCSD                                                 
         GOTO1 READ                                                             
         GOTO1 NAMOUT                                                           
         OI    4(R2),X'20'                                                      
*                                                                               
ME12     CLI   LOGPCD,C' '                                                      
         BNH   ME14                                                             
         LA    R2,LOGPCDH                                                       
         MVC   KEY(1),COMPANY                                                   
         MVC   KEY+1(14),LOGPCD                                                 
         GOTO1 READ                                                             
         GOTO1 NAMOUT                                                           
         OI    4(R2),X'20'                                                      
*                                                                               
ME14     CLI   LOGTWO,C' '                                                      
         BNH   ME16                                                             
         LA    R2,LOGTWOH                                                       
         MVC   KEY(1),COMPANY                                                   
         MVC   KEY+1(14),LOGTWO                                                 
         GOTO1 READ                                                             
         GOTO1 NAMOUT                                                           
         OI    4(R2),X'20'                                                      
*                                                                               
ME16     CLI   LOGTIN,C' '                                                      
         BNH   ME18                                                             
         LA    R2,LOGTINH                                                       
         MVC   KEY(1),COMPANY                                                   
         MVC   KEY+1(14),LOGTIN                                                 
         GOTO1 READ                                                             
         GOTO1 NAMOUT                                                           
         OI    4(R2),X'20'                                                      
*                                                                               
ME18     CLC   COUNTRY,=C'UK'                                                   
         BNE   ME20                                                             
         LA    R2,LOGVATOH                                                      
         MVC   KEY(1),COMPANY                                                   
         MVC   KEY+1(14),LOGVATO                                                
         GOTO1 READ                                                             
         GOTO1 NAMOUT                                                           
         OI    4(R2),X'20'                                                      
         SPACE 1                                                                
ME20     MVC   LOGNUM,SPACES                                                    
         OI    LOGNUMH+6,X'80'                                                  
         MVC   LOGRSET,SPACES                                                   
         OI    LOGRSETH+6,X'80'                                                 
         CLI   ACMDLEN,X'40'                                                    
         BL    ME22                                                             
         MVC   LOGRSET(4),ACMDRSET                                              
         MVC   LOGNUM(6),ACMDLBIL                                               
         TM    ACMDSTAT,X'80'                                                   
         BZ    ME22                                                             
         MVC   LOGSTAT(8),=C'BILLHEAD'                                          
         OI    LOGSTATH+6,X'80'                                                 
         SPACE 1                                                                
ME22     LA    R2,LOGDESCH                                                      
         B     XIT                                                              
         EJECT                                                                  
*              BUILD (OR AMEND) ELEMENT                                         
         SPACE 1                                                                
ME30     LA    R4,ELEMENT                                                       
         XC    ELEMENT,ELEMENT                                                  
         USING ACMEDIAD,R4                                                      
         MVI   ACMDEL,X'11'                                                     
         MVI   ACMDLEN,ACMDLNQ3                                                 
         MVC   ACMDCODE,LOGCODE                                                 
*                                                                               
         LA    R2,LOGCOMMH                                                      
         GOTO1 ANY                                                              
         GOTO1 MOVE                                                             
         CLC   WORK(2),=C'SI'                                                   
         BNE   ME48                                                             
         MVC   KEY(1),COMPANY                                                   
         MVC   KEY+1(14),WORK                                                   
         MVC   ACMDCOMM,KEY                                                     
         TM    4(R2),X'20'                                                      
         BO    ME32                                                             
         MVC   LOGCNAM,SPACES                                                   
         OI    LOGCNAMH+6,X'80'                                                 
         GOTO1 READ                                                             
         GOTO1 NAMOUT                                                           
         BAS   RE,BALEL                                                         
         OI    4(R2),X'20'                                                      
*                                                                               
ME32     LA    R2,LOGCSDH                                                       
         CLI   5(R2),0                                                          
         BE    ME34                                                             
         GOTO1 MOVE                                                             
         CLC   WORK(2),=C'SI'                                                   
         BNE   ME48                                                             
         MVC   KEY(1),COMPANY                                                   
         MVC   KEY+1(14),WORK                                                   
         MVC   ACMDCSHD,KEY                                                     
         TM    4(R2),X'20'                                                      
         BO    ME34                                                             
         MVC   LOGCSDN,SPACES                                                   
         OI    LOGCSDNH+6,X'80'                                                 
         GOTO1 READ                                                             
         GOTO1 NAMOUT                                                           
         BAS   RE,BALEL                                                         
         OI    4(R2),X'20'                                                      
*                                                                               
ME34     LA    R2,LOGPCDH                                                       
         CLI   5(R2),0                                                          
         BE    ME36                                                             
         GOTO1 MOVE                                                             
         CLC   WORK(2),=C'SI'                                                   
         BNE   ME48                                                             
         MVC   KEY(1),COMPANY                                                   
         MVC   KEY+1(14),WORK                                                   
         MVC   ACMDPRCD,KEY                                                     
         TM    4(R2),X'20'                                                      
         BO    ME36                                                             
         MVC   LOGPCDN,SPACES                                                   
         OI    LOGPCDNH+6,X'80'                                                 
         GOTO1 READ                                                             
         GOTO1 NAMOUT                                                           
         BAS   RE,BALEL                                                         
         OI    4(R2),X'20'                                                      
*                                                                               
ME36     LA    R2,LOGTWOH                                                       
         CLI   5(R2),0                                                          
         BE    ME38                                                             
         GOTO1 MOVE                                                             
         CLC   WORK(2),=C'SI'                                                   
         BNE   ME48                                                             
         MVC   KEY(1),COMPANY                                                   
         MVC   KEY+1(14),WORK                                                   
         MVC   ACMDTWO,KEY                                                      
         TM    4(R2),X'20'                                                      
         BO    ME38                                                             
         MVC   LOGTWON,SPACES                                                   
         OI    LOGTWONH+6,X'80'                                                 
         GOTO1 READ                                                             
         GOTO1 NAMOUT                                                           
         BAS   RE,BALEL                                                         
         OI    4(R2),X'20'                                                      
*                                                                               
ME38     LA    R2,LOGTINH                                                       
         CLI   5(R2),0                                                          
         BE    ME40                                                             
         GOTO1 MOVE                                                             
         CLC   WORK(2),=C'SK'                                                   
         BE    ME39                                                             
         CLC   WORK(2),=C'SI'                                                   
         BNE   ME48                                                             
*                                                                               
ME39     MVC   KEY(1),COMPANY                                                   
         MVC   KEY+1(14),WORK                                                   
         MVC   ACMDTIN,KEY                                                      
         TM    4(R2),X'20'                                                      
         BO    ME40                                                             
         MVC   LOGTINN,SPACES                                                   
         OI    LOGTINNH+6,X'80'                                                 
         GOTO1 READ                                                             
         GOTO1 NAMOUT                                                           
         BAS   RE,BALEL                                                         
         OI    4(R2),X'20'                                                      
*                                                                               
         SPACE 1                                                                
ME40     MVC   ACMDDESC,SPACES                                                  
         LA    R2,LOGDESCH                                                      
         CLI   5(R2),0                                                          
         BE    ME42                                                             
         GOTO1 MOVE                                                             
         MVC   ACMDDESC+3(12),WORK                                              
         B     ME44                                                             
         SPACE 1                                                                
ME42     LA    R2,ACMDDESC+3                                                    
         GOTO1 =V(CHOPPER),DMCB,(36,LOGCNAM),(12,(R2)),(0,1),RR=RB              
         MVC   LOGDESC,ACMDDESC+3                                               
         OI    LOGDESCH+6,X'80'                                                 
         SPACE 1                                                                
ME44     MVC   ACMDANAL,SPACES                                                  
         OC    LOGANAL,SPACES                                                   
         LA    R2,LOGANALH                                                      
         CLI   5(R2),0                                                          
         BE    ME46                                                             
         MVC   KEY+1(14),SPACES                                                 
         MVC   KEY+1(2),=C'11'                                                  
         MVC   KEY+3(12),LOGANAL                                                
         GOTO1 READ                                                             
         BAS   RE,BALEL                                                         
         MVC   KEY+1(2),=C'12'                                                  
         GOTO1 READ                                                             
         BAS   RE,BALEL                                                         
         MVC   ACMDANAL,LOGANAL                                                 
         MVC   ACMDCOST,LOGANAL                                                 
         SPACE 1                                                                
ME46     LA    R2,LOGNUMH                                                       
         CLI   5(R2),6             BILL NUMBERS MUST BE 6 CHARACTERS            
         BNE   ME48                                                             
         GOTO1 NUMERIC                                                          
         MVC   ACMDFBIL,LOGNUM                                                  
         MVC   ACMDLBIL,LOGNUM                                                  
         LA    R2,LOGRSETH                                                      
         GOTO1 NUMERIC                                                          
         CLI   5(R2),4             RESET NUMBERS MUST BE 4 CHARACTERS           
         BNE   ME48                                                             
         MVC   ACMDRSET,LOGRSET                                                 
         B     ME50                                                             
         SPACE 1                                                                
ME48     MVI   ERROR,INVALID                                                    
         B     XIT                                                              
         SPACE 1                                                                
ME50     LA    R2,LOGVATOH                                                      
         CLC   COUNTRY,=C'UK'                                                   
         BNE   ME52                                                             
         GOTO1 ANY                                                              
         GOTO1 MOVE                                                             
         MVC   KEY(1),COMPANY                                                   
         MVC   KEY+1(14),WORK                                                   
         MVC   ACMDVTAC,KEY                                                     
         TM    4(R2),X'20'                                                      
         BO    ME52                                                             
         MVC   LOGVNAM,SPACES                                                   
         OI    LOGVNAMH+6,X'80'                                                 
         GOTO1 READ                                                             
         GOTO1 NAMOUT                                                           
         BAS   RE,BALEL                                                         
         OI    4(R2),X'20'                                                      
         SPACE 1                                                                
ME52     CLC   LOGSTAT(8),=C'BILLHEAD'                                          
         BNE   ME54                                                             
         OI    ACMDSTAT,X'80'                                                   
         SPACE 1                                                                
ME54     GOTO1 REMANEL,DMCB,(X'11',0)                                           
         GOTO1 ADDANEL                                                          
         B     XIT                                                              
         EJECT                                                                  
*              FIND BALANCE ELEMENT                                             
*                                                                               
         USING ACKEYD,R3                                                        
BALEL    LA    R3,IO                                                            
         LA    R3,ACRECORD                                                      
         SR    R1,R1                                                            
*                                                                               
BAL2     CLI   0(R3),X'32'                                                      
         BER   RE                  FOUND 32 - OK TO RETURN                      
         CLI   0(R3),0                                                          
         BE    BAL3                NO 32 IS AN ERROR                            
         IC    R1,1(R3)                                                         
         AR    R3,R1                                                            
         B     BAL2                                                             
*                                                                               
BAL3     MVI   ERROR,18            SET - INVALID FOR POSTING- AND EXIT          
*                                                                               
         SPACE 1                                                                
XIT      XIT1  REGS=(R2)                                                        
         EJECT                                                                  
       ++INCLUDE ACLFMFFD                                                       
         ORG   LOGTABH                                                          
         EJECT                                                                  
       ++INCLUDE ACLFMF9D                                                       
         PRINT OFF                                                              
         EJECT                                                                  
       ++INCLUDE ACLFMWORK                                                      
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
       ++INCLUDE ACGENFILE                                                      
       ++INCLUDE ACLFMEQU                                                       
       ++INCLUDE DDFLDIND                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'025ACLFM06   05/01/02'                                      
         END                                                                    
